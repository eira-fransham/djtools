use acc_reader::AccReader;
use arrayvec::ArrayVec;
use binrw::{binrw, BinRead, BinReaderExt, BinResult, BinWrite, Endian};
use chrono::{NaiveDateTime, Utc};
use dashmap::{
    mapref::{entry::Entry, multiple::RefMulti},
    DashMap,
};
use parking_lot::{Mutex, MutexGuard};
use std::{
    borrow::Cow,
    collections::{btree_map, hash_map::RandomState, BTreeMap},
    ffi::CStr,
    hash::BuildHasher,
    io::{self, ErrorKind, Read, Result, Write},
    iter::Map,
    marker::PhantomData,
    net::{SocketAddr, ToSocketAddrs, UdpSocket},
    str,
};

pub const DEFAULT_ANNOUNCE_PORT: u16 = 50000;

#[derive(Debug, Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct DeviceId(pub u64);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Device<Addr> {
    pub last_seen: NaiveDateTime,
    pub num_packets: u64,
    pub address: Addr,
}

pub trait GetSource {
    type Addr: Send + Sync + 'static;

    fn source(&self) -> &Self::Addr;
}

pub trait Socket {
    type Addr: Clone + Send + Sync + 'static;
    type Reader<'a>: Read + GetSource<Addr = Self::Addr>
    where
        Self: 'a;

    fn receiver_from(&self) -> Result<Self::Reader<'_>>;
    fn set_nonblocking(&self, _: bool) -> Result<()>;
}

pub trait NetworkSendSocket: Socket {
    type Writer<'a, Addrs: 'a>: Write
    where
        Addrs: ToSocketAddrs,
        Self: 'a;

    fn sender_to<'a, I: 'a>(&'a self, addrs: I) -> Self::Writer<'a, I>
    where
        I: ToSocketAddrs;
}

pub trait GenericSendSocket: Socket {
    type Writer<'a, Addrs: 'a>: Write
    where
        Addrs: Clone + IntoIterator,
        <Addrs as IntoIterator>::Item: Into<Cow<'a, Self::Addr>>,
        Self: 'a;

    fn sender_to<'a, I: 'a>(&'a self, addrs: I) -> Self::Writer<'a, I>
    where
        I: Clone + IntoIterator,
        <I as IntoIterator>::Item: Into<Cow<'a, Self::Addr>>;
}

enum UdpReaderState {
    NeedsCheck,
    Checked,
}

pub struct UdpReader<'a> {
    inner: &'a UdpSocket,
    addr: SocketAddr,
    state: UdpReaderState,
}

impl<'a> UdpReader<'a> {
    pub fn new(inner: &'a UdpSocket) -> Result<Self> {
        let mut fake_buf: [u8; 1] = [0];
        let (_, src) = inner.peek_from(&mut fake_buf)?;
        Ok(Self {
            inner: inner,
            addr: src,
            state: UdpReaderState::Checked,
        })
    }
}

impl GetSource for UdpReader<'_> {
    type Addr = SocketAddr;

    fn source(&self) -> &Self::Addr {
        &self.addr
    }
}

impl Read for UdpReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        match self.state {
            UdpReaderState::NeedsCheck => {
                let mut fake_buf: [u8; 1] = [0];
                let (_, next_src) = match self.inner.peek_from(&mut fake_buf) {
                    Ok(val) => val,
                    Err(e) if e.kind() == ErrorKind::WouldBlock => return Ok(0),
                    Err(e) => return Err(e),
                };
                if next_src != self.addr {
                    return Ok(0);
                }
            }
            UdpReaderState::Checked => {}
        }

        let (count, src) = self.inner.recv_from(buf)?;
        self.state = UdpReaderState::NeedsCheck;
        if src != self.addr {
            Ok(0)
        } else {
            Ok(count)
        }
    }
}

pub struct GenericMarker;
pub struct NetworkMarker;

pub struct UdpWriter<'a, Addrs, Marker = GenericMarker> {
    inner: &'a UdpSocket,
    addrs: Addrs,
    _phantom: PhantomData<Marker>,
}

impl<'a, Addrs> UdpWriter<'a, Addrs> {
    pub fn new(inner: &'a UdpSocket, addrs: Addrs) -> Self {
        Self {
            inner,
            addrs,
            _phantom: PhantomData,
        }
    }
}

impl<'a, Addrs> UdpWriter<'a, Addrs, NetworkMarker> {
    pub fn new_network(inner: &'a UdpSocket, addrs: Addrs) -> Self {
        Self {
            inner,
            addrs,
            _phantom: PhantomData,
        }
    }
}

struct ToSocketAddrsAdapter<'a, Inner: 'a>(Inner, PhantomData<&'a Inner>);

impl<'a, Inner> ToSocketAddrs for ToSocketAddrsAdapter<'a, Inner>
where
    Inner: Clone + IntoIterator,
    <Inner as IntoIterator>::Item: Into<Cow<'a, SocketAddr>>,
{
    type Iter = Map<Inner::IntoIter, fn(<Inner as IntoIterator>::Item) -> SocketAddr>;

    fn to_socket_addrs(&self) -> Result<Self::Iter> {
        Ok(self
            .0
            .clone()
            .into_iter()
            .map(|val| val.into().into_owned()))
    }
}

impl<'a, Addrs: 'a> Write for UdpWriter<'a, Addrs>
where
    Addrs: Clone + IntoIterator,
    <Addrs as IntoIterator>::Item: Into<Cow<'a, SocketAddr>>,
{
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let out = self
            .inner
            .send_to(buf, ToSocketAddrsAdapter(self.addrs.clone(), PhantomData))?;

        Ok(out)
    }

    fn flush(&mut self) -> Result<()> {
        // UDP sockets always send immediately
        Ok(())
    }
}

impl<'a, Addrs: 'a> Write for UdpWriter<'a, Addrs, NetworkMarker>
where
    Addrs: ToSocketAddrs,
{
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let out = self.inner.send_to(buf, &self.addrs)?;

        Ok(out)
    }

    fn flush(&mut self) -> Result<()> {
        // UDP sockets always send immediately
        Ok(())
    }
}

impl Socket for UdpSocket {
    type Addr = SocketAddr;
    type Reader<'a> = UdpReader<'a>;

    fn receiver_from(&self) -> Result<Self::Reader<'_>> {
        Self::Reader::new(self)
    }

    fn set_nonblocking(&self, nonblocking: bool) -> Result<()> {
        self.set_nonblocking(nonblocking)
    }
}

impl GenericSendSocket for UdpSocket {
    type Writer<'a, Addrs: 'a> = UdpWriter<'a, Addrs>
    where
        Addrs: Clone + IntoIterator,
        <Addrs as IntoIterator>::Item: Into<Cow<'a, Self::Addr>>,
        Self: 'a
        ;

    fn sender_to<'a, I: 'a>(&'a self, addrs: I) -> Self::Writer<'a, I>
    where
        I: Clone + IntoIterator,
        <I as IntoIterator>::Item: Into<Cow<'a, Self::Addr>>,
    {
        UdpWriter::new(self, addrs)
    }
}

impl NetworkSendSocket for UdpSocket {
    type Writer<'a, Addrs: 'a> = UdpWriter<'a, Addrs, NetworkMarker> where Addrs: ToSocketAddrs, Self: 'a;

    fn sender_to<'a, I: 'a>(&'a self, addrs: I) -> Self::Writer<'a, I>
    where
        I: ToSocketAddrs,
    {
        UdpWriter::new_network(self, addrs)
    }
}

type LastSeen = BTreeMap<NaiveDateTime, Vec<DeviceId>>;

pub struct Finder<S: Socket, MapState = RandomState> {
    socket: S,
    devices: DashMap<DeviceId, Device<<S as Socket>::Addr>, MapState>,
    by_last_seen: Mutex<LastSeen>,
    last_purge: NaiveDateTime,
}

impl Finder<UdpSocket> {
    pub fn connect() -> io::Result<Self> {
        let sock = UdpSocket::bind(("127.0.0.1", DEFAULT_ANNOUNCE_PORT))?;
        sock.set_nonblocking(true)?;
        Ok(Self::from_socket(sock))
    }
}

impl<S: Socket, MapState: BuildHasher + Clone + Default> Finder<S, MapState> {
    pub fn from_socket(socket: S) -> Self {
        Self {
            socket,
            devices: Default::default(),
            by_last_seen: Default::default(),
            last_purge: NaiveDateTime::UNIX_EPOCH,
        }
    }
}

const PURGE_FREQUENCY_MS: usize = 1000;
const PURGE_MAX_AGE_MS: usize = 5000;

impl<S: Socket, MapState: BuildHasher + Clone> Finder<S, MapState> {
    pub fn update_known_devices(&self) -> Result<()> {
        dbg!();
        let recv = match self.socket.receiver_from() {
            Ok(recv) => recv,
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => return Ok(()),
            Err(other) => return Err(other),
        };
        dbg!();
        let addr = recv.source().clone();
        dbg!();

        dbg!();
        let mut by_last_seen = self.by_last_seen.lock();
        dbg!();

        // TODO: We shouldn't just crash when receiving an invalid packet
        let linkpacket: DeviceLink = AccReader::new(recv)
            .read_type(Endian::Big)
            .map_err(|e| io::Error::new(ErrorKind::Other, e))?;
        let now = Utc::now().naive_utc();

        dbg!(&linkpacket);

        let id = DeviceId(linkpacket.device_id as _);

        dbg!();
        match self.devices.entry(id) {
            Entry::Occupied(mut entry) => {
                dbg!();
                let entry = entry.get_mut();
                entry.num_packets += 1;
                let last_seen = entry.last_seen;
                entry.last_seen = now;
                if let Some(ids) = by_last_seen.get_mut(&last_seen) {
                    if let Ok(i) = ids.binary_search(&id) {
                        ids.remove(i);
                    }
                }
                dbg!();
            }
            Entry::Vacant(entry) => {
                dbg!();
                entry.insert(Device {
                    last_seen: now,
                    num_packets: 1,
                    address: addr,
                });
                dbg!();
            }
        }

        dbg!();
        match by_last_seen.entry(now) {
            btree_map::Entry::Occupied(mut entry) => {
                entry.get_mut().push(id);
            }
            btree_map::Entry::Vacant(entry) => {
                entry.insert(vec![id]);
            }
        }

        dbg!();
        if (now - self.last_purge).num_milliseconds() >= PURGE_FREQUENCY_MS as i64 {
            dbg!();
            self.purge(&mut by_last_seen);
            dbg!();
        }

        Ok(())
    }

    pub fn known_devices(
        &self,
    ) -> impl Iterator<Item = RefMulti<'_, DeviceId, Device<<S as Socket>::Addr>, MapState>> + '_
    {
        self.devices.iter()
    }

    fn purge(&self, by_last_seen: &mut MutexGuard<'_, LastSeen>) {
        let now = Utc::now().naive_utc();

        while let Some(entry) = by_last_seen.last_entry() {
            if (now - *entry.key()).num_milliseconds() > PURGE_MAX_AGE_MS as i64 {
                entry.remove();
            } else {
                break;
            }
        }
    }
}

const MAX_DEVICE_NAME_LEN: usize = 20;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DevName(pub ArrayVec<u8, MAX_DEVICE_NAME_LEN>);

impl DevName {
    pub fn new(name: &str) -> Self {
        let mut out = ArrayVec::<_, MAX_DEVICE_NAME_LEN>::new();
        out.try_extend_from_slice(&name.as_bytes()[..MAX_DEVICE_NAME_LEN.min(name.len())])
            .expect("This should never fail!");
        let _ = out.try_push(0);
        Self(out)
    }

    pub fn try_to_str(&self) -> Option<&str> {
        if self.0.is_full() {
            str::from_utf8(&self.0[..]).ok()
        } else {
            let cstr = CStr::from_bytes_until_nul(&self.0).ok()?;

            let s = cstr.to_str().ok()?;

            Some(s)
        }
    }
}

impl From<&str> for DevName {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl BinWrite for DevName {
    type Args<'a> = ();

    fn write_options<W: Write + io::prelude::Seek>(
        &self,
        writer: &mut W,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<()> {
        let zeroes = [0; MAX_DEVICE_NAME_LEN];
        let mut name = self.0.clone();
        name.try_extend_from_slice(&zeroes[name.len()..])
            .expect("This should never fail!");

        name.write_options(writer, endian, args)
    }
}

impl BinRead for DevName {
    type Args<'a> = ();

    fn read_options<R: Read + io::prelude::Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let arr = <[u8; MAX_DEVICE_NAME_LEN]>::read_options(reader, endian, args)?;

        let mut out = ArrayVec::from(arr);

        if let Some(first_zero) = out.iter().position(|b| *b == 0) {
            out.truncate((first_zero + 1).min(out.capacity()));
        }

        Ok(DevName(out))
    }
}

#[binrw]
#[brw(big)]
#[brw(magic = b"Qspt1WmJOL")]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Header {
    header_type: u8,
    #[brw(magic(0x00u8))]
    #[bw(calc = ())]
    _zero: (),
    name: DevName,
    #[brw(magic(0x01020036u32))]
    #[bw(calc = ())]
    _unknown: (),
}

#[binrw]
#[brw(big)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct DeviceLink {
    header: Header,
    device_id: u8,
    #[brw(magic(0x01u8))]
    #[bw(calc = ())]
    _unknown0: (),
    mac_addr: [u8; 6],
    ip_addr: [u8; 6],
    #[brw(magic(0x0200u16))]
    #[bw(calc = ())]
    _unknown1: (),
    #[brw(magic(0x00000164u32))]
    #[bw(calc = ())]
    _unknown2: (),
}

#[cfg(test)]
mod test {
    use std::{
        io::{self, Read, Seek as _, Write},
        net::UdpSocket,
    };

    use binrw::{BinRead as _, BinWrite as _};

    use crate::finder::{DeviceLink, Header};

    use super::{NetworkSendSocket, Socket};

    #[test]
    fn check_api() -> io::Result<()> {
        let socket_a = UdpSocket::bind("127.0.0.1:6777")?;
        let socket_b = UdpSocket::bind("127.0.0.1:6778")?;

        let mut buf = [0u8; 8];

        for i in 0..buf.len() {
            buf[i] = i as u8;
        }

        let mut sender = socket_b.sender_to(socket_a.local_addr()?);
        sender.write_all(&buf)?;
        sender.flush()?;

        let mut receiver = socket_a.receiver_from()?;
        let mut new_buf = [0; 8];
        receiver.read(&mut new_buf)?;

        assert_eq!(&buf[..], &new_buf[..]);

        for i in &mut buf {
            *i = *i + 1;
        }

        let mut sender = socket_a.sender_to(socket_b.local_addr()?);
        sender.write_all(&buf)?;
        sender.flush()?;

        Ok(())
    }

    #[test]
    fn dysentery_parity() {
        const ANNOUNCE_MESSAGE: &[u8] = &[
            81, 115, 112, 116, 49, 87, 109, 74, 79, 76, 6, 0, 72, 101, 108, 108, 111, 44, 32, 119,
            111, 114, 108, 100, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 54, 1, 1, 1, 0, 3, 0, 5, 0, 0, 5,
            0, 3, 0, 1, 2, 0, 0, 0, 1, 100,
        ];

        let message = DeviceLink {
            header: Header {
                header_type: 0x06,
                name: "Hello, world".into(),
            },
            device_id: 1,
            mac_addr: [0x01, 0x00, 0x03, 0x00, 0x05, 0x00],
            ip_addr: [0x00, 0x05, 0x00, 0x03, 0x00, 0x01],
        };

        let mut buf = io::Cursor::new(vec![0u8; 32]);

        message.write(&mut buf).unwrap();

        assert_eq!(buf.get_ref(), ANNOUNCE_MESSAGE);
    }

    #[test]
    fn round_trip() {
        let message = DeviceLink {
            header: Header {
                header_type: 0x06,
                name: "Hello, world".into(),
            },
            device_id: 1,
            mac_addr: [0x01, 0x00, 0x03, 0x00, 0x05, 0x00],
            ip_addr: [0x00, 0x05, 0x00, 0x03, 0x00, 0x01],
        };

        let mut buf = io::Cursor::new(vec![0u8; 32]);

        message.write(&mut buf).unwrap();

        buf.seek(io::SeekFrom::Start(0)).unwrap();

        assert_eq!(message, DeviceLink::read(&mut buf).unwrap());
    }
}
