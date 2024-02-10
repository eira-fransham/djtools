use acc_reader::AccReader;
use binrw::{binrw, BinReaderExt, Endian};
use chrono::{NaiveDateTime, Utc};
use dashmap::{mapref::entry::Entry, DashMap};
use parking_lot::{Mutex, MutexGuard};
use std::{
    borrow::Cow,
    collections::{btree_map, hash_map::RandomState, BTreeMap},
    hash::BuildHasher,
    io::{self, ErrorKind, Read, Result, Write},
    iter::Map,
    marker::PhantomData,
    net::{SocketAddr, ToSocketAddrs, UdpSocket},
};

pub const DEFAULT_ANNOUNCE_PORT: usize = 50000;

#[derive(Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
struct DeviceId(u64);

struct Device<Addr> {
    last_seen: NaiveDateTime,
    num_packets: u64,
    address: Addr,
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

impl<S: Socket, MapState: BuildHasher + Clone + Default> Finder<S, MapState> {
    pub fn new(socket: S) -> Self {
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
        let recv = self.socket.receiver_from()?;
        let addr = recv.source().clone();

        let mut by_last_seen = self.by_last_seen.lock();

        // TODO: We shouldn't just crash when receiving an invalid packet
        let linkpacket: DeviceLink = AccReader::new(recv)
            .read_type(Endian::Big)
            .map_err(|e| io::Error::new(ErrorKind::Other, e))?;
        let now = Utc::now().naive_utc();

        let id = DeviceId(linkpacket.device_id as _);

        match self.devices.entry(id) {
            Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                entry.num_packets += 1;
                let last_seen = entry.last_seen;
                entry.last_seen = now;
                if let Some(ids) = by_last_seen.get_mut(&last_seen) {
                    if let Ok(i) = ids.binary_search(&id) {
                        ids.remove(i);
                    }
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(Device {
                    last_seen: now,
                    num_packets: 1,
                    address: addr,
                });
            }
        }

        match by_last_seen.entry(now) {
            btree_map::Entry::Occupied(mut entry) => {
                entry.get_mut().push(id);
            }
            btree_map::Entry::Vacant(entry) => {
                entry.insert(vec![id]);
            }
        }

        if (now - self.last_purge).num_milliseconds() >= PURGE_FREQUENCY_MS as i64 {
            self.purge(&mut by_last_seen);
        }

        Ok(())
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

#[binrw]
struct DeviceLink {
    #[bw(calc([0; 35]))]
    _unknown0: [u8; 35],
    device_id: u8,
    #[bw(calc([0; 54 - 1 - 35]))]
    _unknown1: [u8; 54 - 1 - 35],
}

#[cfg(test)]
mod test {
    use std::{
        io::{self, Read, Write},
        net::UdpSocket,
    };

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
}
