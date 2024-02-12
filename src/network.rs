use std::{
    borrow::Cow,
    ffi::CStr,
    io::{self, ErrorKind, Read, Result, Write},
    iter::Map,
    marker::PhantomData,
    net::{SocketAddr, ToSocketAddrs, UdpSocket},
    str,
};

use arrayvec::ArrayVec;
use binrw::{binrw, BinRead, BinResult, BinWrite, Endian};

pub trait GetSource {
    type Addr: Send + Sync + 'static;

    fn source(&self) -> &Self::Addr;
}

pub trait Socket {
    type Addr: Clone + Send + Sync + 'static;
    type Reader<'a>: Read + GetSource<Addr = Self::Addr>
    where
        Self: 'a;

    fn mac_addr(&self) -> [u8; 6];
    fn ip_addr(&self) -> (u32, u16);

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

    fn ip_addr(&self) -> (u32, u16) {
        match self.local_addr().unwrap() {
            SocketAddr::V4(v4) => (v4.ip().to_bits(), v4.port()),
            SocketAddr::V6(_) => todo!(),
        }
    }

    fn mac_addr(&self) -> [u8; 6] {
        mac_address::get_mac_address().unwrap().unwrap().bytes()
    }

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

    fn write_options<W: Write + io::Seek>(
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

    fn read_options<R: Read + io::Seek>(
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
// TODO: This is from `header_bytes` in Dysentery, we should find out what this actually means
#[brw(magic = b"Qspt1WmJOL")]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Header {
    pub header_type: u8,
    // TODO: Unclear from Dysentery what this means
    #[brw(magic(0x00u8))]
    #[bw(calc = ())]
    _zero: (),
    pub name: DevName,
    // TODO: Unclear from Dysentery what this means
    #[brw(magic(0x01020036u32))]
    #[bw(calc = ())]
    _unknown: (),
}

#[binrw]
#[brw(big)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DeviceLink {
    pub header: Header,
    pub device_id: u8,
    #[brw(magic(0x01u8))]
    #[bw(calc = ())]
    _unknown0: (),
    pub mac_addr: [u8; 6],
    pub ip_addr: u32,
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
        io::{self, Read as _, Write as _},
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
