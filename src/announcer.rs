use std::{
    borrow::Cow,
    io::{self, Seek as _, Write as _},
    net::Ipv4Addr,
};

use binrw::BinWrite as _;

use crate::{
    finder::DEFAULT_ANNOUNCE_PORT,
    network::{DeviceLink, Header, NetworkSendSocket, Socket},
};

pub struct AnnouncerBuilder {
    device_id: Option<u8>,
    name: Option<Cow<'static, str>>,
}

impl AnnouncerBuilder {
    pub fn new() -> Self {
        Self {
            device_id: None,
            name: None,
        }
    }

    pub fn device_id(&mut self, id: u8) -> &mut Self {
        self.device_id = Some(id);

        self
    }

    pub fn name<Str>(&mut self, name: Str) -> &mut Self
    where
        Str: Into<Cow<'static, str>>,
    {
        self.name = Some(name.into());

        self
    }
}

impl AnnouncerBuilder {
    pub fn connect<S: Socket>(&mut self, socket: S) -> Announcer<S> {
        let mac_addr = socket.mac_addr();

        Announcer {
            device_id: self.device_id.take().unwrap_or(6),
            name: self.name.take().unwrap_or("Virtual CDJ".into()),
            mac_addr,
            socket,
        }
    }
}

pub struct Announcer<S> {
    device_id: u8,
    mac_addr: [u8; 6],
    name: Cow<'static, str>,
    socket: S,
}

impl<S> Announcer<S>
where
    S: Socket,
{
    pub fn from_socket(sock: S) -> Self {
        AnnouncerBuilder::new().connect(sock)
    }
}

impl<S: NetworkSendSocket> Announcer<S> {
    pub fn send_keepalive(&self) -> io::Result<()> {
        let (ip_addr, _) = self.socket.ip_addr();

        let message = DeviceLink {
            header: Header {
                header_type: 0x06,
                name: self.name.as_ref().into(),
            },
            device_id: self.device_id,
            mac_addr: self.mac_addr,
            ip_addr,
        };

        let mut out_bytes = io::Cursor::new(vec![0u8; 64]);

        message
            .write(&mut out_bytes)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

        let mut sender = self
            .socket
            .sender_to((Ipv4Addr::BROADCAST, DEFAULT_ANNOUNCE_PORT));
        let size = out_bytes.seek(io::SeekFrom::Current(0))?;
        let mut out_bytes = out_bytes.into_inner();
        out_bytes.truncate(size as usize);
        sender.write_all(&out_bytes)?;

        Ok(())
    }
}
