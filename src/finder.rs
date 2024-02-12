use acc_reader::AccReader;
use arrayvec::ArrayVec;
use binrw::{binrw, io::BufReader, BinRead, BinReaderExt, BinResult, BinWrite, Endian};
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
    io::{self, ErrorKind, Read, Result, Seek as _, Write},
    iter::Map,
    marker::PhantomData,
    net::{Ipv4Addr, SocketAddr, ToSocketAddrs, UdpSocket},
    str,
};

use crate::network::{DeviceLink, GetSource as _, NetworkSendSocket, Socket};

pub const DEFAULT_ANNOUNCE_PORT: u16 = 50000;

#[derive(Debug, Copy, Clone, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct DeviceId(pub u64);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Device<Addr> {
    pub last_seen: NaiveDateTime,
    pub num_packets: u64,
    pub address: Addr,
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
            // TODO: Make this properly configurable
            devices: Default::default(),
            by_last_seen: Default::default(),
            last_purge: NaiveDateTime::UNIX_EPOCH,
        }
    }
}

const PURGE_FREQUENCY_MS: usize = 1000;
const PURGE_MAX_AGE_MS: usize = 5000;

impl<S: Socket, MapState: BuildHasher + Clone> Finder<S, MapState> {
    pub fn socket(&self) -> &S {
        &self.socket
    }

    pub fn update_known_devices(&self) -> Result<()> {
        let now = Utc::now().naive_utc();

        let mut by_last_seen = self.by_last_seen.lock();

        loop {
            let recv = match self.socket.receiver_from() {
                Ok(recv) => recv,
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                Err(other) => return Err(other),
            };
            let addr = recv.source().clone();

            let recv = BufReader::new(recv);

            // TODO: We shouldn't just crash when receiving an invalid packet
            let linkpacket: DeviceLink = match AccReader::new(recv)
                .read_type(Endian::Big)
                .map_err(|e| io::Error::new(ErrorKind::Other, e))
            {
                Ok(packet) => packet,
                Err(_) => continue,
            };

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
        }

        if (now - self.last_purge).num_milliseconds() >= PURGE_FREQUENCY_MS as i64 {
            self.purge(&mut by_last_seen);
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

#[cfg(test)]
mod test {
    use std::io::{self, Seek as _};

    use binrw::{BinRead as _, BinWrite as _};

    use crate::network::{DeviceLink, Header};

    #[test]
    fn dysentery_parity() {
        const ANNOUNCE_MESSAGE: &[u8] = &[
            81, 115, 112, 116, 49, 87, 109, 74, 79, 76, 6, 0, 72, 101, 108, 108, 111, 44, 32, 119,
            111, 114, 108, 100, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 54, 1, 1, 1, 0, 3, 0, 5, 0, 0, 5,
            0, 3, 2, 0, 0, 0, 1, 100,
        ];

        let message = DeviceLink {
            header: Header {
                header_type: 0x06,
                name: "Hello, world".into(),
            },
            device_id: 1,
            mac_addr: [0x01, 0x00, 0x03, 0x00, 0x05, 0x00],
            ip_addr: 0x00050003,
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
            ip_addr: 0x00050003,
        };

        let mut buf = io::Cursor::new(vec![0u8; 32]);

        message.write(&mut buf).unwrap();

        buf.seek(io::SeekFrom::Start(0)).unwrap();

        assert_eq!(message, DeviceLink::read(&mut buf).unwrap());
    }
}
