use std::{net::UdpSocket, thread, time::Duration};

use djtools::{
    announcer,
    finder::{self, DEFAULT_ANNOUNCE_PORT},
};

fn main() {
    let announcer = {
        let sock = UdpSocket::bind("192.168.0.142:49950").unwrap();
        sock.set_nonblocking(true).unwrap();
        sock.set_broadcast(true).unwrap();
        announcer::AnnouncerBuilder::new()
            .name("Eira's Special CDJ")
            .connect(sock)
    };

    thread::scope(|scope| {
        scope.spawn(|| loop {
            announcer.send_keepalive().unwrap();
            thread::sleep(Duration::from_millis(1500));
        });
    });

    dbg!();
}
