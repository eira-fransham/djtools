use std::{net::UdpSocket, thread, time::Duration};

use djtools::finder::{self, DEFAULT_ANNOUNCE_PORT};

fn main() {
    let [finder_a, finder_b] = [DEFAULT_ANNOUNCE_PORT - 2, DEFAULT_ANNOUNCE_PORT - 1].map(|port| {
        let sock = UdpSocket::bind(("0.0.0.0", port)).unwrap();
        sock.set_nonblocking(true).unwrap();
        finder::Finder::<_>::from_socket(sock)
    });

    finder_b.socket().set_broadcast(true).unwrap();

    thread::scope(|scope| {
        scope.spawn(|| loop {
            finder_a.update_known_devices().unwrap();
            thread::sleep(Duration::from_millis(1100));
        });

        scope.spawn(|| {
            thread::sleep(Duration::from_millis(450));
            loop {
                for d in finder_a.known_devices() {
                    dbg!(d.pair());
                }
                thread::sleep(Duration::from_millis(900));
            }
        });

        scope.spawn(|| loop {
            finder_b.send_keepalive().unwrap();
            thread::sleep(Duration::from_millis(1500));
        });
    });

    dbg!();
}
