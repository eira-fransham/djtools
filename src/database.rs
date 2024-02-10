use std::{
    fmt::Display,
    io::{Read, Seek, Write},
};

use binrw::{BinRead, BinResult, BinWrite, Endian};

pub mod wire {
    use arrayvec::ArrayVec;
    use binrw::{binrw, BinRead, BinResult, BinWrite, Endian, NullWideString};
    use encoding_rs::{Encoding, UTF_16BE};
    use std::{
        io::{self, Read},
        mem,
    };

    #[binrw]
    #[brw(repr = u8)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum ArgType {
        String = 0x02,
        Blob = 0x03,
        Number = 0x06,
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ArgString(
        #[br(parse_with = utf16be_parser)]
        #[bw(write_with = utf16be_writer)]
        pub String,
    );

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ArgBlob {
        #[bw(try_calc(u32::try_from(data.len())))]
        pub size: u32,
        #[br(count = size)]
        pub data: Vec<u8>,
    }

    const MAX_NUMBER_SIZE: usize = 4;

    const U8_MAGIC: u8 = 0x0f;
    const U16_MAGIC: u8 = 0x10;
    const U32_MAGIC: u8 = 0x11;

    #[binrw]
    #[brw(repr = u8)]
    #[repr(u8)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum NumberSize {
        U8 = U8_MAGIC,
        U16 = U16_MAGIC,
        U32 = U32_MAGIC,
    }

    impl NumberSize {
        const fn to_byte_count(self) -> usize {
            match self {
                Self::U8 => 1,
                Self::U16 => 2,
                Self::U32 => 4,
            }
        }

        fn try_from_byte_count(count: usize) -> Result<Self, io::Error> {
            Ok(match count {
                1 => Self::U8,
                2 => Self::U16,
                4 => Self::U32,
                _ => return Err(io::ErrorKind::InvalidData.into()),
            })
        }
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ArgNumber {
        #[bw(try_calc(NumberSize::try_from_byte_count(number.1.len())))]
        size_tag: NumberSize,
        #[br(args(size_tag))]
        number: NumberInner,
    }

    impl ArgNumber {
        pub fn bytes(&self) -> &[u8] {
            self.number.bytes()
        }
    }

    impl<T> From<T> for ArgNumber
    where
        NumberInner: From<T>,
    {
        fn from(other: T) -> Self {
            Self {
                number: other.into(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct NumberInner(Endian, ArrayVec<u8, MAX_NUMBER_SIZE>);

    impl NumberInner {
        fn bytes(&self) -> &[u8] {
            &*self.1
        }

        fn make_buf<const SIZE: usize>(&self) -> [u8; SIZE] {
            let mut buf = [0; SIZE];
            let len = buf.len();

            let slice = match self.0 {
                Endian::Little => &mut buf[len - self.1.len()..len],
                Endian::Big => &mut buf[0..self.1.len()],
            };

            slice.copy_from_slice(&self.1[..]);

            buf
        }
    }

    impl From<[u8; 1]> for NumberInner {
        fn from(other: [u8; 1]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    impl From<[u8; 2]> for NumberInner {
        fn from(other: [u8; 2]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    impl From<[u8; 4]> for NumberInner {
        fn from(other: [u8; 4]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    macro_rules! number_int_conv {
    ($($t:ty),*) => {
        $(
            impl From<NumberInner> for $t {
                fn from(other: NumberInner) -> Self {
                   Self::read_options(&mut io::Cursor::new(other.make_buf::<{mem::size_of::<$t>()}>()), other.0, ()).expect("This should never fail!")
                }
            }

            impl From<$t> for NumberInner {
                fn from(other: $t) -> Self {
                    let endian = Endian::Big;
                    let mut out = io::Cursor::new([0u8; mem::size_of::<$t>()]);
                    other.write_options(&mut out, endian, ()).expect("This should never fail!");

                    Self(endian, out.into_inner().into_iter().collect())
                }
            }

            impl From<ArgNumber> for $t {
                fn from(other: ArgNumber) -> Self {
                    Self::from(other.number)
                }
            }
        )*
    };
}
    number_int_conv!(u8, u16, u32, i8, i16, i32);

    impl BinRead for NumberInner {
        type Args<'a> = (NumberSize,);

        fn read_options<R: Read + io::Seek>(
            reader: &mut R,
            endian: Endian,
            args: Self::Args<'_>,
        ) -> BinResult<Self> {
            let args = args.0;
            let mut out = ArrayVec::new();

            for _ in 0..args.to_byte_count() {
                out.push(0);
            }

            reader.read_exact(&mut out[..])?;

            Ok(Self(endian, out))
        }
    }

    impl BinWrite for NumberInner {
        type Args<'a> = ();

        fn write_options<W: io::Write + io::Seek>(
            &self,
            writer: &mut W,
            endian: Endian,
            args: Self::Args<'_>,
        ) -> BinResult<()> {
            let to_write = if endian == self.0 {
                self.1.clone()
            } else {
                self.1.clone().into_iter().rev().collect()
            };

            to_write[..].write_options(writer, endian, args)
        }
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Arg {
        String(ArgString),
        Blob(ArgBlob),
        Number(ArgNumber),
    }

    impl<T> From<T> for Arg
    where
        ArgNumber: From<T>,
    {
        fn from(other: T) -> Self {
            Self::Number(other.into())
        }
    }

    static ENCODING: &'static Encoding = UTF_16BE;

    #[binrw::parser(reader, endian)]
    fn utf16be_parser() -> BinResult<String> {
        let wide_str = NullWideString::read_options(reader, endian, ())?;
        let wide_str_bytes: &[u8] = bytemuck::cast_slice(&*wide_str.0);

        let (string, _, _) = ENCODING.decode(wide_str_bytes);
        Ok(string.into_owned())
    }

    #[binrw::writer(writer, endian)]
    fn utf16be_writer(string: &String) -> BinResult<()> {
        let (utf16, _, _) = ENCODING.encode(&string);
        utf16.write_options(writer, endian, ())?;
        0u16.write_options(writer, endian, ())?;
        Ok(())
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ArgVec(pub Vec<Arg>);

    impl BinRead for ArgVec {
        type Args<'a> = (&'a [ArgType],);

        fn read_options<R: Read + io::Seek>(
            reader: &mut R,
            endian: Endian,
            args: Self::Args<'_>,
        ) -> BinResult<Self> {
            let args = args.0;
            let mut out = Vec::with_capacity(args.len());
            for t in args {
                out.push(match t {
                    ArgType::Blob => Arg::Blob(<_>::read_options(reader, endian, ())?),
                    ArgType::String => Arg::String(<_>::read_options(reader, endian, ())?),
                    ArgType::Number => Arg::Number(<_>::read_options(reader, endian, ())?),
                });
            }

            Ok(Self(out))
        }
    }

    impl BinWrite for ArgVec {
        type Args<'a> = ();

        fn write_options<W: io::Write + io::Seek>(
            &self,
            writer: &mut W,
            endian: Endian,
            args: Self::Args<'_>,
        ) -> BinResult<()> {
            self.0.write_options(writer, endian, args)
        }
    }

    impl ArgVec {
        fn types(&self) -> impl Iterator<Item = ArgType> + '_ {
            self.0.iter().map(|val| match val {
                Arg::Blob(_) => ArgType::Blob,
                Arg::String(_) => ArgType::String,
                Arg::Number { .. } => ArgType::Number,
            })
        }
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Args {
        #[bw(try_calc(u8::try_from(args.0.len())))]
        #[brw(magic = 0x0f_u8)]
        _num_args: u8,
        // This is apparently redundant, arg_types appears to always take up 12 bytes of space
        #[bw(calc(TYPES_SIZE as _))]
        // Blob size is 0x14 instead of 0x11 even though it's still a u32?
        #[brw(magic = 0x14_u8)]
        _type_size: u32,
        #[br(parse_with = types_parser)]
        #[bw(write_with = types_writer)]
        pub arg_types: ArrayVec<ArgType, TYPES_SIZE>,
        #[br(args(&arg_types[..]))]
        pub args: ArgVec,
    }

    impl Args {
        fn new<A: Into<Arg>, I: IntoIterator<Item = A>>(args: I) -> Self {
            let args = ArgVec(args.into_iter().map(Into::into).collect::<Vec<_>>());
            let arg_types = args.types().collect::<ArrayVec<_, TYPES_SIZE>>();

            Args { arg_types, args }
        }
    }

    const TYPES_SIZE: usize = 12;

    #[binrw::parser(reader, endian)]
    fn types_parser() -> BinResult<ArrayVec<ArgType, TYPES_SIZE>> {
        let mut out = ArrayVec::default();
        let mut buf = [0u8; 1];
        for _ in 0..TYPES_SIZE {
            reader.read_exact(&mut buf)?;

            if buf == [0] {
                reader.seek(io::SeekFrom::Current((TYPES_SIZE - out.len() - 1) as _))?;
                break;
            }

            match ArgType::read_options(&mut io::Cursor::new(&buf), endian, ()) {
                Ok(ty) => out.push(ty),
                Err(e) => return Err(e),
            }
        }

        Ok(out)
    }

    #[binrw::writer(writer, endian)]
    fn types_writer(string: &ArrayVec<ArgType, 12>) -> BinResult<()> {
        let mut out = [0u8; TYPES_SIZE];
        for (src, dst) in string.iter().zip(&mut out) {
            *dst = *src as u8;
        }

        out.write_options(writer, endian, ())?;
        Ok(())
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Transaction {
        #[brw(magic = 0x11_u8)]
        #[bw(calc = ())]
        _magic_0: (),
        #[brw(magic = 0x872349ae_u32)]
        #[bw(calc = ())]
        _magic_1: (),
        #[brw(magic = 0x11_u8)]
        pub transaction_id: u32,
        #[brw(magic = 0x10_u8)]
        pub message_type: u16,
        pub args: Args,
    }

    const MAX_MENU_REQUEST_COUNT: u32 = 64;

    impl Transaction {
        pub const SETUP_ID: u32 = 0xfffffffe;
        pub const SETUP: u16 = 0x0;
        pub const READ_MENU: u16 = 0x3000;

        pub fn setup(player_number: u8) -> Self {
            Transaction {
                transaction_id: Self::SETUP_ID,
                message_type: Self::SETUP,
                args: Args::new([player_number as u32]),
            }
        }

        pub fn read_menu<C: Into<ArgNumber>>(
            transaction_id: u32,
            player: u8,
            slot: u8,
            count: C,
        ) -> Self {
            const TRACK_TYPE: u8 = 1;

            let count = count.into();

            // TODO: Offset
            Transaction {
                transaction_id,
                message_type: Self::READ_MENU,
                args: Args::new([
                    ArgNumber::from([player, 1, slot, TRACK_TYPE]),
                    // TODO: Offset
                    0u32.into(),
                    // TODO: This is the count in just this current packet, i.e. it should be separate from the
                    //       total and limited to `MAX_MENU_REQUEST_COUNT`
                    count.clone(),
                    // Always zero
                    0u32.into(),
                    count.clone(),
                    // Always zero
                    0u32.into(),
                ]),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Transaction {
    Setup {
        player_number: u8,
    },
    ReadMenu {
        id: u32,
        player_number: u8,
        slot: u8,
        count: u32,
    },
}

impl From<&Transaction> for wire::Transaction {
    fn from(value: &Transaction) -> Self {
        match *value {
            Transaction::Setup { player_number } => Self::setup(player_number),
            Transaction::ReadMenu {
                id,
                player_number,
                slot,
                count,
            } => Self::read_menu(id, player_number, slot, count),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FromWireError {
    NoSuchTransaction,
    InvalidArgs,
}

impl Display for FromWireError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSuchTransaction => write!(f, "No such transaction"),
            Self::InvalidArgs => write!(f, "Invalid arguments"),
        }
    }
}

impl TryFrom<wire::Transaction> for Transaction {
    type Error = FromWireError;

    fn try_from(value: wire::Transaction) -> Result<Self, Self::Error> {
        match (value.transaction_id, value.message_type) {
            (wire::Transaction::SETUP_ID, wire::Transaction::SETUP) => {
                if let [wire::Arg::Number(player_number)] = &*value.args.args.0 {
                    let player_number: u32 = player_number.clone().into();
                    let player_number = player_number as u8;

                    Ok(Transaction::Setup { player_number })
                } else {
                    Err(FromWireError::InvalidArgs)
                }
            }
            (id, wire::Transaction::READ_MENU) => {
                use wire::Arg::Number as N;
                if let [N(args), N(_offset), N(count), N(_), N(_count), N(_)] = &*value.args.args.0
                {
                    let count = count.clone().into();
                    if let [player_number, _1, slot, _track_type] = args.bytes() {
                        let player_number = *player_number;
                        let slot = *slot;

                        Ok(Transaction::ReadMenu {
                            id,
                            player_number,
                            slot,
                            count,
                        })
                    } else {
                        Err(FromWireError::InvalidArgs)
                    }
                } else {
                    Err(FromWireError::InvalidArgs)
                }
            }
            _ => Err(FromWireError::NoSuchTransaction),
        }
    }
}

impl BinRead for Transaction {
    type Args<'a> = <wire::Transaction as BinRead>::Args<'a>;

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let pos = reader.stream_position()?;
        wire::Transaction::read_options(reader, endian, args)?
            .try_into()
            .map_err(|e: FromWireError| binrw::Error::Custom {
                pos,
                err: Box::new(e),
            })
    }
}

impl BinWrite for Transaction {
    type Args<'a> = <wire::Transaction as BinWrite>::Args<'a>;

    fn write_options<W: Write + Seek>(
        &self,
        writer: &mut W,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<()> {
        wire::Transaction::from(self).write_options(writer, endian, args)
    }
}

#[cfg(test)]
mod test {
    use std::io::{self, Seek as _};

    use binrw::{BinRead as _, BinWrite as _};

    use crate::database::Transaction;

    // This crate is reverse-engineered with help from Dysentery (https://github.com/Deep-Symmetry/dysentery),
    // their code clearly works so I run against some test-cases of theirs.
    //
    // TODO: Can we host Clojure in Rust and do this fuzz-style?
    #[test]
    fn dysentery_parity() {
        const SETUP_MESSAGE: &[u8] = &[
            17, 135, 35, 73, 174, 17, 255, 255, 255, 254, 16, 0, 0, 15, 1, 20, 0, 0, 0, 12, 6, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17, 0, 0, 0, 1,
        ];
        const READ_MENU_MESSAGE: &[u8] = &[
            17, 135, 35, 73, 174, 17, 0, 0, 0, 1, 16, 48, 0, 15, 6, 20, 0, 0, 0, 12, 6, 6, 6, 6, 6,
            6, 0, 0, 0, 0, 0, 0, 17, 3, 1, 1, 1, 17, 0, 0, 0, 0, 17, 0, 0, 0, 32, 17, 0, 0, 0, 0,
            17, 0, 0, 0, 32, 17, 0, 0, 0, 0,
        ];

        let tests = [
            (Transaction::Setup { player_number: 1 }, SETUP_MESSAGE),
            (
                Transaction::ReadMenu {
                    id: 1,
                    player_number: 3,
                    slot: 1,
                    count: 32,
                },
                READ_MENU_MESSAGE,
            ),
        ];

        for (msg, target) in tests {
            let mut buf = io::Cursor::new(vec![0u8; target.len()]);

            msg.write_be(&mut buf).unwrap();

            // assert_eq!(target.len(), buf.position() as usize);
            assert_eq!(buf.get_ref(), target);
        }
    }

    #[test]
    fn check_roundtrip() {
        let transactions = [
            Transaction::Setup { player_number: 1 },
            Transaction::ReadMenu {
                id: 1,
                player_number: 1,
                slot: 1,
                count: 0xff,
            },
        ];

        for transaction in transactions {
            let mut buf = io::Cursor::new(vec![0u8; 32]);

            transaction.write_be(&mut buf).unwrap();

            buf.seek(io::SeekFrom::Start(0)).unwrap();

            assert_eq!(transaction, Transaction::read_be(&mut buf).unwrap());
        }
    }
}
