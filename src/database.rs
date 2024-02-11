use std::{
    fmt::Display,
    io::{Read, Seek, Write},
};

use binrw::{BinRead, BinResult, BinWrite, Endian};

pub mod wire {
    use arrayvec::ArrayVec;
    use binrw::{
        binrw,
        meta::{ReadEndian, WriteEndian},
        BinRead, BinResult, BinWrite, Endian, NullWideString,
    };
    use bytemuck::Pod;
    use encoding_rs::{Encoding, UTF_16BE};
    use std::{
        fmt::Display,
        io::{self, Read},
        iter,
        marker::PhantomData,
        mem,
        ops::{Deref, DerefMut},
    };

    #[binrw]
    #[brw(repr = u8)]
    #[brw(big)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum ArgType {
        String = 0x02,
        Blob = 0x03,
        Number = 0x06,
    }

    #[binrw]
    #[brw(big)]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ArgString(
        #[br(parse_with = utf16be_parser)]
        #[bw(write_with = utf16be_writer)]
        pub String,
    );

    #[binrw]
    #[brw(big)]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ArgBlob<Inner = u8, const FAKE_SIZE: usize = 0> {
        #[bw(try_calc(ArgNumber::<BlobSize>::try_from(BlobSize::from(data.len()))))]
        pub size: ArgNumber<BlobSize>,
        #[br(count = *size.number)]
        pub data: Vec<u8>,

        _phantom: PhantomData<Inner>,
    }

    impl<T> ArgBlob<T> {
        pub fn cast_items<U>(self) -> ArgBlob<U> {
            ArgBlob {
                data: self.data,
                _phantom: PhantomData,
            }
        }
    }

    impl<Inner: Pod> ArgBlob<Inner> {
        pub fn from_items(items: Vec<Inner>) -> Self {
            Self {
                data: bytemuck::cast_vec(items),
                _phantom: PhantomData,
            }
        }

        pub fn items(&self) -> &[Inner] {
            bytemuck::cast_slice(&*self.data)
        }

        pub fn items_mut(&mut self) -> &mut [Inner] {
            bytemuck::cast_slice_mut(&mut *self.data)
        }

        pub fn into_items(self) -> Vec<Inner> {
            bytemuck::cast_vec(self.data)
        }
    }

    impl<Inner: BinWrite + WriteEndian, const FAKE_SIZE: usize> ArgBlob<Inner, FAKE_SIZE>
    where
        for<'a> Inner::Args<'a>: Default,
    {
        pub fn write_items<I>(items: I) -> BinResult<Self>
        where
            I: IntoIterator<Item = Inner>,
        {
            let items = items.into_iter();
            let cap = items.size_hint().0.max(FAKE_SIZE);
            let mut data = vec![0; cap];
            let data = if FAKE_SIZE > 0 {
                let mut out = io::Cursor::new(data);

                for i in items {
                    i.write(&mut out)?;
                }

                out.into_inner()
            } else {
                let mut out = io::Cursor::new(&mut data[..]);

                for i in items {
                    i.write(&mut out)?;
                }

                data
            };

            Ok(Self {
                data,
                _phantom: PhantomData,
            })
        }
    }

    // TODO: Currently doesn't work if the inner size is more than 1
    impl<'a, Inner: BinRead, const FAKE_SIZE: usize> ArgBlob<Inner, FAKE_SIZE>
    where
        Inner::Args<'a>: Clone,
    {
        pub fn read_items_options(
            &'a self,
            endian: Endian,
            args: Inner::Args<'a>,
        ) -> impl Iterator<Item = BinResult<Inner>> + 'a {
            #[derive(Debug, Copy, Clone, PartialEq, Eq)]
            enum State {
                Unfinished,
                Done,
            }

            let mut buf = [0u8; 1];
            let mut reader = io::Cursor::new(&*self.data);
            let mut state = State::Unfinished;

            iter::from_fn(move || match &mut state {
                State::Unfinished => {
                    if let Err(e) = reader.read_exact(&mut buf) {
                        state = State::Done;

                        return match e.kind() {
                            io::ErrorKind::UnexpectedEof => None,
                            _ => Some(Err(binrw::Error::Io(e))),
                        };
                    }

                    if buf == [0] {
                        state = State::Done;
                        None
                    } else {
                        match Inner::read_options(&mut io::Cursor::new(&buf), endian, args.clone())
                        {
                            Ok(ty) => Some(Ok(ty)),
                            Err(e) => {
                                state = State::Done;

                                if FAKE_SIZE == 0 {
                                    Some(Err(e))
                                } else {
                                    None
                                }
                            }
                        }
                    }
                }
                State::Done => None,
            })
        }
    }

    impl<'a, Inner: BinRead + ReadEndian, const FAKE_SIZE: usize> ArgBlob<Inner, FAKE_SIZE>
    where
        Inner::Args<'a>: Default + Clone,
    {
        pub fn read_items(&'a self) -> impl Iterator<Item = BinResult<Inner>> + 'a {
            self.read_items_options(
                Inner::ENDIAN.endian().unwrap_or(Endian::Big),
                Default::default(),
            )
        }
    }

    impl<'a, Inner: BinRead, const FAKE_SIZE: usize> ArgBlob<Inner, FAKE_SIZE>
    where
        Inner::Args<'a>: Default + Clone,
    {
        pub fn read_items_le(&'a self) -> impl Iterator<Item = BinResult<Inner>> + 'a {
            self.read_items_options(Endian::Little, Default::default())
        }

        pub fn read_items_be(&'a self) -> impl Iterator<Item = BinResult<Inner>> + 'a {
            self.read_items_options(Endian::Big, Default::default())
        }
    }

    const MAX_NUMBER_SIZE: usize = 4;

    const U8_MAGIC: u8 = 0x0f;
    const U16_MAGIC: u8 = 0x10;
    const U32_MAGIC: u8 = 0x11;
    const USIZE_MAGIC: u8 = 0x14;

    #[binrw]
    #[brw(repr = u8)]
    #[repr(u8)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum DynSize {
        U8 = U8_MAGIC,
        U16 = U16_MAGIC,
        U32 = U32_MAGIC,
        USize = USIZE_MAGIC,
    }

    impl Default for DynSize {
        fn default() -> Self {
            Self::U32
        }
    }

    impl DynSize {
        const fn to_byte_count(self) -> usize {
            match self {
                Self::U8 => 1,
                Self::U16 => 2,
                Self::U32 | Self::USize => 4,
            }
        }

        fn try_from_byte_count(size: usize) -> Result<DynSize, InvalidSize> {
            Ok(match size {
                1 => DynSize::U8,
                2 => DynSize::U16,
                4 => DynSize::U32,
                _ => return Err(InvalidSize),
            })
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct InvalidSize;

    impl Display for InvalidSize {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Invalid size")
        }
    }

    pub trait ByteCountToSize: BinWrite + BinRead {
        fn try_size(&self) -> Result<DynSize, InvalidSize>;
        fn read_args<'a>(size: DynSize) -> <Self as BinRead>::Args<'a>;
        fn write_args<'a>() -> <Self as BinWrite>::Args<'a>;
    }

    impl ByteCountToSize for DynNumber {
        fn try_size(&self) -> Result<DynSize, InvalidSize> {
            DynSize::try_from_byte_count(self.1.len())
        }

        fn read_args<'a>(size: DynSize) -> <Self as BinRead>::Args<'a> {
            (size,)
        }
        fn write_args<'a>() -> <Self as BinWrite>::Args<'a> {
            Default::default()
        }
    }

    macro_rules! byte_count_to_size {
        ($($t:ty),*) => {
            $(
                impl ByteCountToSize for $t {
                    fn try_size(&self) -> Result<DynSize, InvalidSize> {
                        DynSize::try_from_byte_count(mem::size_of::<$t>())
                    }

                    fn read_args<'a>(_: DynSize) -> <Self as BinRead>::Args<'a> {
                        ()
                    }

                    fn write_args<'a>() -> <Self as BinWrite>::Args<'a> { Default::default() }
                }
            )*
        }
    }

    byte_count_to_size!(u8, u16, u32, i8, i16, i32);

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    #[repr(transparent)]
    struct BlobSize(pub u32);

    impl Deref for BlobSize {
        type Target = u32;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl DerefMut for BlobSize {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    impl From<u32> for BlobSize {
        fn from(value: u32) -> Self {
            Self(value)
        }
    }

    impl From<usize> for BlobSize {
        fn from(value: usize) -> Self {
            Self(value as _)
        }
    }

    impl ByteCountToSize for BlobSize {
        fn try_size(&self) -> Result<DynSize, InvalidSize> {
            Ok(DynSize::USize)
        }

        fn read_args<'a>(_size: DynSize) -> <Self as BinRead>::Args<'a> {
            ()
        }

        fn write_args<'a>() -> <Self as BinWrite>::Args<'a> {
            Default::default()
        }
    }

    #[binrw]
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct ArgNumber<Inner: ByteCountToSize = DynNumber> {
        #[bw(try_calc(<Inner>::try_size(&number)))]
        size_tag: DynSize,
        #[br(args_raw(<Inner as ByteCountToSize>::read_args(size_tag)))]
        #[bw(args_raw(<Inner as ByteCountToSize>::write_args()))]
        number: Inner,
    }

    impl<T> From<T> for ArgNumber<T>
    where
        T: ByteCountToSize,
    {
        fn from(number: T) -> Self {
            Self { number }
        }
    }

    impl ArgNumber {
        pub fn from_bytes<const SIZE: usize>(bytes: [u8; SIZE]) -> Self
        where
            DynNumber: From<[u8; SIZE]>,
        {
            Self::from(DynNumber::from(bytes))
        }
        pub fn bytes(&self) -> &[u8] {
            self.number.bytes()
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct DynNumber(Endian, ArrayVec<u8, MAX_NUMBER_SIZE>);

    impl DynNumber {
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

    impl From<[u8; 1]> for DynNumber {
        fn from(other: [u8; 1]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    impl From<[u8; 2]> for DynNumber {
        fn from(other: [u8; 2]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    impl From<[u8; 4]> for DynNumber {
        fn from(other: [u8; 4]) -> Self {
            Self(Endian::Big, other.into_iter().collect())
        }
    }

    macro_rules! number_int_conv {
        ($($t:ty),*) => {
            $(
                impl From<DynNumber> for $t {
                    fn from(other: DynNumber) -> Self {
                       Self::read_options(&mut io::Cursor::new(other.make_buf::<{mem::size_of::<$t>()}>()), other.0, ()).expect("This should never fail!")
                    }
                }

                impl From<$t> for DynNumber {
                    fn from(other: $t) -> Self {
                        let endian = Endian::Big;
                        let mut out = io::Cursor::new([0u8; mem::size_of::<$t>()]);
                        other.write_options(&mut out, endian, ()).expect("This should never fail!");

                        Self(endian, out.into_inner().into_iter().collect())
                    }
                }

                impl<T> From<ArgNumber<T>> for $t
                where
                    T: ByteCountToSize,
                    $t: From<T>,
                {
                    fn from(other: ArgNumber<T>) -> Self {
                        Self::from(other.number)
                    }
                }

                impl From<$t> for ArgNumber {
                    fn from(other: $t) -> Self {
                        Self { number: other.into() }
                    }
                }
            )*
        };
    }

    number_int_conv!(u8, u16, u32, i8, i16, i32);

    impl BinRead for DynNumber {
        type Args<'a> = (DynSize,);

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

    impl BinWrite for DynNumber {
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
        type Args<'a> = (ArgBlob<ArgType, 12>,);

        fn read_options<R: Read + io::Seek>(
            reader: &mut R,
            endian: Endian,
            args: Self::Args<'_>,
        ) -> BinResult<Self> {
            let args = args.0.read_items();
            let mut out = Vec::with_capacity(args.size_hint().0);
            for t in args.filter_map(Result::ok) {
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
        #[bw(try_calc(ArgBlob::<ArgType, 12>::write_items(args.types())))]
        arg_types: ArgBlob<ArgType, 12>,
        #[br(args(arg_types))]
        pub args: ArgVec,
    }

    impl Args {
        fn new<A: Into<Arg>, I: IntoIterator<Item = A>>(args: I) -> Self {
            Args {
                args: ArgVec(args.into_iter().map(Into::into).collect::<Vec<_>>()),
            }
        }
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
        pub transaction_id: ArgNumber<u32>,
        pub message_type: ArgNumber<u16>,
        pub args: Args,
    }

    impl Transaction {
        pub const SETUP_ID: u32 = 0xfffffffe;
        pub const SETUP: u16 = 0x0;
        pub const READ_MENU: u16 = 0x3000;

        pub fn setup(player_number: u8) -> Self {
            Transaction {
                transaction_id: Self::SETUP_ID.into(),
                message_type: Self::SETUP.into(),
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
                transaction_id: transaction_id.into(),
                message_type: Self::READ_MENU.into(),
                args: Args::new([
                    ArgNumber::from_bytes([player, 1, slot, TRACK_TYPE]),
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
        match (value.transaction_id.into(), value.message_type.into()) {
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
