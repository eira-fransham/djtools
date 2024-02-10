use arrayvec::ArrayVec;
use binrw::{binrw, BinRead, BinReaderExt, BinResult, BinWrite, Endian, NullWideString};
use encoding_rs::{Encoding, UTF_16BE};
use std::io::{self, Read};

#[binrw]
#[brw(repr = u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum ArgType {
    String = 0x02,
    Blob = 0x03,
    Number = 0x06,
}

#[binrw]
#[brw(little)]
#[derive(Clone, PartialEq, Eq, Hash)]
struct ArgString(
    #[br(parse_with = utf16be_parser)]
    #[bw(write_with = utf16be_writer)]
    String,
);

#[binrw]
#[brw(little)]
#[derive(Clone, PartialEq, Eq, Hash)]
struct ArgBlob {
    #[bw(try_calc(u32::try_from(data.len())))]
    size: u32,
    #[br(count = size)]
    data: Vec<u8>,
}

#[binrw]
#[brw(little)]
#[derive(Clone, PartialEq, Eq, Hash)]
enum Arg {
    String(ArgString),
    Blob(ArgBlob),
    Number(i64),
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
#[derive(Clone, PartialEq, Eq, Hash)]
struct ArgVec(Vec<Arg>);

impl BinRead for ArgVec {
    type Args<'a> = &'a [ArgType];

    fn read_options<R: Read + io::Seek>(
        reader: &mut R,
        endian: Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
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

#[binrw]
#[brw(little)]
struct Args {
    #[bw(try_calc(u8::try_from(args.len())))]
    num_args: u8,
    // This is apparently redundant, arg_types appears to always take up 12 bytes of space
    #[bw(try_calc(u32::try_from(arg_types.len())))]
    _type_size: u32,
    #[br(parse_with = types_parser)]
    #[bw(write_with = types_writer)]
    arg_types: ArrayVec<ArgType, TYPES_SIZE>,
    #[br(count = num_args)]
    args: Vec<Arg>,
}

const TYPES_SIZE: usize = 12;

#[binrw::parser(reader, endian)]
fn types_parser() -> BinResult<ArrayVec<ArgType, TYPES_SIZE>> {
    let mut out = ArrayVec::default();
    while !out.is_full() {
        match ArgType::read_options(reader, endian, ()) {
            Ok(ty) => out.push(ty),
            Err(binrw::Error::NoVariantMatch { .. }) => break,
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
#[brw(little, magic = 0x872349ae_u32)]
struct Transaction {
    transaction_id: u32,
    message_type: u16,
    args: Args,
}

