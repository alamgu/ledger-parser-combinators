// use crate::schema::*;
use crate::interp::*;
use core::future::Future;
use crate::async_parser::*;
use crate::protobufs::schema::*;
use crate::protobufs::interp::*;
use arrayvec::ArrayVec;
pub use num_traits::FromPrimitive;

trait IsLengthDelimited { }

struct VarInt32;

impl HasOutput<Varint> for VarInt32 {
    type Output = u32;
}

impl<BS: Readable> AsyncParser<Varint, BS> for VarInt32 {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut accumulator : u32 = 0;
            let mut n : u8 = 0;
            loop {
                let [current] : [u8; 1] = input.read().await;
                if n == 4 && current > 0x0f {
                    reject().await
                }
                accumulator += (current as u32) << 7*n;
                n += 1;
                if current & 0x80 == 0 {
                    return accumulator;
                }
            }
        }
    }
    type State<'c> = impl Future<Output = Self::Output>;
}

impl HasOutput<Varint> for DropInterp {
    type Output = ();
}
impl<BS: Readable> AsyncParser<Varint, BS> for DropInterp {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut n : u8 = 0;
            loop {
                let [current] : [u8; 1] = input.read().await;
                if n == 4 && current > 0x0f {
                    reject().await
                }
                n += 1;
                if current & 0x80 == 0 {
                    return ()
                }
            }
        }
    }
    type State<'c> = impl Future<Output = Self::Output>;
}

impl HasOutput<Fixed64> for DefaultInterp {
    type Output = [u8; 8];
}

impl<BS: Readable> AsyncParser<Fixed64, BS> for DefaultInterp {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            input.read().await
        }
    }
    type State<'c> = impl Future<Output = Self::Output>;
}

trait LengthDelimitedParser<Schema, BS: Readable> : HasOutput<Schema>{
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c>;
    type State<'c>: Future<Output = Self::Output>;
}

impl<const N : usize> HasOutput<String> for Buffer<N> {
    type Output = ArrayVec<u8, N>;
}

async fn read_arrayvec_n<'a, const N: usize, BS: Readable>(input: &'a mut BS, length: usize) -> ArrayVec<u8, N> {
    if length > N {
        reject().await
    }
    let mut accumulator = ArrayVec::new();
    for _ in 0..length {
        let [byte] = input.read().await;
        accumulator.push(byte);
    }
    accumulator
}

impl<const N : usize, BS: Readable> LengthDelimitedParser<String, BS> for Buffer<N> {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        read_arrayvec_n(input, length)
    }
    type State<'c> = impl Future<Output = Self::Output>;
}

impl<const N : usize> HasOutput<Bytes> for Buffer<N> {
    type Output = ArrayVec<u8, N>;
}

impl<const N: usize, BS: Readable> LengthDelimitedParser<Bytes, BS> for Buffer<N> {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        read_arrayvec_n(input, length)
    }
    type State<'c> = impl Future<Output = Self::Output>;
}

impl<const FIELD_NUMBER: u32, Schema: ProtobufWireFormat, Value: HasOutput<Schema>> HasOutput<MessageField<FIELD_NUMBER, Schema>> for MessageFieldInterp<FIELD_NUMBER, Value> {
    type Output = Value::Output;
}

struct TrackLength<BS: Readable>(BS, usize);

impl<BS: 'static + Readable> Readable for TrackLength<BS> {
    type OutFut<'a, const N: usize> = impl Future<Output = [u8; N]>;
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N> {
        self.1 += N;
        self.0.read()
    }
}

pub async fn skip_field<BS: Readable>(fmt: ProtobufWire, i: &mut BS) {
    match fmt {
        ProtobufWire::Varint => { <DropInterp as AsyncParser<Varint, BS>>::parse(&DropInterp, i).await; }
        ProtobufWire::Fixed64Bit => { i.read::<8>().await; }
        ProtobufWire::LengthDelimited => {
            let len = <VarInt32 as AsyncParser<Varint, BS>>::parse(&VarInt32, i).await;
            for _ in [0..len] {
                i.read::<1>().await;
            }
        }
        ProtobufWire::StartGroup => { reject().await }
        ProtobufWire::EndGroup => { reject().await }
        ProtobufWire::Fixed32Bit => { i.read::<4>().await; }
    }
}

pub use paste::paste;

#[macro_export]
macro_rules! define_message {
    { $name:ident { $($field:ident : $schemaType:tt $(($schemaParams:tt))* = $number:literal),* } } => {
        define_message!{ @enrich, $name { , $($field : $schemaType$(($schemaParams))* = $number),* } { } }
    };
    { @enrich, $name:ident { , $field:ident : message($schemaType:tt) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, $schemaType) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : packed($schemaType:tt) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, $schemaType) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : bytes = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, Bytes) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : string = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, ProtoString) = $number } }
    };
    { @enrich, $name:ident { , $field:ident: enum($schemaType:ty) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (AsyncParser, $schemaType) = $number } }
    };
    { @enrich, $name:ident { , $field:ident: $schemaType:ty = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (AsyncParser, $schemaType) = $number } }
    };
    { @enrich, $name:ident { } { $($body:tt)* } } => {
        define_message!{ @impl $name { $($body)* } }
    };
    { @impl $name:ident { , $($field:ident : ($parseTrait:ident, $schemaType:ty) = $number:literal),* } } => {
        $crate::protobufs::async_parser::paste! {
            pub struct [<$name Interp>]<$([<Field $field:camel>]),*> {
                $(pub [<field_ $field:snake>] : [<Field $field:camel>] ),*
            }
            
            pub struct [<$name:camel>];

            impl<$([<Field $field:camel Interp>] : HasOutput<[<$schemaType:camel>]>),*> HasOutput<[<$name:camel>]> for [<$name:camel Interp>]<$([<Field $field:camel Interp>]),*> {
                type Output = ();
            }

            impl ProtobufWireFormat for [<$name:camel>] {
                const FORMAT: ProtobufWire = ProtobufWire::LengthDelimited;
            }

            impl<BS: 'static + Clone + Readable,$([<Field $field:camel Interp>] : $parseTrait<[<$schemaType:camel>], TrackLength<BS>>),*> LengthDelimitedParser<[<$name:camel>], BS> for [<$name:camel Interp>]<$([<Field $field:camel Interp>]),*> {
                fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
                    async move {
                        // First, structural check:
                        let mut tl = TrackLength(input.clone(), 0);
                        loop {
                            // Probably should check for presence of all expected fields here as
                            // well. On the other hand, fields that we specify an interpretation
                            // for are _required_.
                            let tag = VarInt32.parse(&mut tl).await;
                            let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                            skip_field(wire, &mut tl).await;
                            if tl.1 == length {
                                break;
                            }
                            if tl.1 > length {
                                return reject().await;
                            }
                        }
                        $(
                            let mut tl = TrackLength(input.clone(), 0);
                            loop {
                                let tag = VarInt32.parse(&mut tl).await;
                                let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                                if tag >> 3 == $number {
                                    if wire != [<$schemaType:camel>]::FORMAT {
                                        return reject().await;
                                    }
                                    define_message! { @call_parser_for, $parseTrait, tl, self.[<field_ $field:snake>] }
                                    break;
                                } else {
                                    skip_field(wire, &mut tl).await;
                                    // Skip it
                                }
                                if tl.1 >= length {
                                    return reject().await;
                                }
                            }
                        )*
                        ()
                    }
                }
                type State<'c> = impl Future<Output = Self::Output>;
            }
        }
    };
    { @call_parser_for, AsyncParser, $tl:ident, $($p:tt)* } => {
        $($p)*.parse(&mut $tl).await;
    };
    { @call_parser_for, LengthDelimitedParser, $tl:ident, $($p:tt)* } => { {
       let length = VarInt32.parse(&mut $tl).await as usize;
       $($p)*.parse(&mut $tl, length).await;
    } };
}

#[macro_export]
macro_rules! define_enum {
    { $name:ident { $($variant:ident = $number:literal),* } } =>
    {
        $crate::protobufs::async_parser::paste! {
            #[derive($crate::num_derive::FromPrimitive, PartialEq)]
            #[repr(u32)]
            pub enum $name {
                $([<$variant:camel>] = $number),*
            }

            impl HasOutput<$name> for DefaultInterp {
                type Output = $name;
            }
            
            impl ProtobufWireFormat for [<$name:camel>] {
                const FORMAT: ProtobufWire = ProtobufWire::Varint;
            }

            impl<BS: Readable> AsyncParser<$name, BS> for DefaultInterp {
                fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
                    async move {
                        match $name::from_u32(VarInt32.parse(input).await) {
                            None => reject().await,
                            Some(a) => a,
                        }
                    }
                }
                type State<'c> = impl Future<Output = Self::Output>;
            }
        }
    }
}

#[cfg(test)]
mod test {
    trace_macros!(true);
    define_message! { OtherMessage { foo: bytes = 0 } }
    define_message! { SimpleMessage { foo: message(otherMessage) = 0, bar: enum(SimpleEnum) = 1 } }
    define_enum! { SimpleEnum { default = 0, noodle = 1 } }
    trace_macros!(false);
    
    #[test]
    fn test_parser() {

    }
}

