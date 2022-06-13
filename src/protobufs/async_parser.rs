// use crate::schema::*;
use crate::interp::*;
use core::future::Future;
use crate::async_parser::*;
use crate::protobufs::schema::*;
use crate::protobufs::interp::*;
use arrayvec::ArrayVec;
pub use num_traits::FromPrimitive;

trait IsLengthDelimited { }

fn parse_varint<'a: 'c, 'c, T, BS: Readable>(input: &'a mut BS) -> impl Future<Output = T> + 'c where
    T: Default + core::ops::Shl<Output = T> + core::ops::AddAssign + core::convert::From<u8>
{
    async move {
        let mut accumulator : T = Default::default();
        let mut n : u8 = 0;
        loop {
            let [current] : [u8; 1] = input.read().await;
            // Check that adding this base-128 digit will not overflow
            if 7*n as usize > (core::mem::size_of::<T>()-1)*8 && 0 != ((current & 0x7f) >> (core::mem::size_of::<T>()*8 - (7*n as usize))) {
                reject().await
            }
            accumulator += core::convert::Into::<T>::into(current & 0x7f) << core::convert::From::from(7*n as u8);
            n += 1;
            if current & 0x80 == 0 {
                return accumulator;
            }
        }
    }
}

fn skip_varint<'a: 'c, 'c, BS: Readable>(input: &'a mut BS) -> impl Future<Output = ()> + 'c where
{
    async move {
        loop {
            let [current] : [u8; 1] = input.read().await;
            if current & 0x80 == 0 {
                return ();
            }
        }
    }
}

macro_rules! VarintPrimitive {
    { $name:ident : $returning:ty : $v:ident => $($decode:tt)* } =>
    { $crate::protobufs::async_parser::paste! {
        impl HasOutput<[<$name:camel>]> for DefaultInterp {
            type Output = $returning;
        }

        impl<BS: Readable> AsyncParser<[<$name:camel>], BS> for DefaultInterp {
            type State<'c> = impl Future<Output = Self::Output>;
            fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
                async move {
                    let $v = parse_varint::<'a, 'c, $returning, BS>(input).await;
                    $($decode)*
                }
            }
        }

        impl<BS: Readable> AsyncParser<[<$name:camel>], BS> for DropInterp {
            type State<'c> = impl Future<Output = Self::Output>;
            fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
                async move {
                    parse_varint::<'a, 'c, $returning, BS>(input).await;
                }
            }
        }
    } }
}

VarintPrimitive! { int32 : i32 : x => x }
VarintPrimitive! { int64 : i64 : x => x }
VarintPrimitive! { uint32 : u32 : x => x }
VarintPrimitive! { uint64 : u64 : x => x }
VarintPrimitive! { sint32 : i32 : x => x >> 1 ^ (-(x & 1)) }
VarintPrimitive! { sint64 : i64 : x => x >> 1 ^ (-(x & 1)) }

impl HasOutput<Bool> for DefaultInterp {
    type Output = bool;
}

impl<BS: Readable> AsyncParser<Bool, BS> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            parse_varint::<'a, 'c, u16, BS>(input).await == 1
        }
    }
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


impl<T, S: LengthDelimitedParser<T, BS>, R, BS: Readable, F: Fn(<S as HasOutput<T>>::Output) -> Option<R>> LengthDelimitedParser<T, BS> for Action<S, F> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        async move {
            match self.1(self.0.parse(input, length).await) {
                Some(a) => a,
                None => reject().await,
            }
        }
    }
}

impl<Schema, BS: Readable> LengthDelimitedParser<Schema, BS> for DropInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        async move {
            for _ in 0..length {
                let [_]: [u8; 1] = input.read().await;
            }
        }
    }
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

#[derive(Clone)]
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
        ProtobufWire::Varint => { skip_varint::<'_, '_, BS>(i).await } // <DropInterp as AsyncParser<Varint, BS>>::parse(&DropInterp, i).await; }
        ProtobufWire::Fixed64Bit => { i.read::<8>().await; }
        ProtobufWire::LengthDelimited => {
            let len = <DefaultInterp as AsyncParser<Int32, BS>>::parse(&DefaultInterp, i).await;
            for _ in 0..len {
                i.read::<1>().await;
            }
        }
        ProtobufWire::StartGroup => { reject().await }
        ProtobufWire::EndGroup => { reject().await }
        ProtobufWire::Fixed32Bit => { i.read::<4>().await; }
    }
}

pub struct BytesAsMessage<Schema, M: HasOutput<Schema>>(Schema, M);

impl<Schema, M: HasOutput<Schema>> HasOutput<Bytes> for BytesAsMessage<Schema, M> {
    type Output = M::Output;
}

impl<Schema, M: LengthDelimitedParser<Schema, BS>, BS: Readable> LengthDelimitedParser<Bytes, BS> for BytesAsMessage<Schema, M> {
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        self.1.parse(input, length)
    }
    type State<'c> = impl Future<Output = Self::Output>;
}


pub use paste::paste;

#[macro_export]
macro_rules! define_message {
    { $name:ident { $($field:ident : $schemaType:tt $(($($schemaParams:tt)*))* = $number:literal),* } } => {
        define_message!{ @enrich, $name { , $($field : $schemaType$(($($schemaParams)*))* = $number),* } { } }
    };
    { @enrich, $name:ident { , $field:ident : message($schemaType:tt) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, $schemaType, false) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : repeated(message($schemaType:tt)) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, $schemaType, true) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : packed($schemaType:tt) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, $schemaType, true) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : bytes = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, Bytes, false) = $number } }
    };
    { @enrich, $name:ident { , $field:ident : string = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (LengthDelimitedParser, String, false) = $number } }
    };
    { @enrich, $name:ident { , $field:ident: enum($schemaType:ty) = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (AsyncParser, $schemaType, false) = $number } }
    };
    { @enrich, $name:ident { , $field:ident: $schemaType:ty = $number:literal $($rest:tt)* } { $($body:tt)* } } => {
        define_message!{ @enrich, $name { $($rest)* } { $($body)*, $field: (AsyncParser, $schemaType, false) = $number } }
    };
    { @enrich, $name:ident { } { $($body:tt)* } } => {
        define_message!{ @impl $name { $($body)* } }
    };
    { @impl $name:ident { , $($field:ident : ($parseTrait:ident, $schemaType:ty, $repeated:literal) = $number:literal),* } } => {
        $crate::protobufs::async_parser::paste! {
            pub struct [<$name Interp>]<$([<Field $field:camel>]),*> {
                $(pub [<field_ $field:snake>] : [<Field $field:camel>] ),*
            }

            pub struct [<$name:camel>];

            impl<$([<Field $field:camel Interp>] : HasOutput<[<$schemaType:camel>], Output=()>),*> HasOutput<[<$name:camel>]> for [<$name:camel Interp>]<$([<Field $field:camel Interp>]),*> {
                type Output = ();
            }

            impl ProtobufWireFormat for [<$name:camel>] {
                const FORMAT: ProtobufWire = ProtobufWire::LengthDelimited;
            }

            impl<BS: 'static + Clone + Readable,$([<Field $field:camel Interp>] : HasOutput<[<$schemaType:camel>], Output=()> + $parseTrait<[<$schemaType:camel>], TrackLength<BS>>),*> LengthDelimitedParser<[<$name:camel>], BS> for [<$name:camel Interp>]<$([<Field $field:camel Interp>]),*> {
                fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
                    async move {
                        // First, structural check:
                        let mut tl = TrackLength(input.clone(), 0);
                        loop {
                            // Probably should check for presence of all expected fields here as
                            // well. On the other hand, fields that we specify an interpretation
                            // for are _required_.
                            let tag : u32 = parse_varint(&mut tl).await;
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
                            let mut seen = false;
                            loop {
                                let tag : u32 = parse_varint(&mut tl).await;
                                let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                                if tag >> 3 == $number {
                                    if wire != [<$schemaType:camel>]::FORMAT {
                                        return reject().await;
                                    }
                                    define_message! { @call_parser_for, $parseTrait, tl, self.[<field_ $field:snake>] }
                                    if(seen && ! $repeated) {
                                        // Rejecting because of multiple fields on non-repeating;
                                        // protobuf spec says we should "take the last value" but
                                        // our flow doesn't permit this.
                                        return reject().await;
                                    }
                                    seen = true;
                                } else {
                                    skip_field(wire, &mut tl).await;
                                    // Skip it
                                }
                                if tl.1 == length {
                                    break;
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
       let length : usize = parse_varint(&mut $tl).await;
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
                        match $name::from_u32(parse_varint(input).await) {
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

use trie_enum::TrieLookup;
pub struct StringEnum<E: TrieLookup>(core::marker::PhantomData<E>);

pub const fn string_enum<E: TrieLookup>() -> StringEnum<E> {
    StringEnum(core::marker::PhantomData)
}

impl<E: TrieLookup> HasOutput<String> for StringEnum<E> {
    type Output = E;
}

impl<E: 'static + TrieLookup + core::fmt::Debug, BS: Readable> LengthDelimitedParser<String, BS> for StringEnum<E> where [(); E::N]: Sized {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
        async move {
            let mut cursor = E::start();
            for _ in 0..length {
                let [c] = input.read().await;
                cursor = match cursor.step(c) {
                    None => reject().await,
                    Some(cur) => cur
                }
            }
            match cursor.get_val() {
                None => reject().await,
                Some(r) => *r
            }
        }
    }
}

pub use trie_enum::enum_trie;

#[macro_export]
macro_rules! any_of {
    { $name:ident { $($variant:ident : $schema:ident = $string:literal),* } } =>
    { $crate::protobufs::async_parser::paste! {

        enum_trie! { [< $name Discriminator >] { $($variant = $string),* } }
        struct $name<$([< $variant:camel Interp >]),*>{
            $([<$variant:snake>]: [< $variant:camel Interp >]),*
        }

        impl<O, $([< $variant:camel Interp >]: HasOutput<$schema, Output = O>),*> HasOutput<Any> for $name<$([< $variant:camel Interp >]),*>
        {
            type Output = O;
        }

        impl<BS: 'static + Clone + Readable, O, $([< $variant:camel Interp >]: LengthDelimitedParser<$schema, TrackLength<BS>> + HasOutput<$schema, Output = O>),*> LengthDelimitedParser<Any, BS> for $name<$([< $variant:camel Interp >]),*> {
            type State<'c> = impl Future<Output = Self::Output>;
            fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS, length: usize) -> Self::State<'c> {
                async move {
                    let mut tl = TrackLength(input.clone(), 0);
                    let mut seen = false;
                    let mut discriminator = None;
                    loop {
                        let tag : u32 = parse_varint(&mut tl).await;
                        let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                        if tag >> 3 == 1 {
                            if wire != ProtobufWire::LengthDelimited {
                                return reject().await;
                            }
                            if(seen) {
                                return reject().await;
                            }
                            seen = true;
                            let length : usize = parse_varint(&mut tl).await;
                            let discrim_parse = [<$name Discriminator>]::start();
                            for _ in 0..length {
                                let [c] = tl.read().await;
                                discrim_parse.step(c);
                            }
                            discriminator = discrim_parse.get_val();
                        } else {
                            skip_field(wire, &mut tl).await;
                            // Skip it
                        }
                        if tl.1 == length {
                            break;
                        }
                        if tl.1 >= length {
                            return reject().await;
                        }
                    }

                    match discriminator {
                        None => { reject().await }
                        $(
                            Some([<$name Discriminator>]::$variant) => {
                                let mut tl = TrackLength(input.clone(), 0);
                                let mut seen = false;
                                loop {
                                    let tag : u32 = parse_varint(&mut tl).await;
                                    let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                                    if tag >> 3 == 2 {
                                        if wire != [<$schema:camel>]::FORMAT {
                                            return reject().await;
                                        }
                                        let length : usize = parse_varint(&mut tl).await;
                                        self.[<$variant:snake>].parse(&mut tl, length).await;
                                        if(seen) {
                                            // Rejecting because of multiple fields on non-repeating;
                                            // protobuf spec says we should "take the last value" but
                                            // our flow doesn't permit this.
                                            return reject().await;
                                        }
                                        seen = true;
                                    } else {
                                        skip_field(wire, &mut tl).await;
                                        // Skip it
                                    }
                                    if tl.1 == length {
                                        break;
                                    }
                                    if tl.1 >= length {
                                        return reject().await;
                                    }
                                }
                            }
                        )*

                    }

                    return reject().await;

                }
            }
        }
    } }
}

/*
// Any handler: take a list of
pub struct AnyOf<T: HList>(T);

impl<T> HasOutput<Any> for AnyOf<T> {
    type Output = ();
}

impl LengthDelimitedParser<Any> for AnyOf<T> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        {
            let mut tl = TrackLength(input.clone(), 0);
            let mut seen = false;
            loop {
                let tag : u32 = parse_varint(&mut tl).await;
                let wire = match ProtobufWire::from_u32(tag & 0x07) { Some(w) => w, None => reject().await, };
                if tag >> 3 == 1 {
                    if wire != ProtobufWire::LengthDelimited {
                        println!("Incorrect format: reject");
                        return reject().await;
                    }
                    let length : usize = parse_varint(&mut tl).await;
                    
                    define_message! { @call_parser_for, $parseTrait, tl, self.[<field_ $field:snake>] }
                    if(seen && ! $repeated) {
                        // Rejecting because of multiple fields on non-repeating;
                        // protobuf spec says we should "take the last value" but
                        // our flow doesn't permit this.
                        return reject().await;
                    }
                    seen = true;
                } else {
                    println!("Not current field; skipping");
                    skip_field(wire, &mut tl).await;
                    // Skip it
                }
                if tl.1 == length {
                    break;
                }
                if tl.1 >= length {
                    println!("Message off end; rejecting.");
                    return reject().await;
                }
            }
        }
        
    }
}
*/

#[cfg(test)]
mod test {
    use super::*;
    use core::future::Future;
    pub use num_traits::FromPrimitive;
    // trace_macros!(true);
    // trace_macros!(false);

    #[derive(Clone)]
    struct TestReadable<const N: usize>([u8; N], usize);
    impl<const M: usize> Readable for TestReadable<M> {
        type OutFut<'a, const N: usize> = impl 'a + Future<Output = [u8; N]>;
        fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N> {
            if self.1 + N <= self.0.len() {
                let offset = self.1;
                self.1+=N;
                use core::convert::TryInto;
                core::future::ready(self.0[offset..self.1].try_into().unwrap())
            } else {
                panic!("Read past end of input");
            }
        }
    }

    use core::task::*;

    static RAW_WAKER_VTABLE : RawWakerVTable = RawWakerVTable::new(|a| RawWaker::new(a, &RAW_WAKER_VTABLE), |_| {}, |_| {}, |_| {});

    fn poll_once<F: Future>(mut input: F) -> core::task::Poll<F::Output> {
        let waker = unsafe { Waker::from_raw(RawWaker::new(&(), &RAW_WAKER_VTABLE)) };
        let mut ctxd = Context::from_waker(&waker);
        let mut pinned = unsafe { core::pin::Pin::new_unchecked(&mut input) };
        pinned.as_mut().poll(&mut ctxd)
    }

    #[test]
    fn test_varint() {
        let mut input = TestReadable([0,0,0],0);
        assert_eq!(poll_once(Int32.def_parse(&mut input)), Poll::Ready(0));
        let mut input = TestReadable([255,0,0],0);
        assert_eq!(poll_once(Int32.def_parse(&mut input)), Poll::Ready(127));
        let mut input = TestReadable([254,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready(63));
        let mut input = TestReadable([255,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready(-64));
        let mut input = TestReadable([0,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready(0));
        let mut input = TestReadable([1,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready(-1));
        let mut input = TestReadable([2,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready(1));
        let mut input = TestReadable([128,128,128,128,2,0,0],0);
        assert_eq!(poll_once(Int32.def_parse(&mut input)), Poll::Ready(1<<(7*4+1)));
        let mut input = TestReadable([128,128,128,128,2,0,0],0);
        assert_eq!(poll_once(Sint32.def_parse(&mut input)), Poll::Ready((1<<(7*4+1))/2));
    }

    #[test]
    fn test_bytes() {
        let mut input = TestReadable([1,2,3,4,5],0);
        if let Poll::Ready(res) = poll_once(<Buffer<10> as LengthDelimitedParser<Bytes, TestReadable<5>>>::parse(&Buffer::<10>, &mut input, 5)) {
            assert_eq!(&res[..], &[1,2,3,4,5]);
        } else { assert_eq!(true, false) }
    }

    define_message! { OtherMessage { foo: bytes = 0 } }
    define_message! { SimpleMessage { foo: message(otherMessage) = 0, bar: enum(SimpleEnum) = 1 } }
    define_enum! { SimpleEnum { default = 0, noodle = 1 } }
    define_message! {
        SignDoc {
            body_bytes: bytes = 1,
            auth_info_bytes: bytes = 2,
            chain_id: string = 3,
            account_number: uint64 = 4
        }}

    define_message! {
        Any {
            type_url: string = 1,
            value: bytes = 2
        }
    }
    define_message! {
        TxBody {
            messages: repeated(message(Any)) = 1,
            memo: string = 2,
            timeout_height: int64 = 3,
            extension_options: repeated(message(Any)) = 1023
        }
    }

    any_of! {
        FooAnyInterp {
            TxBody: TxBody = b"some.uri.here",
            SignDoc: SignDoc = b"some.other.uri.here"
        }
    }

    #[test]
    fn test_messages() {
        let mut input = TestReadable([(0<<3)+2,2,0,1],0);
        let cell = core::cell::RefCell::new(0);
        let interp = OtherMessageInterp {
            field_foo: Action(Buffer::<5>, |a: ArrayVec<u8, 5>| { *cell.borrow_mut()+=a.len() as u64; Some(()) }),
        };
        if let Poll::Ready(res) = poll_once(interp.parse(&mut input, 4)) {
            assert_eq!(res, ());
            assert_eq!(cell.into_inner(), 2);
        } else { assert!(false, "Failed to parse") }

        let mut input = TestReadable([(1<<3)+2,2,0,1,(2<<3)+2,0,(3<<3)+2,0,(4<<3),4],0);
        let cell = core::cell::RefCell::new(0);
        let interp = SignDocInterp {
            field_body_bytes: Action(Buffer::<5>, |a: ArrayVec<u8, 5>| { *cell.borrow_mut()+=a.len() as u64; Some(()) }),
            field_auth_info_bytes: DropInterp, // Action(Buffer::<5>, |_| Some(())),
            field_chain_id: Action(Buffer::<5>, |_| Some(())),
            field_account_number: Action(DefaultInterp, |a| { *cell.borrow_mut()+=a; Some(()) })
        };
        if let Poll::Ready(res) = poll_once(interp.parse(&mut input, 10)) {
            assert_eq!(res, ());
            assert_eq!(cell.into_inner(), 6);
        } else { assert!(false, "Failed to parse") }

        let mut input = TestReadable([(1<<3)+2,2,0,1,(2<<3)+2,0,(3<<3)+2,0,(4<<3),4],0);
        let cell = core::cell::RefCell::new(0);
        let interp = BytesAsMessage(SignDoc, SignDocInterp {
            field_body_bytes: Action(Buffer::<5>, |a: ArrayVec<u8, 5>| { *cell.borrow_mut()+=a.len() as u64; Some(()) }),
            field_auth_info_bytes: DropInterp, // Action(Buffer::<5>, |_| Some(())),
            field_chain_id: Action(Buffer::<5>, |_| Some(())),
            field_account_number: Action(DefaultInterp, |a| { *cell.borrow_mut()+=a; Some(()) })
        });
        if let Poll::Ready(res) = poll_once(interp.parse(&mut input, 10)) {
            assert_eq!(res, ());
            assert_eq!(cell.into_inner(), 6);
        } else { assert!(false, "Failed to parse") }


        // Testing embedding of Message in Bytes field
        let mut input = TestReadable([(1<<3)+2,2,2,0,(2<<3)+2,0,(3<<3)+2,0,(4<<3),4],0);
        let cell = core::cell::RefCell::new(0);
        let interp = SignDocInterp {
            field_body_bytes:
                 BytesAsMessage(OtherMessage, OtherMessageInterp { field_foo: Action(Buffer::<5>, |_| { *cell.borrow_mut()+=5; Some(()) }) }),
            field_auth_info_bytes: DropInterp, // Action(Buffer::<5>, |_| Some(())),
            field_chain_id: Action(Buffer::<5>, |_| Some(())),
            field_account_number: Action(DefaultInterp, |a| { *cell.borrow_mut()+=a; Some(()) })
        };
        if let Poll::Ready(res) = poll_once(interp.parse(&mut input, 10)) {
            assert_eq!(res, ());
            assert_eq!(cell.into_inner(), 9);
        } else { assert!(false, "Failed to parse") }
    }
}

