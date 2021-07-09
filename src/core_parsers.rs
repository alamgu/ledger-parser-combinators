use crate::endianness::Endianness;

// use generic_array::{ArrayLength, GenericArray};
pub trait RV {
    type R;
}

pub struct Byte;
impl RV for Byte {
    type R = u8;
}

pub struct Array<I, const N : usize>(pub I);

impl< I : RV, const N : usize > RV for Array<I, N> {
    type R = [I::R; N];
}

macro_rules! number_parser {
    ($p:ident, $t:ty) => {
        pub struct $p<const E : Endianness>;
        impl<const E: Endianness> RV for $p<E> {
            type R = $t;
        }
    }
}

number_parser! { U16, u16 }
number_parser! { U32, u32 }
number_parser! { U64, u64 }

//pub enum OutOfBand {
//    Prompt('a mut dyn Fn() -> usize),
//}

pub struct Action<I : RV, O, A> {
    pub sub: I,
    pub f: fn(&I::R) -> (O, Option<A>)
}

impl<I : RV, O, A> RV for Action<I, O, A> {
    type R = O;
}

//pub struct DArray<I, N>;
//pub struct Table;
