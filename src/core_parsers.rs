use crate::endianness::Endianness;

// use generic_array::{ArrayLength, GenericArray};
pub trait RV {
    type R;
}

#[derive(Default)]
pub struct Byte;
impl RV for Byte {
    type R = u8;
}

#[derive(Default)]
pub struct Array<I, const N : usize>(pub I);

impl< I : RV, const N : usize > RV for Array<I, N> {
    type R = [I::R; N];
}

macro_rules! number_parser {
    ($p:ident, $t:ty) => {

        #[derive(Default)]
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

pub struct Action<I : RV, O, A, F: Fn(&I::R) -> (O, Option<A>)> {
    pub sub: I,
    pub f: F,
}

impl<I : RV, O, A, F: Fn(&I::R) -> (O, Option<A>)> RV for Action<I, O, A, F> {
    type R = O;
}

//pub struct DArray<I, N>;
//pub struct Table;
