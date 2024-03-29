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
pub struct Array<I, const N: usize>(pub I);

impl<I: RV, const N: usize> RV for Array<I, N> {
    type R = [I::R; N];
}

pub struct DArray<N, I, const M: usize>(pub N, pub I);

use arrayvec::ArrayVec;
use core::convert::TryInto;
impl<N: RV, I: RV, const M: usize> RV for DArray<N, I, M>
where
    <N as RV>::R: TryInto<usize>,
{
    type R = ArrayVec<I::R, M>;
}

macro_rules! number_parser {
    ($p:ident, $t:ty) => {
        #[derive(Default)]
        pub struct $p<const E: Endianness>;

        impl<const E: Endianness> RV for $p<E> {
            type R = $t;
        }
    };
}

number_parser! { U16, u16 }
number_parser! { U32, u32 }
number_parser! { U64, u64 }

//pub enum OutOfBand {
//    Prompt('a mut dyn Fn() -> usize),
//}

pub struct NOf<I, N>(pub N, pub I);

impl<I: RV, N: RV> RV for NOf<I, N>
where
    <I as RV>::R: TryInto<()>,
    <N as RV>::R: TryInto<usize>,
{
    type R = ();
}

//pub struct DArray<I, N>;
//pub struct Table;

pub struct LengthFallback<N, S>(pub N, pub S);

pub struct Alt<A, B>(pub A, pub B);
