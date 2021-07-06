
use generic_array::{ArrayLength, GenericArray};
use std::marker::PhantomData;
pub trait RV {
    type R : PartialEq + Clone + Default;
}

pub struct Byte(pub PhantomData<()>);
impl RV for Byte {
    type R = u8;
}

pub struct Array<I, N>(pub I, pub PhantomData<(I,N)>);

impl<I : RV, N : ArrayLength<I::R> > RV for Array<I, N> {
    type R = GenericArray<I::R,N>;
}

pub struct Action<I : RV, O, E> {
    pub sub: I,
    pub f: fn(&I::R) -> Result<O, E>
}

impl<I : RV, O : PartialEq + Default + Clone, E> RV for Action<I, O, E> {
    type R = O;
}

//pub struct DArray<I, N>;
//pub struct Table;
