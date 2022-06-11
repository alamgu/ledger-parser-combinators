//! Parser implmementation using Futures.
//!
//! The main entry point of this module is [AsyncParser].
//!
//! Note: Currently, all of the parsers in this module are implemented using async blocks, but it is
//! worth remembering that if for some reason one of them is exhibiting poor stack / state
//! behavior, we _can_ manually construct a state type and directly implement Future for that
//! state.
//!
use crate::schema::*;
use crate::interp::{SubInterp,DefaultInterp,Action, ObserveBytes, DropInterp};
use crate::endianness::{Endianness, Convert};

use core::future::Future;
use core::convert::TryInto;
use arrayvec::ArrayVec;

pub trait HasDefParser<BS: Readable> where DefaultInterp: HasOutput<Self> {
    fn def_parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c>;

    /// Type synonym for the future returned by this parser.
    type State<'c>: Future<Output = <DefaultInterp as HasOutput<Self>>::Output>;
}

impl<T, BS: Readable> HasDefParser<BS> for T where DefaultInterp: AsyncParser<T, BS> {
    fn def_parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        <DefaultInterp as AsyncParser<T, BS>>::parse(&DefaultInterp, input)
    }
    type State<'c> = impl Future<Output = <DefaultInterp as HasOutput<Self>>::Output>;
}

/// Reject the parse.
pub fn reject<T>() -> impl Future<Output = T> {
    // Do some out-of-band rejection thingie
    core::future::pending()
}

/// Readable defines an interface for input to a parser.
pub trait Readable {
    /// Type alias for the future type of read
    type OutFut<'a, const N: usize> : 'a + Future<Output = [u8; N]>;
    /// read N bytes from this Readable; returns a future that will complete with a byte array of
    /// the result.
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N>;
}

pub trait HasOutput<Schema: ?Sized> {
    type Output;
}

/// Core trait; parser that consumes a Readable according to the Schema and returns a future returning the result type.
///
///
pub trait AsyncParser<Schema, BS: Readable> : HasOutput<Schema> {
    /// Parse input, returning a future that returns this parser's return type for this schema.
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c>;

    /// Type synonym for the future returned by this parser.
    type State<'c>: Future<Output = Self::Output>;

}

impl<T, S: HasOutput<T>, const N: usize> HasOutput<Array<T, N>> for SubInterp<S> {
    type Output = [S::Output; N];
}

impl<S, T, BS: Readable, const N: usize> AsyncParser<Array<T, N>, BS> for SubInterp<S> where T : DefaultArray, S: AsyncParser<T, BS> {
    type State<'c> = impl Future<Output = [<S as HasOutput<T>>::Output; N]>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut accumulator = ArrayVec::<<S as HasOutput<T>>::Output, N>::new();
            while !accumulator.is_full() {
                accumulator.push(<S as AsyncParser<T, BS>>::parse(&self.0, input).await);
            }
            match accumulator.take().into_inner() {
                Ok(rv) => rv,
                _ => reject().await,
            }
        }
    }
}

impl HasOutput<Byte> for DefaultInterp {
    type Output = u8;
}

impl<BS: Readable> AsyncParser<Byte, BS> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let [u] = input.read().await;
            return u;
        }
    }
}

/// DefaultArray marks array element types where there is no specialization for the array type.
///
/// This allows trait selection to operate with positive reasoning except for
/// where we implement !DefaultArray explicitly for particular types, such as Byte.
pub auto trait DefaultArray { }

impl !DefaultArray for Byte { }

/// We can implement DefaultInterp for [u8; N] becuase Byte has !DefaultArray.
impl<const N: usize> HasOutput<Array<Byte, N>> for DefaultInterp {
    type Output = [u8; N];
}

/// We can implement DefaultInterp for [u8; N] becuase Byte has !DefaultArray.
///
/// Overlap is not permitted, but this doesn't overlap because we've disabled the default
/// implementation.
impl <const N: usize, BS: Readable> AsyncParser<Array<Byte, N>, BS> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            input.read().await
        }
    }
}

macro_rules! number_parser {
    ($p:ident, $size:expr, $t:ty) => {
        impl<const E: Endianness> HasOutput<$p<E>> for DefaultInterp {
            type Output = $t;
        }
        impl<const E: Endianness, BS: Readable> AsyncParser<$p<E>, BS> for DefaultInterp where $t : Convert<E> {
            type State<'c> = impl Future<Output = Self::Output>;
            fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
                async move {
                    Convert::<E>::deserialize(input.read::<$size>().await)
                }
            }
        }
    }
}
number_parser! { U16, 2, u16 }
number_parser! { U32, 4, u32 }
number_parser! { U64, 8, u64 }

impl<T, const N: usize> HasOutput<Array<T, N>> for DefaultInterp where T : DefaultArray, DefaultInterp: HasOutput<T> {
    type Output = [<DefaultInterp as HasOutput<T>>::Output; N];
}

impl<T, const N: usize, BS: Readable> AsyncParser<Array<T, N>, BS> for DefaultInterp where T : DefaultArray, DefaultInterp: AsyncParser<T, BS> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        <SubInterp<DefaultInterp> as AsyncParser<Array<T, N>, BS>>::parse(&SubInterp(DefaultInterp), input)
    }
}

impl<N, I, S: HasOutput<I>, const M: usize> HasOutput<DArray<N, I, M>> for SubInterp<S> {
    type Output = ArrayVec<S::Output, M>;
}

impl<S, N, I, const M: usize, BS: Readable> AsyncParser<DArray<N, I, M>, BS> for SubInterp<S> where S: AsyncParser<I, BS>, DefaultInterp: AsyncParser<N, BS>, <DefaultInterp as HasOutput<N>>::Output: TryInto<usize> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let length : usize = match DefaultInterp.parse(input).await.try_into() {
                Ok(a) => a,
                Err(_) => reject().await,
            };
            let mut accumulator = ArrayVec::new();
            for _ in 1..length {
                accumulator.push(self.0.parse(input).await);
            }
            accumulator
        }
    }
}

impl<Schema> HasOutput<Schema> for DropInterp {
    type Output = ();
}

impl<N, I, const M: usize, BS: Readable> AsyncParser<DArray<N, I, M>, BS> for DropInterp where DefaultInterp: AsyncParser<I, BS>, DefaultInterp: AsyncParser<N, BS>, <DefaultInterp as HasOutput<N>>::Output: TryInto<usize>
, DefaultInterp: AsyncParser<I, BS> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let length : usize = match <DefaultInterp as AsyncParser<N, BS>>::parse(&DefaultInterp, input).await.try_into() {
                Ok(a) => a,
                Err(_) => reject().await,
            };
            for _ in 1..length {
                <DefaultInterp as AsyncParser<I, BS>>::parse(&DefaultInterp, input).await;
            }
        }
    }
}

impl<T, S: HasOutput<T>, R> HasOutput<T> for Action<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type Output = R;
}

impl<T, S: AsyncParser<T, BS>, R, BS: Readable> AsyncParser<T, BS> for Action<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            match self.1(self.0.parse(input).await) {
                Some(a) => a,
                None => reject().await,
            }
        }
    }
}

impl<A, R, S, SR> HasOutput<A> for Action<S, fn(&SR, &mut Option<R>) -> Option<()>> {
    type Output = R;
}

impl<A, R, S : AsyncParser<A, BS>, BS: Readable> AsyncParser<A, BS> for Action<S, fn(&<S as HasOutput<A>>::Output, &mut Option<R>) -> Option<()>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut destination = None;
            match self.1(&self.0.parse(input).await, &mut destination) {
                Some(()) => { match destination {
                    Some(a) => a,
                    None => reject().await,
                } }
                None => reject().await,
            }
        }
    }
}

/// Slightly different version of Action; present for reasons of impl selection.
pub struct FAction<S, O, R>(pub S, pub fn(O) -> Option<R>);

impl<T, S: HasOutput<T>, R> HasOutput<T> for FAction<S, S::Output, R> {
    type Output = R;
}

impl<T, S: AsyncParser<T, BS>, R, BS: Readable> AsyncParser<T, BS> for FAction<S, <S as HasOutput<T>>::Output, R> { // FAction<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            match self.1(self.0.parse(input).await) {
                Some(a) => a,
                None => reject().await,
            }
        }
    }
}

impl<A, X, F, S: HasOutput<A>> HasOutput<A> for ObserveBytes<X, F, S> {
    type Output = (X, Option<<S as HasOutput<A>>::Output>);
}

/// HashIntercept wraps a Readable and updates a hash-like object with the data as it passes.
///
/// Used to support ObserveBytes for async parsers.
pub struct HashIntercept<BS, X>(pub BS, pub X);

impl<BS: 'static + Readable, X: 'static> Readable for HashIntercept<BS, X> {
    type OutFut<'a, const N: usize> = impl core::future::Future<Output = [u8; N]>;
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N> {
        self.0.read()
    }
}

/// ObserveBytes for AsyncParser operates by passing a HashIntercept to the sub-parser.
impl<X: 'static, F, S: AsyncParser<A, HashIntercept<BS, X>>, A, BS: 'static + Readable + Clone> AsyncParser<A, BS> for ObserveBytes<X, F, S> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut hi = HashIntercept(input.clone(), (self.0)());
            let rv = self.2.parse(&mut hi).await;
            *input = hi.0;
            (hi.1, Some(rv))
        }
    }
}

impl<A, B, S: HasOutput<A>, T: HasOutput<B>> HasOutput<(A, B)> for (S, T) {
    type Output = (Option<S::Output>, Option<T::Output>);
}

/// Pairs of parsers parse the sequence of their two schemas.
impl<A, B, S: AsyncParser<A, BS>, T: AsyncParser<B, BS>, BS: Readable> AsyncParser<(A, B), BS> for (S, T) {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let t = self.0.parse(input).await;
            let s = self.1.parse(input).await;
            (Some(t), Some(s))
        }
    }
}
