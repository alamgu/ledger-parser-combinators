use crate::core_parsers::*;
use crate::interp_parser::{SubInterp,DefaultInterp,Action, ObserveBytes, DropInterp};
use crate::endianness::{Endianness, Convert};

use core::future::Future;
use core::convert::TryInto;
use arrayvec::ArrayVec;

pub fn reject<T>() -> impl Future<Output = T> {
    // Do some out-of-band rejection thingie
    core::future::pending()
}

pub trait Readable {
    type OutFut<'a, const N: usize> : 'a + Future<Output = [u8; N]>;
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N>;
}

pub trait HasOutput<Schema> {
    type Output;
}

pub trait AsyncParser<Schema, ByteStream: Readable> : HasOutput<Schema> {
    type State<'c>: Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c>;
}

impl<T, S: HasOutput<T>, const N: usize> HasOutput<Array<T, N>> for SubInterp<S> {
    type Output = [S::Output; N];
}

impl<S, T, ByteStream: Readable, const N: usize> AsyncParser<Array<T, N>, ByteStream> for SubInterp<S> where T : DefaultArray, S: AsyncParser<T, ByteStream> {
    type State<'c> = impl Future<Output = [<S as HasOutput<T>>::Output; N]>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
        async move {
            let mut accumulator = ArrayVec::<<S as HasOutput<T>>::Output, N>::new();
            while !accumulator.is_full() {
                accumulator.push(<S as AsyncParser<T, ByteStream>>::parse(&self.0, input).await);
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

impl<ByteStream: Readable> AsyncParser<Byte, ByteStream> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
        async move {
            let [u] = input.read().await;
            return u;
        }
    }
}

pub auto trait DefaultArray { }

impl !DefaultArray for Byte { }


impl<const N: usize> HasOutput<Array<Byte, N>> for DefaultInterp {
    type Output = [u8; N];
}

// We can't overlap 
impl <const N: usize, ByteStream: Readable> AsyncParser<Array<Byte, N>, ByteStream> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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
        impl<const E: Endianness, ByteStream: Readable> AsyncParser<$p<E>, ByteStream> for DefaultInterp where <$p<E> as RV>::R : Convert<E> {
            type State<'c> = impl Future<Output = Self::Output>;
            fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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

impl<T, const N: usize, ByteStream: Readable> AsyncParser<Array<T, N>, ByteStream> for DefaultInterp where T : DefaultArray, DefaultInterp: AsyncParser<T, ByteStream> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
        <SubInterp<DefaultInterp> as AsyncParser<Array<T, N>, ByteStream>>::parse(&SubInterp(DefaultInterp), input)
    }
}

impl<N, I, S: HasOutput<I>, const M: usize> HasOutput<DArray<N, I, M>> for SubInterp<S> {
    type Output = ArrayVec<S::Output, M>;
}

impl<S, N, I, const M: usize, ByteStream: Readable> AsyncParser<DArray<N, I, M>, ByteStream> for SubInterp<S> where S: AsyncParser<I, ByteStream>, DefaultInterp: AsyncParser<N, ByteStream>, <DefaultInterp as HasOutput<N>>::Output: TryInto<usize> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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

impl<N, I, const M: usize> HasOutput<DArray<N, I, M>> for DropInterp {
    type Output = ();
}

impl<N, I, const M: usize, ByteStream: Readable> AsyncParser<DArray<N, I, M>, ByteStream> for DropInterp where DefaultInterp: AsyncParser<I, ByteStream>, DefaultInterp: AsyncParser<N, ByteStream>, <DefaultInterp as HasOutput<N>>::Output: TryInto<usize>
, DefaultInterp: AsyncParser<I, ByteStream> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
        async move {
            let length : usize = match <DefaultInterp as AsyncParser<N, ByteStream>>::parse(&DefaultInterp, input).await.try_into() {
                Ok(a) => a,
                Err(_) => reject().await,
            };
            for _ in 1..length {
                <DefaultInterp as AsyncParser<I, ByteStream>>::parse(&DefaultInterp, input).await;
            }
        }
    }
}

impl<T, S: HasOutput<T>, R> HasOutput<T> for Action<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type Output = R;
}

impl<T, S: AsyncParser<T, ByteStream>, R, ByteStream: Readable> AsyncParser<T, ByteStream> for Action<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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

impl<A, R, S : AsyncParser<A, ByteStream>, ByteStream: Readable> AsyncParser<A, ByteStream> for Action<S, fn(&<S as HasOutput<A>>::Output, &mut Option<R>) -> Option<()>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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

pub struct FAction<S, O, R>(pub S, pub fn(O) -> Option<R>);

impl<T, S: HasOutput<T>, R> HasOutput<T> for FAction<S, S::Output, R> {
    type Output = R;
}

impl<T, S: AsyncParser<T, ByteStream>, R, ByteStream: Readable> AsyncParser<T, ByteStream> for FAction<S, <S as HasOutput<T>>::Output, R> { // FAction<S, fn(<S as HasOutput<T>>::Output) -> Option<R>> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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

pub struct HashIntercept<BS, X>(pub BS, pub X);

impl<BS: 'static + Readable, X: 'static> Readable for HashIntercept<BS, X> {
    type OutFut<'a, const N: usize> = impl core::future::Future<Output = [u8; N]>;
    fn read<'a: 'b, 'b, const N: usize>(&'a mut self) -> Self::OutFut<'b, N> {
        self.0.read()
    }
}

impl<X: 'static, F, S: AsyncParser<A, HashIntercept<ByteStream, X>>, A, ByteStream: 'static + Readable + Clone> AsyncParser<A, ByteStream> for ObserveBytes<X, F, S> {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
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


impl<A, B, S: AsyncParser<A, ByteStream>, T: AsyncParser<B, ByteStream>, ByteStream: Readable> AsyncParser<(A, B), ByteStream> for (S, T) {
    type State<'c> = impl Future<Output = Self::Output>;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut ByteStream) -> Self::State<'c> {
        async move {
            let t = self.0.parse(input).await;
            let s = self.1.parse(input).await;
            (Some(t), Some(s))
        }
    }
}
