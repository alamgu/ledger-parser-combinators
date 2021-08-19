use crate::core_parsers::*;
// use crate::endianness::{Endianness, Convert};
use arrayvec::{ArrayVec, ArrayString};

#[derive(Debug, PartialEq)]
pub enum OOB {
    Prompt([ArrayString<128>; 2]),
    Reject
}

// None = Incomplete
type PResult<T> = Option<T>;

type RX<'a, R> = Result<(R, &'a [u8]), (PResult<OOB>, &'a [u8] )>;

// type RR<'a, I> = RX<'a, <I as core_parsers::RV>::R>;


pub trait InterpParser<P> {
    type State;
    type Returning;
    fn init(&self) -> Self::State;
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning>;
}

pub struct DefaultInterp;
pub struct SubInterp<S>(pub S);
pub struct DropInterp;

pub struct ByteState;

impl InterpParser<Byte> for DefaultInterp {
    type State = ByteState;
    type Returning = u8;
    fn init(&self) -> Self::State { Self::State {} }
    fn parse<'a, 'b>(&self, _state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        match chunk.split_first() {
            None => Err((None, chunk)),
            Some((first, rest)) => {
                Ok((*first, rest))
            }
        }
    }
}

impl InterpParser<Byte> for DropInterp {
    type State = ();
    type Returning = ();
    fn init(&self) -> Self::State { () }
    fn parse<'a, 'b>(&self, _state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        match chunk.split_first() {
            None => Err((None, chunk)),
            Some((_, rest)) => {
                Ok(((), rest))
            }
        }
    }
}

pub struct ForwardArrayParserState<I, S, const N : usize > {
    buffer: ArrayVec<I, N>,
    sub: S
}

impl<I, S : InterpParser<I>, const N : usize> InterpParser<Array< I, N>> for SubInterp<S> {
    type State = ForwardArrayParserState<<S as InterpParser<I>>::Returning, <S as InterpParser<I>>::State, N>;
    type Returning = [<S as InterpParser<I>>::Returning; N];
    fn init(&self) -> Self::State {
        Self::State { buffer: ArrayVec::<<S as InterpParser<I>>::Returning,N>::new(), sub: <S as InterpParser<I>>::init(&self.0) }
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        let mut remaining : &'a [u8] = chunk;
        while !state.buffer.is_full() {
            match self.0.parse(&mut state.sub, remaining)? {
                (ret, new_chunk) => {
                    remaining=new_chunk;
                    state.buffer.push(ret);
                    state.sub = <S as InterpParser<I>>::init(&self.0);
                }
            }
        }
        match state.buffer.take().into_inner() {
            Ok(rv) => Ok((rv, remaining)),
            Err(_) => Err((Some(OOB::Reject), remaining)) // Should be impossible, could just panic.
        }
    }
}

use crate::endianness::{Endianness, Convert};
macro_rules! number_parser {
    ($p:ident, $size:expr) => {
        impl<const E: Endianness> InterpParser<$p<E>> for DefaultInterp where <$p<E> as RV>::R : Convert<E> {
            type State = <DefaultInterp as InterpParser<Array<Byte, $size>>>::State;
            type Returning = <$p<E> as RV>::R;
            fn init(&self) -> Self::State {
                <DefaultInterp as InterpParser<Array<Byte, $size>>>::init(&DefaultInterp)
            }
            fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
                <DefaultInterp as InterpParser<Array<Byte, $size>>>::parse(&DefaultInterp, state, chunk)
                    .map(|(r, b)| (Convert::<E>::deserialize(r), b))
            }
        }
    }
}
number_parser! { U16, 2 }
number_parser! { U32, 4 }
number_parser! { U64, 8 }

pub enum ForwardDArrayParserState<N, IS, I, const M : usize > {
    Length(N),
    Elements(ArrayVec<I, M>, usize, IS),
    Done
}

use core::convert::TryFrom;
impl<N, I, S : InterpParser<I>, const M : usize> InterpParser<DArray<N, I, M> > for SubInterp<S> where
    DefaultInterp : InterpParser<N>,
    usize: TryFrom<<DefaultInterp as InterpParser<N>>::Returning>,
    <S as InterpParser<I>>::Returning: Clone{
    type State=ForwardDArrayParserState<<DefaultInterp as InterpParser<N>>::State, <S as InterpParser<I>>::State, <S as InterpParser<I>>::Returning, M>;
    type Returning = ArrayVec<<S as InterpParser<I>>::Returning, M>;
    fn init(&self) -> Self::State {
        Self::State::Length(<DefaultInterp as InterpParser<N>>::init(&DefaultInterp))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        use ForwardDArrayParserState::*;
        let mut cursor : &'a [u8] = chunk;
        loop {
            match state {
                Length(ref mut nstate) => {
                    let (len_temp, newcur) : (_, &'a [u8]) = <DefaultInterp as InterpParser<N>>::parse(&DefaultInterp, nstate, chunk)?;
                    cursor = newcur;
                    let len = <usize as TryFrom<<DefaultInterp as InterpParser<N>>::Returning>>::try_from(len_temp).or(Err((Some(OOB::Reject), newcur)))?;
                    *state = Elements(ArrayVec::new(), len, <S as InterpParser<I>>::init(&self.0));
                }
                Elements(ref mut vec, len, ref mut istate) => {
                    while vec.len() < *len {
                        match self.0.parse(istate, cursor)? {
                            (ret, new_cursor) => {
                                cursor=new_cursor;
                                vec.push(ret);
                                *istate = <S as InterpParser<I>>::init(&self.0);
                            }
                        }
                    }
                    break Ok((vec.clone(), cursor));
                }
                Done => { break Err((Some(OOB::Reject), cursor)); }
            }
        }
    }
}


impl< I, const N : usize >  InterpParser<Array<I, N>> for DefaultInterp where
    DefaultInterp : InterpParser<I> {
    type State = <SubInterp<DefaultInterp> as InterpParser<Array< I, N> >>::State;
    type Returning = <SubInterp<DefaultInterp> as InterpParser<Array< I, N> >>::Returning;
    fn init(&self) -> Self::State {
        <SubInterp<DefaultInterp> as InterpParser<Array<I, N>>>::init(&SubInterp(DefaultInterp))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        <SubInterp<DefaultInterp> as InterpParser<Array<I, N>>>::parse(&SubInterp(DefaultInterp), state, chunk)
    }
}


/*
impl< N, I, const M : usize> InterpParser<DArray<N, I, M>> for DefaultInterp where
    DefaultInterp : InterpParser<I> + InterpParser<N>, 
    usize: From<<DefaultInterp as InterpParser<N>>::Returning> {
    type State = <SubInterp<DefaultInterp> as InterpParser<DArray< N, I, M> > >::State;
    type Returning = <SubInterp<DefaultInterp> as InterpParser<DArray< N, I, M> > >::Returning;
    fn init(&self) -> Self::State {
        <SubInterp<DefaultInterp> as InterpParser<DArray<N, I, M>>>::init(&SubInterp(DefaultInterp))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        <SubInterp<DefaultInterp> as InterpParser<DArray<N, I, M>>>::parse(&SubInterp(DefaultInterp), state, chunk)
    }
}
*/


// use core::marker::PhantomData;
// pub struct ActionInterp<A, B, C, F : FnMut(<S as InterpParser<A>>::Returning) -> (B, Option<C>), S : InterpParser<A> >(F, S, PhantomData<(A,B,C)>);

pub struct ActionInterp<F, S>(pub F, pub S);

pub enum ActionState<S, O> {
    ParsingInputs(S),
    StoredReturnValue(O),
    Done
}

impl<A, R: Clone, F: Fn(&<S as InterpParser<A>>::Returning) -> (R, Option<OOB>), S : InterpParser<A>> InterpParser<A> for ActionInterp<F, S> {
    type State = ActionState< <S as InterpParser<A> >::State, R>;
    type Returning = R;
    fn init(&self) -> Self::State {
        Self::State::ParsingInputs(<S as InterpParser<A>>::init(&self.1))
    }

    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        match state {
            ActionState::ParsingInputs(ref mut sub) => match <S as InterpParser<A> >::parse(&self.1, sub, chunk)? {
                (ret, new_chunk) => {
                    match (self.0)(&ret) {
                        (rv, None) => {
                            *state = ActionState::Done;
                            Ok((rv, new_chunk))
                        }
                        (rv, Some(oob)) => {
                            *state = ActionState::StoredReturnValue(rv.clone());
                            Err((Some(oob), new_chunk))
                        }
                    }
                }
            }
            ActionState::StoredReturnValue(rv) => {
                let rv_temp = rv.clone();
                *state = ActionState::Done;
                Ok((rv_temp, chunk))
            }
            ActionState::Done => {
                Err((Some(OOB::Reject), chunk))
            }
        }
    }
}


pub struct ObserveBytes<X, F, S>(pub X, pub F, pub S);

impl<A, X : Clone, F : Fn(&mut X, &[u8])->(), S : InterpParser<A>> InterpParser<A> for ObserveBytes<X, F, S>
    {
    type State = (X, <S as InterpParser<A>>::State);
    type Returning = (X, <S as InterpParser<A>>::Returning);
    fn init(&self) -> Self::State {
        (self.0.clone(), <S as InterpParser<A>>::init(&self.2))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        let rv = <S as InterpParser<A>>::parse(&self.2, &mut state.1, chunk);
        let new_chunk = match rv { Ok((_, remaining)) => remaining, Err((_, remaining)) => remaining };
        self.1(&mut state.0, &chunk[0..chunk.len()-new_chunk.len()]);
        let (fin_rv, remaining) = rv?;
        Ok(((state.0.clone(), fin_rv), remaining))
    }
}

pub enum PairState<A, B, R> {
  First(A),
  Second(R, B),
}

impl<A : InterpParser<C>, B : InterpParser<D>, C, D> InterpParser<(C, D)> for (A, B) where
    <A as InterpParser<C>>::Returning: Clone {
    type State = PairState<<A as InterpParser<C>>::State, <B as InterpParser<D>>::State, A::Returning>;
    type Returning = (A::Returning, B::Returning);
    fn init(&self) -> Self::State {
        PairState::First(<A as InterpParser<C>>::init(&self.0))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        match state {
            PairState::First(ref mut sub) => match <A as InterpParser<C> >::parse(&self.0, sub, chunk)? {
                (ret, new_chunk) => {
                    *state = PairState::Second(ret.clone(), <B as InterpParser<D>>::init(&self.1));
                    Err((None, new_chunk))
                }
            }
            PairState::Second(a_ret, ref mut sub) => match <B as InterpParser<D> >::parse(&self.1, sub, chunk)? {
                (b_ret, new_chunk) => {
                    Ok(((a_ret.clone(), b_ret), new_chunk))
                }
            }
        }
    }
}

/*
#[macro_export]
macro_rules! def_table {
    {struct $name:ident { $($fieldName:ident : $type:ty),+ } } => 
    {
        struct $name<$($fieldName),+> {
            $($fieldName: $fieldName),+
        }
    }

    enum 
    impl<$($fieldName : InterpParser<$type>),+> InterpParser<$name<$($fieldName),+>> for $name<$($fieldName),+> {

    }
}
*/

#[cfg(test)]
mod test {

#[cfg(all(target_os="nanos", test))]
    use testmacro::test_item as test;
#[cfg(all(target_os="nanos", test))]
#[allow(unused_imports)]
    use nanos_sdk::{TestType, debug_print}; // , Pic};
#[cfg(all(not(target_os="nanos"), test))]
    fn debug_print(s: &str) {
    }

    struct DBG;
    use core;
    #[allow(unused_imports)]
    use core::fmt::Write;
    impl core::fmt::Write for DBG {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            use arrayvec::ArrayString;
            let mut qq = ArrayString::<128>::new();
            qq.push_str(s);
            debug_print(qq.as_str());
            Ok(())
        }
    }

    use core::fmt::Debug;
    use super::{InterpParser, DefaultInterp, SubInterp, ActionInterp, ObserveBytes, OOB, RX};
    #[allow(unused_imports)]
    use crate::core_parsers::{Byte, Array, DArray, U16, U32, Action }; // , RV};
    #[allow(unused_imports)]
    use arrayvec::ArrayVec;
    
    fn parser_test_feed<P, T: InterpParser<P>, RT: Debug + ?Sized>(parser: T, chunks: &[&[u8]], result: &RT, oobs: &[OOB]) where T::Returning: PartialEq<RT> + Debug
    {
        let mut oob_iter = oobs.iter();
        let mut chunk_iter = chunks.iter();
        let mut cursor : &[u8] = chunk_iter.next().unwrap();
        let mut parser_state = T::init(&parser);
        loop {
            match <T as InterpParser<P>>::parse(&parser, &mut parser_state, cursor) {
                Err((Some(o), new_cursor)) => {
                    cursor = new_cursor;
                    assert_eq!(Some(&o), oob_iter.next());
                    match o {
                        OOB::Reject => {
                            assert_eq!(oob_iter.next(), None);
                            assert_eq!(chunk_iter.next(), None);
                            break;
                        }
                        _ => {}
                    }
                }
                Err((None, new_cursor)) => {
                    assert_eq!(new_cursor, &[][..]);
                    match chunk_iter.next() {
                        Some(new) => { 
                            cursor = new;
                        }
                        None => {
                            panic!("Ran out of input chunks before parser accepted");
                        }
                    }
                }
                Ok((rv, new_cursor)) => {
                    assert_eq!(&rv, result);
                    assert_eq!(new_cursor, &[][..]);
                    assert_eq!(chunk_iter.next(), None);
                    assert_eq!(oob_iter.next(), None);
                    break;
                }
            }
        }
    }

#[test]
fn byte_parser() {
    let mut state = <DefaultInterp as InterpParser<Byte>>::init(&DefaultInterp);
    assert_eq!(<DefaultInterp as InterpParser<Byte>>::parse(&DefaultInterp, &mut state, b"cheez"), Ok((b'c', &b"heez"[..])));
    assert_eq!(<DefaultInterp as InterpParser<Byte>>::parse(&DefaultInterp, &mut state, b""), Err((None, &b""[..])));

}

    fn init_parser<A, P: InterpParser<A>>(p: &P) -> <P as InterpParser<A>>::State {
        <P as InterpParser<A>>::init(p)
    }
    fn run_parser<'a, 'b, A, P: InterpParser<A>>(p: &P, state: &'b mut <P as InterpParser<A>>::State, chunk: &'a [u8]) -> RX<'a, <P as InterpParser<A>>::Returning> {
        <P as InterpParser<A>>::parse(p, state, chunk)
    }

#[test]
fn interp_byte_parser() {
    let p = super::ActionInterp(|x: &u8| ((x.clone()), None), DefaultInterp);
    let mut state = init_parser::<Byte,_>(&p);
    assert_eq!(run_parser::<Byte,_>(&p, &mut state, b"cheez"), Ok((b'c', &b"heez"[..])));
//    let mut state = (p as dyn InterpParser<Byte>).init(&DefaultInterp);
//    assert_eq!(<DefaultInterp as InterpParser<Byte>>::parse(&DefaultInterp, &mut state, b"cheez"), Ok((b'c', &b"heez"[..])));
//    assert_eq!(<DefaultInterp as InterpParser<Byte>>::parse(&DefaultInterp, &mut state, b""), Err((None, &b""[..])));
}

#[test]
fn test_array() {
    let p = super::DefaultInterp;
    parser_test_feed::<Array<Byte,5>, super::DefaultInterp, _>(p, &[b"foo",b"ba"], &b"fooba"[..], &[])
}

#[test]
    fn test_darray() {
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(SubInterp(DefaultInterp), &[b"\0"], &b""[..], &[]);
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(SubInterp(DefaultInterp), &[b"\x05abcde"], &b"abcde"[..], &[]);
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(SubInterp(ActionInterp(|_: &u8| ((), None),DefaultInterp)), &[b"\x05abcde"], &[(),(),(),(),()][..], &[]);
        let obs = ObserveBytes(
            0, |a : &mut usize,b : &[u8]| { *a += b.len(); },
            SubInterp(
                ActionInterp(|_: &u8| ((), None),DefaultInterp)));
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(obs, &[b"\x05abcde"], &(6, (ArrayVec::from([(),(),(),(),()]))), &[]);
    }

}

