use crate::core_parsers::*;
use crate::endianness::{Endianness, Convert};
use arrayvec::ArrayVec;

#[derive(PartialEq, Debug)]
pub enum OOB {
    // Prompt removed due to excessive memory use; we gain testability improvements if we can
    // reinstate an OOB for prompts and do the co-routine again, but we can't do that at this
    // memory use.
    //
    // Prompt([ArrayString<128>;2]),
    Reject
}

// None = Incomplete
pub type PResult<T> = Option<T>;

pub type RX<'a, R> = Result<(R, &'a [u8]), (PResult<OOB>, &'a [u8] )>;

pub fn reject<'a, R>(chunk: &'a [u8]) -> Result<R, (PResult<OOB>, &'a [u8])> {
    Err((Some(OOB::Reject), chunk))
}

pub fn need_more<'a, R>(chunk: &'a [u8]) -> Result<R, (PResult<OOB>, &'a [u8])> {
    Err((None, chunk))
}

// Core trait; describes an "interpretation" of a given datatype (specified with the types from
// core_parsers), which can have a variable return type and do stateful actions.

pub trait InterpParser<P> {
    type State;
    type Returning;
    fn init(&self) -> Self::State;
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning>;
}

pub struct DefaultInterp;

pub struct SubInterp<S>(pub S);

// Structurally checks and skips the format, but consumes only the minimum memory required to do so
// and returns nothing.
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


/* // TODO: determine why this doesn't work.
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

pub struct Action<S, F>(pub S, pub F);

impl<A, R, F: Fn(&<S as InterpParser<A>>::Returning) -> Option<R>, S : InterpParser<A>> InterpParser<A> for Action<S, F> {
    type State = <S as InterpParser<A> >::State;
    type Returning = R;
    fn init(&self) -> Self::State {
        <S as InterpParser<A>>::init(&self.0)
    }

    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        let (ret, new_chunk) = self.0.parse(state, chunk)?;
        match (self.1)(&ret) {
            None => { Err((Some(OOB::Reject),new_chunk)) }
            Some(rv) => { Ok((rv, new_chunk)) }
        }
    }
}


pub struct ObserveBytes<X, F, S>(pub fn() -> X, pub F, pub S);

impl<A, X : Clone, F : Fn(&mut X, &[u8])->(), S : InterpParser<A>> InterpParser<A> for ObserveBytes<X, F, S>
    {
    type State = (X, <S as InterpParser<A>>::State);
    type Returning = (X, <S as InterpParser<A>>::Returning);
    fn init(&self) -> Self::State {
        ((self.0)(), <S as InterpParser<A>>::init(&self.2))
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
        let mut cursor = chunk;
        loop {
            match state {
                PairState::First(ref mut sub) => match <A as InterpParser<C> >::parse(&self.0, sub, cursor)? {
                    (ret, new_chunk) => {
                        *state = PairState::Second(ret.clone(), <B as InterpParser<D>>::init(&self.1));
                        cursor = new_chunk;
                    }
                }
                PairState::Second(a_ret, ref mut sub) => match <B as InterpParser<D> >::parse(&self.1, sub, cursor)? {
                    (b_ret, new_chunk) => {
                        break Ok(((a_ret.clone(), b_ret), new_chunk));
                    }
                }
            }
        }
    }
}

/*
 // TODO: handle struct-like data structures without using the pair parser above and with named
 // fields.
 //
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

pub enum LengthFallbackParserState<N, IS> {
    Length(N),
    Element(usize, usize, IS),
    Failed(usize, usize),
    Done
}

pub struct ObserveLengthedBytes<X, F, S>(pub fn() -> X, pub F, pub S);

impl<N, I, S : InterpParser<I>, X: Clone, F: Fn(&mut X, &[u8])->()> InterpParser<LengthFallback<N, I>> for ObserveLengthedBytes<X, F, S> where
    DefaultInterp : InterpParser<N>,
    usize: TryFrom<<DefaultInterp as InterpParser<N>>::Returning> {
    type State=(LengthFallbackParserState<<DefaultInterp as InterpParser<N>>::State, <S as InterpParser<I>>::State>, X);
    type Returning = (Result<<S as InterpParser<I>>::Returning, ()>, X);
    fn init(&self) -> Self::State {
        (LengthFallbackParserState::Length(<DefaultInterp as InterpParser<N>>::init(&DefaultInterp)), self.0())
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        use LengthFallbackParserState::*;
        let mut cursor : &'a [u8] = chunk;
        loop {
            break match state.0 {
                Length(ref mut nstate) => {
                    let (len_temp, newcur) : (_, &'a [u8]) = <DefaultInterp as InterpParser<N>>::parse(&DefaultInterp, nstate, cursor)?;
                    cursor = newcur;
                    let len = <usize as TryFrom<<DefaultInterp as InterpParser<N>>::Returning>>::try_from(len_temp).or(Err((Some(OOB::Reject), newcur)))?;
                    state.0 = Element(0, len, <S as InterpParser<I>>::init(&self.2));
                    continue;
                }
                Element(ref mut consumed, len, ref mut istate) => {
                    let passed_cursor = &cursor[0..core::cmp::min(cursor.len(), (len)-(*consumed))];
                    match self.2.parse(istate, passed_cursor) {
                        Ok((ret, new_cursor)) => {
                            let consumed_from_chunk = passed_cursor.len() - new_cursor.len();
                            *consumed += consumed_from_chunk;
                            self.1(&mut state.1, &cursor[0..passed_cursor.len()-new_cursor.len()]);
                            if *consumed == len {

                                Ok(((Ok(ret), state.1.clone()), &cursor[consumed_from_chunk..]))
                            } else {
                                cursor = new_cursor;
                                state.0 = Failed(*consumed, len);
                                continue;
                            }
                        }
                        Err((None, new_cursor)) => {
                            let consumed_from_chunk = passed_cursor.len() - new_cursor.len();
                            *consumed += consumed_from_chunk;
                            self.1(&mut state.1, &cursor[0..passed_cursor.len()-new_cursor.len()]);
                            if *consumed == len {
                                Ok(((Err(()), state.1.clone()), &cursor[consumed_from_chunk..]))
                            } else {
                                Err((None, &cursor[consumed_from_chunk..]))
                            }
                        }
                        Err((Some(OOB::Reject), _)) => {
                            state.0 = Failed(*consumed, len);
                            continue;
                        }
                    }
                }
                Failed(ref mut consumed, len) => {
                    use core::cmp::min;
                    let new_cursor = &cursor[min((len) - (*consumed), cursor.len())..];
                    self.1(&mut state.1, &cursor[0..cursor.len()-new_cursor.len()]);
                    if cursor.len() >= ((len) - (*consumed)) {
                        state.0 = Done;
                        Ok(((Err(()), state.1.clone()), new_cursor))
                    } else {
                        state.0 = Failed(*consumed-cursor.len(), len);
                        Err((None, new_cursor))
                    }
                }
                Done => { Err((Some(OOB::Reject), cursor)) }
            }
        }
    }
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
            #[cfg(target_os="nanos")]
            nanos_sdk::debug_print(qq.as_str());
            Ok(())
        }
    }

#[cfg(test)]
mod test {

#[cfg(all(target_os="nanos", test))]
    use testmacro::test_item as test;
#[cfg(all(target_os="nanos", test))]
#[allow(unused_imports)]
    use nanos_sdk::{TestType, debug_print}; // , Pic};
#[cfg(all(not(target_os="nanos"), test))]
    fn debug_print(_s: &str) {
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
    use super::{InterpParser, DefaultInterp, SubInterp, Action, ObserveBytes, OOB, RX};
    #[allow(unused_imports)]
    use crate::core_parsers::{Byte, Array, DArray, U16, U32 };
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
                Err((Some(o), _new_cursor)) => {
                    assert_eq!(Some(&o), oob_iter.next());
                    match o {
                        OOB::Reject => {
                            assert_eq!(oob_iter.next(), None);
                            assert_eq!(chunk_iter.next(), None);
                            break;
                        }
                        // If there are any non-Reject OOB options uncomment this.
                        // _ => {
                        //    cursor = new_cursor; // Not sure why rustc claims this is unused.
                        //}
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
    let p = super::Action(DefaultInterp, |x: &u8| Some(x.clone()));
    let mut state = init_parser::<Byte,_>(&p);
    assert_eq!(run_parser::<Byte,_>(&p, &mut state, b"cheez"), Ok((b'c', &b"heez"[..])));
}

#[test]
fn test_length_fallback() {
    type Format = super::LengthFallback<Byte, Array<Byte, 5>>;
    parser_test_feed::<Format, _, _>(super::ObserveLengthedBytes(|| { ArrayVec::<u8, 5>::new() }, |a: &mut ArrayVec<u8, 5> , b: &[u8]| { let _ = a.try_extend_from_slice(b); }, DefaultInterp), &[b"\x05fooba"], &(Ok(*(b"fooba")), (*(b"fooba")).into()), &[]);
    use crate::endianness::{Endianness};
    type Format2 = super::LengthFallback<U32< { Endianness::Little } >, Array<Byte, 5>>;
    parser_test_feed::<Format2, _, _>(super::ObserveLengthedBytes(|| { ArrayVec::<u8, 5>::new() }, |a: &mut ArrayVec<u8, 5> , b: &[u8]| { let _ = a.try_extend_from_slice(b); }, DefaultInterp), &[b"\x05\x00\x00\x00fooba"], &(Ok(*(b"fooba")), (*(b"fooba")).into()), &[]);
    parser_test_feed::<Format2, _, _>(super::ObserveLengthedBytes(|| { ArrayVec::<u8, 6>::new() }, |a: &mut ArrayVec<u8, 6> , b: &[u8]| { let _ = a.try_extend_from_slice(b); }, DefaultInterp), &[b"\x06\x00\x00\x00foobar"], &(Err(()), (*(b"foobar")).into()), &[]);
    parser_test_feed::<Format2, _, _>(super::ObserveLengthedBytes(|| { ArrayVec::<u8, 7>::new() }, |a: &mut ArrayVec<u8, 7> , b: &[u8]| { let _ = a.try_extend_from_slice(b); }, DefaultInterp), &[b"\x07\x00\x00\x00foobarb"], &(Err(()), (*(b"foobarb")).into()), &[]);
    parser_test_feed::<Format2, _, _>(super::ObserveLengthedBytes(|| { ArrayVec::<u8, 4>::new() }, |a: &mut ArrayVec<u8, 4> , b: &[u8]| { let _ = a.try_extend_from_slice(b); }, DefaultInterp), &[b"\x04\x00\x00\x00foob"], &(Err(()), (*(b"foob")).into()), &[]);
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
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(SubInterp(Action(DefaultInterp, |_: &u8| Some(()))), &[b"\x05abcde"], &[(),(),(),(),()][..], &[]);
        let obs = ObserveBytes(
            || 0, |a : &mut usize,b : &[u8]| { *a += b.len(); },
            SubInterp(
                Action(DefaultInterp, |_: &u8| Some(()))));
        parser_test_feed::<DArray<Byte,Byte,5>, _, _>(obs, &[b"\x05abcde"], &(6, (ArrayVec::from([(),(),(),(),()]))), &[]);
    }

}

