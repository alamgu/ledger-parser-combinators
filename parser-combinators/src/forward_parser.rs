use crate::core_parsers;

use crate::endianness::{Endianness, Convert};

use arrayvec::{ArrayVec, ArrayString};

#[derive(Debug, PartialEq)]
pub enum OOB {
    Prompt([ArrayString<128>; 2]),
    Reject
}

// None = Incomplete
type PResult<T> = Option<T>;

type RX<'a, R> = Result<(R, &'a [u8]), (PResult<OOB>, &'a [u8] )>;

type RR<'a, I> = RX<'a, <I as core_parsers::RV>::R>;


/*
// Would rather do:
struct RR<'a, I>(Result<<I as core_parsers::RV>::R, PResult<OOB>>, &'a [u8]);
#[feature(try_trait)]
use std::ops::Try;
#[feature(try_trait)]
impl Try for RR<'a, I> {
    type Ok = (<I as core_parsers::RV>::R, &'a [u8]);
    type Error = (PResult<OOB>, &'a [u8]);
    pub fn into_result(&self) = {
        match self {
            RR(Ok(a),b) => Ok((a,b)),
            RR(Err(a),b) => Err((a,b))
        }
    }
    pub fn from_error((a,b)) = RR(Err(a),b);
    pub fn from_ok((a,b)) = RR(ok(a),b);
}
*/

pub trait ForwardParser : core_parsers::RV {
    type State : Default;
    fn init() -> Self::State;
    fn init_method(&self) -> Self::State { Self::init() }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR< 'a, Self>;
}

#[derive(Default)]
pub struct ByteState;

impl ForwardParser for core_parsers::Byte {
    type State = ByteState;
    fn init() -> Self::State { Self::State {} }
    fn parse<'a,'b>(&self, _state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, Self> {
        match chunk.split_first() {
            None => Err((None, chunk)),
            Some((first, rest)) => {
                Ok((*first, rest))
            }
        }
    }
}

// #[derive(Default)]
pub struct ForwardArrayParserState<I : core_parsers::RV + ForwardParser, const N : usize > {
    buffer: ArrayVec<I::R, N>, //GenericArrayVec<I::R,N>,
    sub: I::State
}


impl <I: ForwardParser, const N: usize> Default for ForwardArrayParserState<I,N> {

    fn default() -> Self {
        Self { buffer: ArrayVec::<I::R,N>::new(), sub: I::init() }
    }
}


impl<I : core_parsers::RV + ForwardParser, const N : usize > ForwardParser for core_parsers::Array<I, N> where <I as core_parsers::RV>::R: Copy {
    type State = ForwardArrayParserState<I, N>;
    fn init() -> Self::State {
        Self::State { buffer: ArrayVec::<I::R,N>::new(), sub: I::init() }
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, Self>{
        let mut remaining : &'a [u8] = chunk;
        let core_parsers::Array(sub_p) = self;
        while !state.buffer.is_full() {
            match sub_p.parse(&mut state.sub, remaining)? {
                (ret, new_chunk) => {
                    remaining=new_chunk;
                    state.buffer.push(ret);
                    state.sub = I::init();
                }
            }
        }
        match state.buffer.take().into_inner() {
            Ok(rv) => Ok((rv, remaining)),
            Err(_) => Err((Some(OOB::Reject), remaining)) // Should be impossible, could just panic.
        }
    }
}

pub enum ForwardDArrayParserState<N : core_parsers::RV + ForwardParser, I : core_parsers::RV + ForwardParser, const M : usize > {
    Length(N::State),
    Elements(ArrayVec<I::R, M>, usize, I::State),
    Done
}
impl<N: ForwardParser,I: ForwardParser, const M : usize> Default for ForwardDArrayParserState<N, I, M> {
    fn default() -> Self {
        Self::Length(N::init())
    }
}

use core::convert::TryInto;
impl<N : ForwardParser, I : core_parsers::RV + ForwardParser, const M : usize > ForwardParser for core_parsers::DArray<N, I, M> where <I as core_parsers::RV>::R: Copy, N::R: TryInto<usize> {
    type State=ForwardDArrayParserState<N, I, M>;
    fn init() -> Self::State {
        ForwardDArrayParserState::<N, I, M>::Length(N::init())
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, Self>{
        let core_parsers::DArray(number_parser, item_parser) = self;
        use ForwardDArrayParserState::*;
        let mut cursor : &'a [u8] = chunk;
        loop {
            match state {
                Length(ref mut nstate) => {
                    let (len_temp, newcur) : (_, &'a [u8]) = number_parser.parse(nstate, chunk)?;
                    cursor = newcur;
                    let len = len_temp.try_into().or(Err((Some(OOB::Reject), newcur)))?;
                    *state = Elements(ArrayVec::new(), len, I::init());
                }
                Elements(ref mut vec, len, ref mut istate) => {
                    while vec.len() < *len {
                        match item_parser.parse(istate, cursor)? {
                            (ret, new_cursor) => {
                                cursor=new_cursor;
                                vec.push(ret);
                                *istate = I::init();
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

macro_rules! number_parser {
    ($p:ident, $state:ident, $size:expr) => {
        #[derive(Default)]
        pub struct $state<const E : Endianness>(ForwardArrayParserState<core_parsers::Byte, $size>);

        impl<const E : Endianness> ForwardParser for core_parsers::$p<E> where Self::R : Convert::<E> {
            type State = $state<E>;
            fn init() -> Self::State {
                // Can't use associate name here so I guess we'll have to fall back on macro param instead
                //<Self as ForwardParser>::State { 0: core_parsers::Array::<core_parsers::Byte, $size>::init() }
                $state::<E>(core_parsers::Array::<core_parsers::Byte, $size>::init())
            }
            fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, Self> {
                core_parsers::Array::<core_parsers::Byte, $size>(core_parsers::Byte)
                    .parse(&mut state.0, chunk)
                    .map(|(r, b)| (Convert::<E>::deserialize(r), b))
            }
        }
    }
}

number_parser! { U16, ForwardU16ParserState, 2 }
number_parser! { U32, ForwardU32ParserState, 4 }
number_parser! { U64, ForwardU64ParserState, 8 }

pub enum ActionState<I : ForwardParser, O> {
    ParsingInputs(I::State),
    StoredReturnValue(O),
    Done
}
impl<I : ForwardParser, O> Default for ActionState<I,O> {
    fn default() -> Self {
        Self::ParsingInputs(I::init())
    }
}

impl<I : core_parsers::RV + ForwardParser, O: Clone, F: Fn(&I::R) -> (O, Option<OOB>)> ForwardParser for core_parsers::Action<I, O, OOB, F> {
    type State = ActionState<I, O>;
    fn init() -> Self::State {
        Self::State::ParsingInputs(I::init())
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, Self>{
        match state {
            ActionState::ParsingInputs(ref mut sub) => match self.sub.parse(sub, chunk)? {
                (ret, new_chunk) => {
                    match (self.f)(&ret) {
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


#[cfg(test)]
mod tests {

#[cfg(all(target_family="bolos", test))]
    use testmacro::test_item as test;
#[cfg(all(target_family="bolos", test))]
    use nanos_sdk::{TestType, debug_print};
#[cfg(all(not(target_family="bolos"), test))]
    fn debug_print(s: &str) {
    }

    use core::fmt::Debug;
    use super::{ForwardParser, OOB, RX};
    use crate::core_parsers::{Byte, Array, DArray, U16, U32, Action };
    use arrayvec::ArrayVec;

    const fn incomplete<X>() -> RX<'static, X> {
        Err((None, &[]))
    }

    #[test]
    fn byte_parser() {
        let parser : Byte = Default::default();
        let mut parser_state = Byte::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok((b'c', &b"heez"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b""), Err((None, &b""[..])));
    }

    #[test] // Passes
    fn array_parser() {
        let parser : Array<Byte, 3> = Default::default();
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ch"), Err((None, &b""[..])));
    }

    struct DBG;
    use core;
    impl core::fmt::Write for DBG {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            debug_print(s);
            Ok(())
        }
    }
    
    #[test]
    pub fn test_write() {
        use core::fmt::Write;
        /*
        debug_print("DEBUG\n");
        // let mut s = arrayvec::ArrayString::<20>::new();
        let mut s = [0 as u8; 20];
        debug_print("DEBUG\n");
        // write!(&mut s[..], "hello"); // Segfault.
        let num = unsafe { 0xc0d00000 as *const () };
        if( unsafe { foo as *const str as *const () } > num ) { 
            debug_print("Pointer to constants\n");
        } else {
            debug_print("Pointer smaller.\n");
        }*/
        let foo = "foo";
        // write!(DBG, "{:p}", (foo as *const str)); // , unsafe { "hello\n" });
        write!(DBG, "{}\n", foo); // , unsafe { "hello\n" });
    }

    #[test]
    fn array_parser_2() {
        let parser : Array<Byte, 3> = Default::default();
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ch"), Err((None, &b""[..])));
        parser_state = Array::init();
        // let mut parser_state2 = Array::init();
        // assert_eq!(ForwardParser::parse(&parser, &mut parser_state2, b"ch"), Err((None, &b""[..])));
        // assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok((*b"che", &b"ez"[..])));
    }
    
    #[test] // Segfaults
    pub fn array_parser_3() {
        {
        let parser = Array::<_,3>(Byte);
        let mut parser_state = parser.init_method();
        // ForwardParser::parse(&parser, &mut parser_state, b"ch");
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ch"), Err((None, &b""[..])));
        }
        {
        let parser_2 = Array::<_,3>(Byte);
        let mut parser_state_2 = parser_2.init_method();
        //let rv = ForwardParser::parse(&parser_2, &mut parser_state_2, b"ch");
        //if(rv != Err((None, &b""[..]))) {
        //    panic!("Failed");
        //}
        assert_eq!(ForwardParser::parse(&parser_2, &mut parser_state_2, b"ch"), Err((None, &b""[..])));
        }
    }

    #[test]
    fn array_parser_second_tier() {
        let parser : Array<Array<Byte, 2>, 3> = Default::default();
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), incomplete());
        //parser_state = Array::init();
        //assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), incomplete());
        //parser_state = Array::init();
        //assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(([*b"ch",*b"ee",*b"zb"],&b"ur"[..])));
    }

    #[test]
    fn action_array_parser() {
        let parser = Array::<_,3>(Action {
            sub: Array::<_,2>(Byte::default()),
            f: |_| ((), None)
        });
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), incomplete());
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), incomplete());
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(([(),(),()],&b"ur"[..])));
    }

    #[test]
    fn action_array_parser_oob() {
        use arrayvec::ArrayString;
        let parser = Array::<_,3>(Action {
            sub: Array::<_,2>(Byte),
            f: |_| ((), Some(OOB::Prompt([ArrayString::new(), ArrayString::new()])))
        });
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"c"), incomplete());
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"hee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ee"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b""[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"zbur"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ur"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ur"), Ok(([(),(),()], &b"ur"[..])));
    }

    /*
    struct PT;
    
    trait ParserTest;

    impl<T: ForwardParser, RT: Debug + PartialEq<T::R> = <T as RV>::R> ParserTest for PT{
    */

    fn parser_test_feed<T: ForwardParser, RT: Debug + ?Sized>(parser: T, chunks: &[&[u8]], result: &RT, oobs: &[OOB]) where T::R: PartialEq<RT> + Debug
    {
        let mut oob_iter = oobs.iter();
        let mut chunk_iter = chunks.iter();
        let mut cursor : &[u8] = chunk_iter.next().unwrap();
        let mut parser_state = T::init();
        loop {
            match parser.parse(&mut parser_state, cursor) {
                Err((Some(o), newCursor)) => {
                    cursor = newCursor;
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
                Err((None, newCursor)) => {
                    assert_eq!(newCursor, &[][..]);
                    match chunk_iter.next() {
                        Some(new) => { 
                            cursor = new;
                        }
                        None => {
                            panic!("Ran out of input chunks before parser accepted");
                        }
                    }
                }
                Ok((rv, newCursor)) => {
                    assert_eq!(&rv, result);
                    assert_eq!(newCursor, &[][..]);
                    assert_eq!(chunk_iter.next(), None);
                    assert_eq!(oob_iter.next(), None);
                    break;
                }
            }
            break;
        }
    }
    // }

    use crate::endianness::Endianness;
    use arrayvec::ArrayString;
    use core::fmt::Write;

    fn prompt(a: &str, b: &str) -> OOB {
        OOB::Prompt([ArrayString::from(a).unwrap(), ArrayString::from(b).unwrap()])
    }

    #[test]
    fn test_darray() {
        let parser = DArray::<_,_,5>(Byte, Byte);
        parser_test_feed(parser, &[b"\0"], &b""[..], &[]);
        let parser = DArray::<_,_,5>(Byte, Byte);
        parser_test_feed(parser, &[b"\x05abcde"], &b"abcde"[..], &[]);
        let parser = DArray::<_,_,10>(Byte, U32::< {Endianness::Big }>);
        parser_test_feed(parser, &[&[4, 0,0,0,0, 0,0,0,1, 0,0,1,0, 0,1,0,0]], &[0,1,256,65536][..], &[]);
        let parser = DArray::<_,_,10>(Byte, U32::< {Endianness::Little }>);
        parser_test_feed(parser, &[&[4, 0,0,0,0, 1,0,0,0, 0,1,0,0, 0,0,1,0]], &[0,1,256,65536][..], &[]);

        let parser = Action {
            f: |x: &ArrayVec<_,10>| -> (u32, Option<OOB>) { (x.iter().sum(), None) },
            sub: (DArray::<_,_,10>(U16::< {Endianness::Little } >, U32::<{Endianness::Big}>)),
        };
        parser_test_feed(parser, &[&[2,0, 0,0,1,1, 0,1,2,1]], &66306, &[]);
        
        let parser = Action {
            f: |x: &ArrayVec<_,10>| -> (u32, Option<OOB>) { 
                let title = ArrayString::from("Sum is:").unwrap();
                let mut buf = ArrayString::<128>::new();
                let theSum = x.iter().sum();
                write!(&mut buf, "{}", theSum);
                (theSum, Some(OOB::Prompt([title, buf])))
            },
            sub: (DArray::<_,_,10>(Action {
                sub: U16::< {Endianness::Little } >,
                f: |x| {
                    let mut buf = ArrayString::<128>::new();
                    write!(&mut buf, "Summing {} elements", x);
                    (*x, Some(OOB::Prompt([buf, ArrayString::from("pong").unwrap()])))
                }
            }, U32::<{Endianness::Big}>)),
        };
        parser_test_feed(parser, &[&[2,0, 0,0,1,1, 0,1,2,1]], &66306, 
                         &[
                         prompt("Summing 2 elements", "pong"),
                         prompt("Sum is:", "66306")
                         ]);
    }
}
