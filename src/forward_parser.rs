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
    type State;
    fn init() -> Self::State;
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR< 'a, Self>;
}

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

pub struct ForwardArrayParserState<I : core_parsers::RV + ForwardParser, const N : usize > {
    buffer: ArrayVec<I::R, N>, //GenericArrayVec<I::R,N>,
    sub: I::State
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

macro_rules! number_parser {
    ($p:ident, $state:ident, $size:expr) => {
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

impl<I : core_parsers::RV + ForwardParser, O: Copy> ForwardParser for core_parsers::Action<I, O, OOB> {
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
                            *state = ActionState::StoredReturnValue(rv);
                            Err((Some(oob), new_chunk))
                        }
                    }
                }
            }
            ActionState::StoredReturnValue(rv) => {
                let rv_temp = *rv;
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
    use super::{ForwardParser, OOB, RX};
    use crate::core_parsers::{Byte, Array, Action};

    const fn incomplete<X>() -> RX<'static, X> {
        Err((None, &[] as _))
    }

    #[test]
    fn byte_parser() {
        let parser = Byte;
        let mut parser_state = Byte::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok((b'c', &b"heez"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b""), Err((None, &b""[..])));
    }

    #[test]
    fn array_parser() {
        let parser = Array::<_,3>(Byte);
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ch"), Err((None, &b""[..])));
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok((*b"che", &b"ez"[..])));
    }

    #[test]
    fn array_parser_second_tier() {
        let parser // : Array<Array<Byte, 2>, 3>
            = Array::<_,3>(Array::<_,2>(Byte));
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), incomplete());
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), incomplete());
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(([*b"ch",*b"ee",*b"zb"],&b"ur"[..])));
    }

    #[test]
    fn action_array_parser() {
        let parser = Array::<_,3>(Action {
            sub: Array::<_,2>(Byte),
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
            f: |_| ((), Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])))
        });
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"c"), incomplete());
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"hee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ee"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b""[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"zbur"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ur"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ur"), Ok(([(),(),()], &b"ur"[..])));
    }
}
