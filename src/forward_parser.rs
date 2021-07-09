use crate::core_parsers;

use arrayvec::{ArrayVec, ArrayString};

#[derive(Debug, PartialEq)]
pub enum OOB {
    Prompt([ArrayString<128>; 2]),
    Reject
}

// None = Incomplete
type PResult<T> = Option<T>;

type RR<'a, I> = Result<(<I as core_parsers::RV>::R, &'a [u8]), (PResult<OOB>, &'a [u8] )>;


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

impl ForwardParser for core_parsers::Byte {
    type State=();
    fn init() -> () { () }
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
    type State=ForwardArrayParserState<I, N>;
    fn init() -> Self::State {
        ForwardArrayParserState::<I, N>{ buffer: ArrayVec::<I::R,N>::new(), sub: I::init() }
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

use type_equals::TypeEquals;
impl<N : core_parsers::RV + ForwardParser, I : core_parsers::RV + ForwardParser, const M : usize > ForwardParser for core_parsers::DArray<N, I, M> where <I as core_parsers::RV>::R: Copy, <N as core_parsers::RV>::R: TypeEquals<Other = usize> {
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
                Length(nstate) => {
                    let (len, newcur) : (usize, &'a [u8]) = number_parser.parse(&mut nstate, chunk)?;
                    cursor = newcur;
                    *state = Elements(ArrayVec::new(), len, I::init());
                }
                Elements(vec, len, istate) => {
                    break Err((Some(OOB::Reject), cursor));
                }
                Done => { break Err((Some(OOB::Reject), cursor)); }
            }
        }
        /*
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
        */
    }
}
pub enum ActionState<I : ForwardParser, O> {
    ParsingInputs(I::State),
    StoredReturnValue(O),
    Done
}

impl<I : core_parsers::RV + ForwardParser, O: Copy> ForwardParser for core_parsers::Action<I, O, OOB> {
    type State = ActionState<I, O>;
    fn init() -> Self::State {
        ActionState::ParsingInputs(I::init())
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
    use crate::forward_parser::{ForwardParser,OOB};
    use crate::core_parsers::{Byte, Array, Action};
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
        let incomplete = Err((None, &b""[..]));
        let parser // : Array<Array<Byte, 2>, 3> 
            = Array::<_,3>(Array::<_,2>(Byte));
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), incomplete);
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), incomplete);
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(([*b"ch",*b"ee",*b"zb"],&b"ur"[..])));
    }
    
    #[test]
    fn action_array_parser() {
        let incomplete = Err((None, &b""[..]));
        let parser = Array::<_,3>(Action {
            sub: Array::<_,2>(Byte),
            f: |_| ((), None)
        });
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), incomplete);
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), incomplete);
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(([(),(),()],&b"ur"[..])));
    }

    #[test]
    fn action_array_parser_oob() {
        use arrayvec::ArrayString;
        let incomplete = Err((None, &b""[..]));
        let parser = Array::<_,3>(Action {
            sub: Array::<_,2>(Byte),
            f: |_| ((), Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])))
        });
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"c"), incomplete);
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"hee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ee"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ee"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b""[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"zbur"), Err((Some(OOB::Prompt([ArrayString::new(),ArrayString::new()])), &b"ur"[..])));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ur"), Ok(([(),(),()], &b"ur"[..])));
    }
}
