

use generic_array::{ArrayLength, GenericArray};

mod core_parsers {
    use generic_array::{ArrayLength, GenericArray};
    use std::marker::PhantomData;
    pub trait RV {
        type R : PartialEq + Clone + Default;
    }
    #[derive(Default)]
    pub struct Byte(pub PhantomData<()>);
    impl RV for Byte {
        type R = u8;
    }
    
    #[derive(Default)]
    pub struct Array<I, N>(pub I, pub PhantomData<(I,N)>);

    impl<I : RV, N : ArrayLength<I::R> > RV for Array<I, N> {
        type R = GenericArray<I::R,N>;
    }

    //#[derive(Default)]
    pub struct Action<I : RV, O, E> {
        pub sub: I,
        pub f: fn(&I::R) -> Result<O, E>
    }

    impl<I : RV, O : PartialEq + Default + Clone, E> RV for Action<I, O, E> {
        type R = O;
    }

    //pub struct DArray<I, N>;
    //pub struct Table;
}

#[derive(PartialEq,Debug)]
pub enum PResult<'a,'b, R : PartialEq> {
    Incomplete,
    Complete(&'b R, &'a [u8])
}

type RR<'a,'b,I> = Result<PResult<'a, 'b, <I as core_parsers::RV>::R>, ()>;

pub trait ForwardParser : core_parsers::RV {
    type State;
    fn init() -> Self::State;
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR< 'a, 'b, Self>;
}

impl ForwardParser for core_parsers::Byte {
    type State=u8;
    fn init() -> u8 { 0 }
    fn parse<'a,'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, 'b, Self> {
        match chunk.split_first() {
            None => Ok(PResult::Incomplete),
            Some((first, rest)) => {
                *state = *first;
                Ok(PResult::Complete(state, rest))
            }
        }
    }
}

pub struct ForwardArrayParserState<I : core_parsers::RV + ForwardParser, N : ArrayLength<I::R> > {
    buffer: GenericArray<I::R,N>,
    fill: usize,
    sub: I::State
}

use log;
impl<I : core_parsers::RV + ForwardParser, N : ArrayLength<I::R> > ForwardParser for core_parsers::Array<I, N> {
    type State=ForwardArrayParserState<I, N>;
    fn init() -> Self::State {
        ForwardArrayParserState::<I, N>{ buffer: GenericArray::default(), fill: 0, sub: I::init() }
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, 'b, Self>{
        let mut remaining : &'a [u8] = chunk;
        let core_parsers::Array(sub_p,_) = self;
        log::debug!("Logging nothign");
        while state.fill < N::to_usize() {
            match sub_p.parse(&mut state.sub, remaining)? {
                PResult::Incomplete => return Ok(PResult::Incomplete),
                PResult::Complete(ret, new_chunk) => {
                    remaining=new_chunk;
                    state.buffer[state.fill].clone_from(ret);
                    state.fill += 1;
                    state.sub = I::init();
                }
            }
        }
        Ok(PResult::Complete(&state.buffer, remaining))
    }
}

pub struct ActionState<I : ForwardParser, O> {
    sub : I::State,
    result : O
}

impl<I : core_parsers::RV + ForwardParser, O : Clone + PartialEq + Default> ForwardParser for core_parsers::Action<I, O, ()> {
    type State = ActionState<I, O>;
    fn init() -> Self::State {
        ActionState {
            sub: I::init(),
            result: O::default()
        }
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RR<'a, 'b, Self>{
        match self.sub.parse(&mut state.sub, chunk)? {
            PResult::Incomplete => return Ok(PResult::Incomplete),
            PResult::Complete(ret, new_chunk) => {
                state.result = (self.f)(ret)?;
                Ok(PResult::Complete(&state.result, new_chunk))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::marker::PhantomData;
    use generic_array::{GenericArray};
    use super::{ForwardParser, core_parsers::{Byte, Array, Action}, PResult};
    #[test]
    fn byte_parser() {
        let parser = Byte(PhantomData);
        let mut parser_state = Byte::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok(PResult::Complete(&b'c', b"heez")));
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b""), Ok(PResult::Incomplete));
    }

    #[test]
    fn array_parser() {
        use generic_array::typenum::U3;
        let parser : Array<Byte, U3> = Array(Byte(PhantomData), PhantomData);
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"ch"), Ok(PResult::Incomplete));
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok(PResult::Complete(GenericArray::from_slice(b"che"), b"ez")));
    }
    #[test]
    fn array_parser_second_tier() {
        use generic_array::typenum::{U2,U3};
        use generic_array::arr;
        let parser : Array<Array<Byte, U2>, U3> = Array(Array(Byte(PhantomData),PhantomData),PhantomData);
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), Ok(PResult::Incomplete));
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok(PResult::Incomplete));
        parser_state = Array::init();
        let trv = arr![GenericArray<u8, U2>; GenericArray::clone_from_slice(b"ch"), GenericArray::clone_from_slice(b"ee"), GenericArray::clone_from_slice(b"zb")];
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(PResult::Complete(&trv,b"ur")));
    }
    
    #[test]
    fn action_array_parser() {
        use generic_array::typenum::{U2,U3};
        use generic_array::arr;
        let parser : Array<Action<Array<Byte, U2>, (), ()>, U3> = Array(Action {
            sub: Array(Byte(PhantomData),PhantomData),
            f: |x| Ok(())
        },PhantomData);
        let mut parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"chee"), Ok(PResult::Incomplete));
        parser_state = Array::init();
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheez"), Ok(PResult::Incomplete));
        parser_state = Array::init();
        let trv = arr![(); (), (),()];
        assert_eq!(ForwardParser::parse(&parser, &mut parser_state, b"cheezbur"), Ok(PResult::Complete(&trv,b"ur")));
    }
}
