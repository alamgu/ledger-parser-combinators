




pub trait BlockParsable {
    type State;
    // Initializer just so that we can avoid handling partially-initialized stuff.
    fn init() -> Self::State;
    fn handle_block(&self, state: &mut Self::State, htl: &HostToLedger) -> Result<LedgerToHost, ()>;
}

enum LedgerToHostCmd {
    RESULT_ACCUMULATING = 0,
    RESULT_FINAL = 1,
    GET_CHUNK = 2,
    PUT_CHUNK = 3
};

enum HostToLedgerCmd {
    START = 0,
    GET_CHUNK_RESPONSE_SUCCESS = 1,
    GET_CHUNK_RESPONSE_FAILURE = 2,
    PUT_CHUNK_RESPONSE = 3,
    RESULT_ACCUMULATING_RESPONSE = 4
};


const MAXIMUM_OUT_BLOCK = 128;
/*
// Conceptually we'd like to just do this:
enum LedgerToHost {
    ResultAccumulating(ArrayVec<u8, MAXIMUM_OUT_BLOCK>),
    ResultFinal(ArrayVec<u8, MAXIMUM_OUT_BLOCK>),
    GetChunk([u8; 32]),
    PutChunk(ArrayVec<u8, MAXIMUM_OUT_BLOCK>)
}
// but that doesn't give us quite enough control to avoid doubled buffers, so instead we define a
// struct containing the wire format and make functions to set the struct to particular enum
// values.
*/

struct LedgerToHost<'a, const usize N> {
    buffer: &'a ArrayVec<u8; N>,
}

enum LedgerToHostView {

}

impl LedgerToHost {
    fn write(LedgerToHostCmd cmd, a: &[u8]) {
        self.buffer.clear();
        self.buffer[0] = cmd.into();
        self.buffer.try_extend_from_slice(a).unwrap();
    }
    fn ResultAccumulating(&mut self, a: &[u8]) {
        self.write(RESULT_ACCUMULATING, a)
    }
    fn ResultFinal(&mut self, &[u8]) {
        self.write(RESULT_FINAL, a)
        self.cmd = RESULT_FINAL;
        self.buffer.clear();
        self.buffer.try_extend_from_slice(a).unwrap();
    }
    fn GetChunk(&mut self, &[u8; 32]) {
        self.write(RESULT_ACCUMULATING, a)
        self.cmd = GET_CHUNK;
        self.buffer.clear();
        self.buffer.try_extend_from_slice(a).unwrap();
    }
    fn PutChunk(&mut self, &[u8]) {
        self.write(PUT_CHUNK, a)
    }
}


/*
 * For the host to ledger direction, we have a buffer allocated and the message will live less than
 * the buffer; we can just use references in the struct to achieve no-copy.
 */

enum HostToLedger<'a> {
    Start(&'a [u8]),
    GetChunkResponseSuccess(&'a [u8]),
    GetChunkResponseFailure,
    PutChunkResponse,
    ResultAccumulatingResponse
}

enum HostToLedgerError {
    InputTooShort,
    UnknownTag,
    ValidationError,
    IncorrectResponse
}

fn validate_chunk(hash: &[u8; 32], block: [u8]) -> Result<> {
    if ( sha256(block) == hash ) {
        Ok((&block[0..32], &block[32..]))
    } else {
        Err(HostToLedgerError::ValidationError)
    }
}

impl HostToLedger {
    fn decode(buf: &[u8]) -> Result<HostToLedger<'a>, HostToLedgerError> {
        match try_into(buf[0]) {
            HostToLedgerCmd::START => HostToLedger::Start(&buf[1..]),
            HostToLedgerCmd::GET_CHUNK_RESPONSE_SUCCESS => HostToLedger::GetChunkResponseSuccess(&buf[1..]),
            HostToLedgerCmd::GET_CHUNK_RESPONSE_FAILURE => HostToLedger::GetChunkResponseFailure,
            HostToLedgerCmd::PUT_CHUNK_RESPONSE => HostToLedger::PutChunkResponse,
            HostToLedgerCmd::RESULT_ACCUMULATING_RESPONSE => HostToLedger::ResultAccumulatingResponse
        }
    }
}

enum ValidatorState {
    Start,
    SentGetChunk([u8; 32]),
    SentPutChunk,
    SentResultAccumulating,
    SentResultFinal
}
struct Validator {
    ValidatorState state
}
impl Validator {
    fn exchange<F: FnOnce(&HostToLedger, &mut LedgerToHost) -> Result<(), a>>(&mut self, f: F, from_host: &[u8], to_host: &mut [u8]) -> Result<(), a> {
        let input = HostToLedger::decode(from_host)?;
        match self.state {
            ValidatorState::Start => { match input {
                HostToLedger::Start(_) => Ok(()),
                _ => Err(HostToLedgerError::IncorrectResponse),
            } }
            ValidatorState::SentGetChunk(ref hash) => {
                match input {
                    GetChunkResponseSuccess(ref chunk) => validate_chunk(hash, chunk),
                    GetChunkResponseFailure => Ok(()),
                    _ => Err(HostToLedgerError::IncorrectResponse),
                }
            }
            ValidatorState::SentPutChunk | input == HostToLedger::PutChunkResponse => Ok(()),
            ValidatorState::SentAccumulatingResponse | input == HostToLedger::ResultAccumulatingResponse => Ok(()),
            _ => Err(HostToLedgerError::IncorrectResponse)
        }?;
        f(input, result)?;
    }
}


struct ParseInterpAdapter<S>(S);

impl <S> BlockParseable for ParseInterpAdapter<S> {
    type State = (S::State, [u8; 32]);
    fn init() -> Self::State { (S::init(), [0; 32]) }
    fn handle_block(&self, state: &mut Self::State, htl: &HostToLedger) -> Result<LedgerToHost, ()> {
        match htl {
            Start(initialHash) => {
                *state = (S::init(), try_from(initialHash)?);
                Ok(LedgerToHost::GetChunk(initialHash))
            }
            GetChunkResponseSuccess(chunk) => {
                let (nextChunk, validated_chunk) = validate_chunk(state.1, chunk)?;
                let parse_rv = self.0.parse(&mut state.0, validated_chunk);
                match parse_rv {
                    Ok(rv) | nextChunk == [0; 32] && remainder == [] => {
                        Ok(LedgerToHost::ResultFinal(rv.clone()))
                    }
                    Err(None) | nextChunk != [0; 32] => {
                        *state.1 = try_from(nextChunk)?;
                        Ok(LedgerToHost::GetChunk(state.1))
                    }
                    _ => {
                        Err(())
                    }
                }
            }
            _ => {
                Err(())
            }
        }
    }
}

