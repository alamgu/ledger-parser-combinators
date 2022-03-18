




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
enum LedgerToHost {
    ResultAccumulating(ArrayVec<u8, MAXIMUM_OUT_BLOCK>),
    ResultFinal(ArrayVec<u8, MAXIMUM_OUT_BLOCK>),
    GetChunk([u8; 32]),
    PutChunk(ArrayVec<u8, MAXIMUM_OUT_BLOCK>)
}

enum HostToLedger<'a> {
    Start(&'a [u8]),
    GetChunkResponseSuccess(&'a [u8]),
    GetChunkResponseFailure,
    PutChunkResponse,
    ResultAccumulatingResponse
}

fn validate_chunk(hash: &[u8; 32], block: [u8]) {
    if ( sha256(block) == hash ) {
        Ok((&block[0..32], &block[32..]))
    } else {
        Err(())
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

