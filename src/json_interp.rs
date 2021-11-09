// use crate::core_parsers::*;
// use crate::json::*;
pub use paste::paste;
pub use bstringify::bstringify;
use crate::json::*;
use crate::interp_parser::*;
use core::fmt::Write;

#[cfg(all(target_os="nanos", test))]
    use testmacro::test_item as test;
#[cfg(all(target_os="nanos", test))]
#[allow(unused_imports)]
    use nanos_sdk::{TestType}; // , Pic};

#[derive(Clone, Copy, Debug)]
pub enum JsonToken<'a> {
    BeginArray,
    EndArray,
    BeginObject,
    EndObject,
    NameSeparator,
    ValueSeparator,
    BeginString,
    StringChunk(&'a [u8]),
    EndString,
    BeginNumber,
    NumberChunk(&'a [u8]),
    EndNumber,
    False,
    Null,
    True
}


pub trait JsonInterp<S> {
    type State;
    type Returning;
    fn init(&self) -> Self::State;
    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>>;
}

#[derive(Debug)]
pub enum JsonStringEscapeState {
    NotEscaped,
    FirstEscape,
    InSequence(u8)
}

#[derive(Debug)]
pub enum JsonTokenizerState {
    Value,
    InNumber,
    InString(JsonStringEscapeState),
    InTrue(u8),
    InNull(u8),
    InFalse(u8),
}

// JSON lexer
fn get_json_token<'a>(state: &mut JsonTokenizerState, chunk: &'a [u8]) -> Result<(JsonToken<'a>, RemainingSlice<'a>), (PResult<OOB>, RemainingSlice<'a>)> {
    let mut cursor = chunk;
    loop {
        match state {
            JsonTokenizerState::Value => {
                break match cursor.split_first() {
                    Some((a, tail)) => {
                        let c = match char::from_u32(*a as u32) {
                            Some(c) => { c }
                            None => { break reject(cursor); }
                        };
                        if matches!(c, '\t' | '\n' | '\r' | ' ') {
                            cursor = tail;
                            continue;
                        }
                        if c.is_digit(10) || c == '-' {
                            *state = JsonTokenizerState::InNumber;
                            break Ok((JsonToken::BeginNumber, cursor))
                        }
                        match c {
                            '"' => {
                                *state = JsonTokenizerState::InString(JsonStringEscapeState::NotEscaped);
                                Ok(JsonToken::BeginString)
                            }
                            '[' => { Ok(JsonToken::BeginArray) }
                            '{' => { Ok(JsonToken::BeginObject) }
                            ']' => { Ok(JsonToken::EndArray) }
                            '}' => { Ok(JsonToken::EndObject) }
                            ':' => { Ok(JsonToken::NameSeparator) }
                            ',' => { Ok(JsonToken::ValueSeparator) }
                            // Handle False, Null, True
                            't' => { *state = JsonTokenizerState::InTrue(1); cursor = tail; continue; }
                            'n' => { *state = JsonTokenizerState::InNull(1); cursor = tail; continue; }
                            'f' => { *state = JsonTokenizerState::InFalse(1); cursor = tail; continue; }
                            _ => { reject(cursor) }
                        }.and_then(|a| Ok((a, tail)))
                    }
                    None => { need_more(cursor) }
                }
            }
            JsonTokenizerState::InString(escape_state) => {
                break match cursor.split_first() {
                    None => { need_more(cursor) }
                    Some((a, tail)) => {
                        let next_char = match char::from_u32(*a as u32) {
                            Some(c) => { c }
                            None => { break reject(cursor); }
                        };
                        match escape_state {
                            JsonStringEscapeState::NotEscaped => {
                                // If first char is ", EndString.
                                //
                                // Scan until quotation, reverse solidus, control character, or
                                // end-of-chunk.
                                //
                                // Switch on next character:
                                match next_char {
                                    '"' => { // Return chunk.
                                        *state = JsonTokenizerState::Value;
                                        Ok(JsonToken::EndString)
                                    }
                                    '\\' => {
                                        *escape_state = JsonStringEscapeState::FirstEscape;
                                        Ok(JsonToken::StringChunk(&cursor[0..1]))
                                    }
                                    /*'\0'..='\u{001f}' => {
                                        reject(cursor)
                                    }*/
                                    _ => { Ok(JsonToken::StringChunk(&cursor[0..1])) }
                                }
                            }
                            JsonStringEscapeState::FirstEscape => {
                                // Expedient. Incorrect.
                                // TODO: fix to validate escape sequences properly.
                                *escape_state = JsonStringEscapeState::NotEscaped;
                                Ok(JsonToken::StringChunk(&cursor[0..0]))

                                    /*match next_char {
                                }
                                reject(cursor)*/
                            }
                            JsonStringEscapeState::InSequence(_some_u8) => {
                                // Unreachable; when fixing the above fix here too.
                                reject(cursor)
                            }
                        }.and_then(|a| Ok((a, tail)))
                    }
                }
            }
            JsonTokenizerState::InNumber => {
                match cursor.split_first() {
                    Some((b','|b'}'|b']', _)) => {
                        *state = JsonTokenizerState::Value;
                        // Returning cursor instead of r; we need to emit both EndNumber and
                        // ValueSeparator, EndObject, or EndArray, so we change state and return
                        // the cursor _including_ the end symbol.
                        break Ok((JsonToken::EndNumber, cursor));
                    }
                    Some((_, r)) => break Ok((JsonToken::NumberChunk(&cursor[0..1]), r)),
                    _ => break Err((None, cursor)),
                }
            }

            JsonTokenizerState::InTrue(1) => {
                match cursor.split_first() { Some((b'r', r)) => { *state = JsonTokenizerState::InTrue(2); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InTrue(2) => {
                match cursor.split_first() { Some((b'u', r)) => { *state = JsonTokenizerState::InTrue(3); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InTrue(3) => {
                match cursor.split_first() { Some((b'e', r)) => { *state = JsonTokenizerState::Value; break Ok((JsonToken::True,r)); } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InNull(1) => {
                match cursor.split_first() { Some((b'u', r)) => { *state = JsonTokenizerState::InNull(2); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InNull(2) => {
                match cursor.split_first() { Some((b'l', r)) => { *state = JsonTokenizerState::InNull(3); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InNull(3) => {
                match cursor.split_first() { Some((b'l', r)) => { *state = JsonTokenizerState::Value; break Ok((JsonToken::Null,r)); } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InFalse(1) => {
                match cursor.split_first() { Some((b'a', r)) => { *state = JsonTokenizerState::InFalse(2); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InFalse(2) => {
                match cursor.split_first() { Some((b'l', r)) => { *state = JsonTokenizerState::InFalse(3); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InFalse(3) => {
                match cursor.split_first() { Some((b's', r)) => { *state = JsonTokenizerState::InFalse(4); cursor=r; } _ => { break reject(cursor); } }
            }
            JsonTokenizerState::InFalse(4) => {
                match cursor.split_first() { Some((b'e', r)) => { *state = JsonTokenizerState::Value; break Ok((JsonToken::False,r)); } _ => { break reject(cursor); } }
            }
            _ => {
                break reject(cursor); // TODO: finish
            }
        }
    }
}

// Tokenize json and pass to a stream.
impl<T, S : JsonInterp<T>> InterpParser<Json<T>> for Json<S> {
    type State = (JsonTokenizerState, <S as JsonInterp<T>>::State);
    type Returning = <S as JsonInterp<T>>::Returning;

    fn init(&self) -> Self::State {
        (JsonTokenizerState::Value, <S as JsonInterp<T>>::init(&self.0))
    }
    #[inline(never)]
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8], destination: &mut Option<Self::Returning>) -> ParseResult<'a> {
        let mut cursor : &[u8] = chunk;
        loop {
            let tok_r = get_json_token(&mut state.0, cursor);
            let (token, new_cursor) = tok_r?;

            break match <S as JsonInterp<T>>::parse(&self.0, &mut state.1, token, destination) {
                Ok(()) => { Ok(new_cursor) }
                Err(None) => { cursor = new_cursor; continue; }
                Err(Some(a)) => { Err((Some(a), new_cursor)) }
            }
        }
    }
}


// Deliberately no JsonInterp<JsonAny> for anything but DropInterp.

#[derive(Clone, Copy, Debug)]
pub enum DropInterpStateEnum {
    Start,
    InName,
    InString,
    InNumber,
    ObjectNamePosition,
    ObjectNameSeparator,
    AfterValue
}

use arrayvec::ArrayVec;

#[derive(Debug)]
pub struct DropInterpJsonState {
    stack : ArrayVec<bool, 32>, // True = Object, False = Array
    seen_item : bool,
    state : DropInterpStateEnum
}

impl JsonInterp<JsonAny> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    #[inline(never)]
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, ref mut seen_item } = full_state;
        *state = match (stack.last(), &state, token) {

            // Strings
            (_, DropInterpStateEnum::Start, JsonToken::BeginString) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::StringChunk(_)) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::EndString) => {
                DropInterpStateEnum::AfterValue
            }
            (_, DropInterpStateEnum::InString, _) => {
                return Err(Some(OOB::Reject)); // Broken invariant from lexer.
            }

            // Numbers
            (_, DropInterpStateEnum::Start, JsonToken::BeginNumber) => {
                DropInterpStateEnum::InNumber
            }
            (_, DropInterpStateEnum::InNumber, JsonToken::NumberChunk(_)) => {
                DropInterpStateEnum::InNumber
            }
            (_, DropInterpStateEnum::InNumber, JsonToken::EndNumber) => {
                DropInterpStateEnum::AfterValue
            }
            (_, DropInterpStateEnum::InNumber, _) => {
                return Err(Some(OOB::Reject)) // Broken invariant from lexer.
            }

            // Named terms
            (_, DropInterpStateEnum::Start, JsonToken::False | JsonToken::Null | JsonToken::True) => {
                DropInterpStateEnum::AfterValue
            }

            // Array handling
            (_, DropInterpStateEnum::Start, JsonToken::BeginArray) => {
                stack.push(false);
                *seen_item = false;
                DropInterpStateEnum::Start
            }
            (Some(false), DropInterpStateEnum::Start, JsonToken::EndArray) => {
                if *seen_item {
                    return Err(Some(OOB::Reject)) // Trailing comma is not allowed in json.
                } else {
                    stack.pop();
                    DropInterpStateEnum::AfterValue
                }
            }
            (Some(false), DropInterpStateEnum::AfterValue, JsonToken::ValueSeparator) => {
                *seen_item = true;
                DropInterpStateEnum::Start
            }
            (Some(false), DropInterpStateEnum::AfterValue, JsonToken::EndArray) => {
                stack.pop();
                DropInterpStateEnum::AfterValue
            }

            // Object handling
            (_, DropInterpStateEnum::Start, JsonToken::BeginObject) => {
                stack.push(true);
                *seen_item = false;
                DropInterpStateEnum::ObjectNamePosition
            }
            (Some(true), DropInterpStateEnum::ObjectNamePosition, JsonToken::EndObject) => {
                if *seen_item {
                    return Err(Some(OOB::Reject)) // Trailing comma is not allowed in json.
                } else {
                    stack.pop();
                    DropInterpStateEnum::AfterValue
                }
            }
            //   Names (special strings)
            (_, DropInterpStateEnum::ObjectNamePosition, JsonToken::BeginString) => {
                DropInterpStateEnum::InName
            }
            (_, DropInterpStateEnum::InName, JsonToken::BeginString) => {
                DropInterpStateEnum::InName
            }
            (_, DropInterpStateEnum::InName, JsonToken::StringChunk(_)) => {
                DropInterpStateEnum::InName
            }
            (_, DropInterpStateEnum::InName, JsonToken::EndString) => {
                DropInterpStateEnum::ObjectNameSeparator
            }
            //   Field values
            (_, DropInterpStateEnum::ObjectNameSeparator, JsonToken::NameSeparator) => { // Recursively parse a value.
                DropInterpStateEnum::Start
            }
            (Some(true), DropInterpStateEnum::AfterValue, JsonToken::ValueSeparator) => {
                *seen_item = true;
                DropInterpStateEnum::ObjectNamePosition
            }
            (Some(true), DropInterpStateEnum::AfterValue, JsonToken::EndObject) => {
                stack.pop();
                DropInterpStateEnum::AfterValue
            }

            _ => { return Err(Some(OOB::Reject)) } // Invalid json structure.
        };
        match (stack.is_empty(), state) {
            (true, DropInterpStateEnum::AfterValue) => { *destination=Some(()); Ok(()) }
            _ => { Err(None) }
        }
    }
}

use core::fmt::Debug;
#[cfg(test)]
fn test_json_interp<T: JsonInterp<A>, A>(p: &T, pairs: &[(JsonToken, Result<T::Returning, Option<OOB>>)]) where <T as JsonInterp<A>>::Returning: Debug + PartialEq + Clone {
    let mut state = T::init(p);
    for (token, expected) in pairs {
        let mut destination = None;
        let rv = T::parse(p, &mut state, *token, &mut destination);
        assert_eq!(rv.map(|_| destination.unwrap()), *expected);
    }

}

#[cfg(test)]
fn test_json_interp_parser<T: InterpParser<A>, A>(p: &T, chunk: &[u8], expected: Result<(T::Returning, &[u8]), (Option<OOB>, &[u8])>) where <T as InterpParser<A>>::Returning: Debug + PartialEq + Clone {
    let mut state = T::init(p);
    let mut destination = None;
    let rv = T::parse(p, &mut state, chunk, &mut destination);
    assert_eq!(rv.map(|c| (destination.unwrap(),c)), expected);
}
/*
#[cfg(test)]
fn test_json_interp_parser_p<T: InterpParser<A>, A, B>(p: &T, chunk: &[u8], expected: Result<(B, &[u8]), (Option<OOB>, &[u8])>) where <T as InterpParser<A>>::Returning: Debug + PartialEq<B> + Clone, B: PartialEq<<T as InterpParser<A>>::Returning> {
    let mut state = T::init(p);
    let rv = T::parse(p, &mut state, chunk);
    assert_eq!(expected, rv);
}
*/

#[cfg(test)]
#[test]
fn test_json_any_drop() {
    test_json_interp::<DropInterp, JsonAny>(&DropInterp, &[
        (JsonToken::BeginString, Err(None)),
        (JsonToken::StringChunk(b"foo"), Err(None)),
        (JsonToken::EndString, Ok(()))]);

    test_json_interp::<DropInterp, JsonAny>(&DropInterp, &[
        (JsonToken::BeginString, Err(None)),
        (JsonToken::StringChunk(b"foo"), Err(None)),
        (JsonToken::EndString, Ok(())),
        (JsonToken::BeginString, Err(Some(OOB::Reject)))]);
    test_json_interp::<DropInterp, JsonAny>(&DropInterp, &[
        (JsonToken::BeginArray, Err(None)),
        (JsonToken::BeginString, Err(None)),
        (JsonToken::StringChunk(b"foo"), Err(None)),
        (JsonToken::EndString, Err(None)),
        (JsonToken::ValueSeparator, Err(None)),
        (JsonToken::BeginString, Err(None)),
        (JsonToken::EndString, Err(None)),
        (JsonToken::ValueSeparator, Err(None)),
        (JsonToken::BeginNumber, Err(None)),
        (JsonToken::NumberChunk(b"4"), Err(None)),
        (JsonToken::NumberChunk(b"2"), Err(None)),
        (JsonToken::EndNumber, Err(None)),
        (JsonToken::EndArray, Ok(())),
        (JsonToken::BeginString, Err(Some(OOB::Reject)))]);
    test_json_interp::<DropInterp, JsonAny>(&DropInterp, &[
        (JsonToken::BeginArray, Err(None)),
        (JsonToken::EndArray, Ok(()))]);

    /*

    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[[],[]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[[]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"{}", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[{}]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[{},[],[[[{}]]]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"[}", Err((Some(OOB::Reject), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"{]", Err((Some(OOB::Reject), b"")));

    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"\"\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"\"foo bar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"\"foo\nbar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"\"foo\nb\\\"ar\"", Ok(((), b"")));

    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"{\"five\": {}}", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"{\"five\": {}, \"six\": [[[[[[[]]]]]]]}", Ok(((), b"")));

    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"null", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"true", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"false", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"1", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonAny>>(&Json(DropInterp), b"{\"five\": 5, \"six\", [[[[[[[]]]]]]]}", Ok(((), b"")));
    */
}

impl JsonInterp<JsonBool> for DropInterp {
    type State = ();
    type Returning = ();
    fn init(&self) -> Self::State { () }
    #[inline(never)]
    fn parse<'a>(&self, _state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        match token {
            JsonToken::True | JsonToken::False => { *destination=Some(()); Ok(()) }
            _ => { Err(Some(OOB::Reject)) }
        }
    }
}

impl JsonInterp<JsonString> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    #[inline(never)]
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, seen_item : _ } = full_state;
        *state = match (stack.last(), &state, token) {

            // Strings
            (_, DropInterpStateEnum::Start, JsonToken::BeginString) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::StringChunk(_)) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::EndString) => {
                DropInterpStateEnum::AfterValue
            }
            (_, DropInterpStateEnum::InString, _) => {
                return Err(Some(OOB::Reject)); // Broken invariant from lexer.
            }

            _ => { return Err(Some(OOB::Reject)) } // Invalid json structure.
        };
        match (stack.is_empty(), state) {
            (true, DropInterpStateEnum::AfterValue) => { *destination=Some(()); Ok(()) }
            _ => { Err(None) }
        }
    }
}

#[cfg(test)]
#[test]
fn test_json_string_drop() {
    test_json_interp_parser::<Json<DropInterp>, Json<JsonString>>(&Json(DropInterp), b"\"foo\nbar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonString>>(&Json(DropInterp), b"{\"foo\nbar\": \"\"}", Err((Some(OOB::Reject), b"\"foo\nbar\": \"\"}")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonString>>(&Json(DropInterp), b"[\"foo\nbar\"]", Err((Some(OOB::Reject), b"\"foo\nbar\"]")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonString>>(&Json(DropInterp), b"{}", Err((Some(OOB::Reject), b"}")));
    test_json_interp_parser::<Json<DropInterp>, Json<JsonString>>(&Json(DropInterp), b"[]", Err((Some(OOB::Reject), b"]")));
}

impl JsonInterp<JsonNumber> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    #[inline(never)]
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, seen_item : _ } = full_state;
        *state = match (stack.last(), &state, token) {

            // Numbers
            (_, DropInterpStateEnum::Start, JsonToken::BeginNumber) => {
                DropInterpStateEnum::InNumber
            }
            (_, DropInterpStateEnum::InNumber, JsonToken::NumberChunk(_)) => {
                DropInterpStateEnum::InNumber
            }
            (_, DropInterpStateEnum::InNumber, JsonToken::EndNumber) => {
                DropInterpStateEnum::AfterValue
            }
            (_, DropInterpStateEnum::InNumber, _) => {
                return Err(Some(OOB::Reject)); // Broken invariant from lexer.
            }

            _ => { return Err(Some(OOB::Reject)) } // Invalid json structure.
        };
        match (stack.is_empty(), state) {
            (true, DropInterpStateEnum::AfterValue) => { *destination = Some(()); Ok(()) }
            _ => { Err(None) }
        }
    }
}

#[derive(Debug)]
pub enum JsonArrayDropState<S, D> {
    Start,
    First,
    Item(S, Option<D>),
    AfterValue
}

impl<T> JsonInterp<JsonArray<T>> for DropInterp where DropInterp: JsonInterp<T> {
    type State = JsonArrayDropState<<DropInterp as JsonInterp<T>>::State, <DropInterp as JsonInterp<T>>::Returning>;
    type Returning = ();
    fn init(&self) -> Self::State { JsonArrayDropState::Start }
    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        use JsonArrayDropState::*;
        use JsonToken::*;
        let st = (state, token);
        loop {
            match st {
                (Start, BeginArray) => { set_from_thunk(st.0, || First); }
                (First, EndArray) => { *destination=Some(()); return Ok(()) }
                (First, _) => {
                    set_from_thunk(st.0, || Item(<DropInterp as JsonInterp<T>>::init(&DropInterp), None));
                    continue;
                }
                (Item(ref mut s, ref mut sub_destination), tok) => { <DropInterp as JsonInterp<T>>::parse(&DropInterp, s, tok, sub_destination)?; set_from_thunk(st.0, || AfterValue); }
                (AfterValue, ValueSeparator) => { set_from_thunk(st.0, || Item(<DropInterp as JsonInterp<T>>::init(&DropInterp), None)) }
                (AfterValue, EndArray) => { *destination=Some(()); return Ok(()) }
                _ => { return Err(Some(OOB::Reject)) }
            };
            return Err(None)
        }
    }
}

/* Important: DROPS it's results */
impl<T, S: JsonInterp<T>> JsonInterp<JsonArray<T>> for SubInterp<S> {
    type State = JsonArrayDropState<<S as JsonInterp<T>>::State, <S as JsonInterp<T>>::Returning>;
    type Returning = ();
    fn init(&self) -> Self::State { JsonArrayDropState::Start }
    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        use JsonArrayDropState::*;
        use JsonToken::*;
        let st = (state, token);
        loop {
            match st {
                (Start, BeginArray) => { set_from_thunk(st.0, || First); }
                (First, EndArray) => { *destination=Some(()); return Ok(()) }
                (First, _) => {
                    set_from_thunk(st.0, || Item(<S as JsonInterp<T>>::init(&self.0), None));
                    continue;
                }
                (Item(ref mut s, ref mut sub_destination), tok) => { <S as JsonInterp<T>>::parse(&self.0, s, tok, sub_destination)?; set_from_thunk(st.0, || AfterValue); }
                (AfterValue, ValueSeparator) => { set_from_thunk(st.0, || Item(<S as JsonInterp<T>>::init(&self.0), None)); }
                (AfterValue, EndArray) => { *destination=Some(()); return Ok(()) }
                _ => { return Err(Some(OOB::Reject)) }
            };
            return Err(None)
        }
    }
}

impl<A, R, S : JsonInterp<A>> JsonInterp<A> for Action<S, fn(&<S as JsonInterp<A>>::Returning, &mut Option<R>) -> Option<()>> {
    type State = (<S as JsonInterp<A> >::State, Option<<S as JsonInterp<A>>::Returning>);
    type Returning = R;
    fn init(&self) -> Self::State {
        (<S as JsonInterp<A>>::init(&self.0), None)
    }

    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        self.0.parse(&mut state.0, token, &mut state.1)?;
        match (self.1)(state.1.as_ref().ok_or(Some(OOB::Reject))?, destination) {
            None => { Err(Some(OOB::Reject)) }
            Some(()) => { Ok(()) }
        }
    }
}

pub struct JsonStringAccumulate<const N : usize>;

#[derive(Debug)]
pub enum JsonStringAccumulateState<const N : usize> {
    Start,
    Accumulating,
    End
}
impl<const N : usize> JsonInterp<JsonString> for JsonStringAccumulate<N> {
    type State = JsonStringAccumulateState<N>;
    type Returning = ArrayVec<u8,N>;
    fn init(&self) -> Self::State { JsonStringAccumulateState::Start }
    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        match (state, token) {
            (state@JsonStringAccumulateState::Start, JsonToken::BeginString) => {
                set_from_thunk(destination, || Some(ArrayVec::new()));
                *state = JsonStringAccumulateState::Accumulating;
                Err(None)
            }
            (JsonStringAccumulateState::Accumulating, JsonToken::StringChunk(c)) => {
                destination.as_mut().ok_or(Some(OOB::Reject))?.try_extend_from_slice(c).map_err(|_| Some(OOB::Reject))?;
                Err(None)
            }
            (state@JsonStringAccumulateState::Accumulating, JsonToken::EndString) => {
                *state = JsonStringAccumulateState::End;
                Ok(())
            }
            _ => {
                Err(Some(OOB::Reject))
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test_json_string_accum() {
    use core::convert::TryInto;
    test_json_interp_parser::<Json<JsonStringAccumulate<10>>, Json<JsonString> >(&Json(JsonStringAccumulate), b"\"foo\nbar\"", Ok(((&b"foo\nbar"[..]).try_into().unwrap(), b"")));
}

/*
// Not currently used, questionable usefulness.
//
impl<const MAX : usize, const STRS : &'static StringList> JsonInterp<JsonStringEnum<MAX, STRS>> for DefaultInterp {
    type State = <JsonStringAccumulate<MAX> as JsonInterp<JsonString>>::State;
    type Returning = usize;
    fn init(&self) -> Self::State { <JsonStringAccumulate<MAX> as JsonInterp<JsonString>>::init(&JsonStringAccumulate) }
    #[inline(never)]
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        let a = <JsonStringAccumulate<MAX> as JsonInterp<JsonString>>::parse(&JsonStringAccumulate, state, token)?;
        let mut cursor = STRS;
        let mut n = 0;
        loop {
            match cursor {
                StringList::Cons(s, r) => {
                    if *s == &a[..] {
                        break Ok(n);
                    } else {
                        n = n + 1;
                        cursor = r;
                    }
                }
                StringList::Nil => { break Err(Some(OOB::Reject)); }

            }
        }
    }
}

#[cfg(test)]
#[test]
fn test_json_string_enum() {
    type TestEnum = JsonStringEnum<10, { &StringList::Cons(b"one", &StringList::Cons(b"five", &StringList::Nil)) }>;
    test_json_interp_parser::<Json<DefaultInterp>, Json<TestEnum> >(&Json(DefaultInterp), b"\"five\"", Ok((1, b"")));
    test_json_interp_parser::<Json<DefaultInterp>, Json<TestEnum> >(&Json(DefaultInterp), b"\"one\"", Ok((0, b"")));
    test_json_interp_parser::<Json<DefaultInterp>, Json<TestEnum> >(&Json(DefaultInterp), b"\"two\"", Err((Some(OOB::Reject), b"")));
}
*/

#[macro_export]
macro_rules! define_json_struct_interp {
    { $name:ident $n:literal { $($field:ident : $schemaType:ty),* } } => {
        $crate::json::paste! {
            #[derive(Debug)]
            pub struct [<$name State>]<$([<Field $field:camel>]),* > {
                state : [<$name StateEnum>]<$([<Field $field:camel>]),*>// ,
                // results : $name<$([<Field $field:camel Result>]),*>
            }

            #[derive(Debug)]
            pub enum [<$name StateEnum>]<$([<Field $field:camel>]),*> {
                Start,
                Key(<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::State, Option<<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::Returning>),
                KeySep(<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::Returning),
                ValueSep,
                End,
                $([<Field $field:camel>]([<Field $field:camel>])),*
            }

            //impl<A, AA : JsonInterp<A>, B, BB : JsonInterp<B>, C, CC : JsonInterp<C>> JsonInterp<SomeStruct<A,B,C>> for SomeStruct<AA, BB, CC>

            impl<$([<Field $field:camel>], [<Field $field:camel Interp>] : JsonInterp<[<Field $field:camel>]>),*> JsonInterp<$name<$([<Field $field:camel>]),*>> for $name<$([<Field $field:camel Interp>]),*> {
                type State = [<$name State>]<
                    $(<[<Field $field:camel Interp>] as JsonInterp<[<Field $field:camel>]>>::State),*// ,
                    // $(Option<<[<Field $field:camel Interp>] as JsonInterp<[<Field $field:camel>]>>::Returning>),*
                    >;
                type Returning = $name <
                    $(Option<<[<Field $field:camel Interp>] as JsonInterp<[<Field $field:camel>]>>::Returning>),*
                    >;
                fn init(&self) -> Self::State { Self::State { state: [<$name StateEnum>]::Start } }
    #[inline(never)]
                fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<$crate::interp_parser::OOB>> {
                    full_state.state = match (&mut full_state.state, token) {
                        // Object handling
                        ([<$name StateEnum>]::Start, JsonToken::BeginObject) => {
                            *destination = Some($name {$( [<field_ $field:snake>] : None ),* } );
                            [<$name StateEnum>]::Key(<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::init(&JsonStringAccumulate), None)
                        }
                        ([<$name StateEnum>]::Key(ref mut key_state, ref mut key_destination), token) => {
                            [<$name StateEnum>]::KeySep({<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::parse(&JsonStringAccumulate, key_state, token, key_destination)?; core::mem::take(key_destination).ok_or(Some($crate::interp_parser::OOB::Reject))? })
                        }

                        ([<$name StateEnum>]::KeySep(ref key), JsonToken::NameSeparator) => {
                            match &key[..] {
                                $(
                                    $crate::json_interp::bstringify!($field) => {
                                        let _ = write!(DBG, "json-struct-interp parser: checking key {:?}\n", core::str::from_utf8(key));
                                        [<$name StateEnum>]::[<Field $field:camel>](<[<Field $field:camel Interp>] as JsonInterp<[<Field $field:camel>]>>::init(&self.[<field_ $field:snake>]))
                                    }
                                )*
                                ,
                                _ => {
                                    let _ = write!(DBG, "json-struct-interp parser: Got unexpected key {:?}\n", core::str::from_utf8(key));
                                    return Err(Some($crate::interp_parser::OOB::Reject)) }
                            }
                        }
                        $(
                        ([<$name StateEnum>]::[<Field $field:camel>](ref mut sub), token) => {
                                let rv_temp=<[<Field $field:camel Interp>] as JsonInterp<[<Field $field:camel>]>>::parse(&self.[<field_ $field:snake>], sub, token, &mut destination.as_mut().ok_or(Some($crate::interp_parser::OOB::Reject))?.[<field_ $field:snake>]);//);
                                rv_temp?;
                            [<$name StateEnum>]::ValueSep
                        })*

                        ([<$name StateEnum>]::ValueSep, JsonToken::ValueSeparator) => {
                            [<$name StateEnum>]::Key(<JsonStringAccumulate<$n> as JsonInterp<JsonString>>::init(&JsonStringAccumulate), None)
                        }
                        ([<$name StateEnum>]::ValueSep, JsonToken::EndObject) => {
                            [<$name StateEnum>]::End
                        }
                        _ => return Err(Some($crate::interp_parser::OOB::Reject)),
                    };
                    match full_state.state {
                        [<$name StateEnum>]::End => Ok(()),
                        _ => Err(None)
                    }
                }
            }


            type [<$name DropInterp>] = $name<$(define_json_struct_interp!{ DROP $field DropInterp } ),*>;
            const [<$name:upper _DROP_INTERP>] : [<$name DropInterp>] = [<$name DropInterp>] { $( [<field_ $field:snake>]: DropInterp ),* };
            impl<$([<Field $field:camel>]),*> JsonInterp<$name<$([<Field $field:camel>]),*> > for DropInterp where
                $( DropInterp : JsonInterp<[<Field $field:camel>]> ),*
                {
                    type State = < [<$name DropInterp>] as JsonInterp<$name<$([<Field $field:camel>]),*> >>::State;
                    type Returning = < [<$name DropInterp>] as JsonInterp<$name<$([<Field $field:camel>]),*> >>::Returning;
                    fn init(&self) -> Self::State { <[<$name DropInterp>] as JsonInterp<$name<$([<Field $field:camel>]),*> >>::init(&[<$name:upper _DROP_INTERP>]) }
    #[inline(never)]
                    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<$crate::interp_parser::OOB>> {
                        <[<$name DropInterp>] as JsonInterp<$name<$([<Field $field:camel>]),*> >>::parse(&[<$name:upper _DROP_INTERP>], full_state, token, destination)
                }
            }
        }
    };
    { DROP $a:ident $b: ident } => { $b }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::define_json_struct;
    define_json_struct!{
        SomeStruct 10 {
            FooString : JsonString,
            bar_noodle : JsonString
        }
    }
    define_json_struct_interp!{
        SomeStruct 10 {
            FooString : JsonString,
            bar_noodle : JsonString
        }
    }

    fn mk_astr<const N : usize>(c: &[u8]) -> Option<ArrayVec<u8, N>> {
        let mut v = ArrayVec::new();
        v.try_extend_from_slice(c).ok()?;
        Some(v)
    }
#[cfg(all(target_os="nanos", test))]
    use testmacro::test_item as test;

    #[test]
    fn test_somestruct_macro() {

        test_json_interp_parser::<Json<SomeStruct<JsonStringAccumulate<10>, JsonStringAccumulate<10>>>, Json<SomeStructSchema> >(
            &Json(SomeStruct { field_foo_string: JsonStringAccumulate, field_bar_noodle: JsonStringAccumulate } ),
            b"{\"FooString\": \"one\", \"bar_noodle\": \"two\"}",
            Ok((SomeStruct { field_foo_string: mk_astr(b"one"), field_bar_noodle: mk_astr(b"two") }, b"")));

        test_json_interp_parser::<Json<SomeStruct<JsonStringAccumulate<10>, JsonStringAccumulate<10>>>, Json<SomeStructSchema> >(
            &Json(SomeStruct { field_foo_string: JsonStringAccumulate, field_bar_noodle: JsonStringAccumulate } ),
            b"{\"NotFooStr\": \"one\", \"bar_noodle\": \"two\"}",
            Err((Some(OOB::Reject), b" \"one\", \"bar_noodle\": \"two\"}")));
    }
}



/*
impl JsonInterp<JsonStringEnum<STRS>> for DefaultInterp {
    type State = DropInterpJsonState;
    type Returning = u8;
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    #[inline(never)]
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>, destination: &mut Option<Self::Returning>) -> Result<(), Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, ref mut seen_item } = full_state;
        *state = match (stack.last(), &state, token) {

            // Strings
            (_, DropInterpStateEnum::Start, JsonToken::BeginString) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::StringChunk(_)) => {
                DropInterpStateEnum::InString
            }
            (_, DropInterpStateEnum::InString, JsonToken::EndString) => {
                DropInterpStateEnum::AfterValue
            }
            (_, DropInterpStateEnum::InString, _) => {
                return Err(Some(OOB::Reject)); // Broken invariant from lexer.
            }

            _ => { return Err(Some(OOB::Reject)) } // Invalid json structure.
        };
        match (stack.is_empty(), state) {
            (true, DropInterpStateEnum::AfterValue) => { Ok(()) }
            _ => { Err(None) }
        }
    }
}
*/

/*
struct Z<const Q : [(&'static [u8], usize); 5]>;

#[cfg(test)]
#[test]
fn test_constant_params() {
  let q = Z::<{
      [
          (b"one", 1),
          (b"two", 2),
          (b"three", 3),
          (b"four", 4),
          (b"five", 5)
      ]
  }>;
}

*/


