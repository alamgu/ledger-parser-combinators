
// use crate::core_parsers::*;
// use crate::json::*;
use crate::interp_parser::*;

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
    fn parse<'a>(&self, state: &mut Self::State, token: JsonToken<'a>) -> Result<Self::Returning, Option<OOB>>;
}


pub struct Json<S>(S);

pub enum JsonStringEscapeState {
    NotEscaped,
    FirstEscape,
    InSequence(u8)
}

pub enum JsonTokenizerState {
    Value,
    InNumber,
    InString(JsonStringEscapeState),
    InTrue(u8),
    InNull(u8),
    InFalse(u8),
}

// JSON lexer
fn get_json_token<'a>(state: &mut JsonTokenizerState, chunk: &'a [u8]) -> RX<'a, JsonToken<'a>> {
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
                                        Ok(JsonToken::StringChunk(&cursor[0..0]))
                                    }
                                    /*'\0'..='\u{001f}' => {
                                        reject(cursor)
                                    }*/
                                    _ => { Ok(JsonToken::StringChunk(&cursor[0..0])) }
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
                            JsonStringEscapeState::InSequence(u8) => {
                                // Unreachable; when fixing the above fix here too.
                                reject(cursor)
                            }
                        }.and_then(|a| Ok((a, tail)))
                    }
                }
            }
            JsonTokenizerState::InNumber => {
                break reject(cursor); // TODO: finish
            }
            _ => {
                break reject(cursor); // TODO: finish
            }
        }
    }
}

// Tokenize json and pass to a stream.
impl<T, S : JsonInterp<T>> InterpParser<T> for Json<S> {
    type State = (JsonTokenizerState, <S as JsonInterp<T>>::State);
    type Returning = <S as JsonInterp<T>>::Returning;
    
    fn init(&self) -> Self::State {
        (JsonTokenizerState::Value, <S as JsonInterp<T>>::init(&self.0))
    }
    fn parse<'a, 'b>(&self, state: &'b mut Self::State, chunk: &'a [u8]) -> RX<'a, Self::Returning> {
        let mut cursor : &[u8] = chunk;
        loop {
            let (token, new_cursor) = get_json_token(&mut state.0, cursor)?;
            break match <S as JsonInterp<T>>::parse(&self.0, &mut state.1, token) {
                Ok(rv) => { Ok((rv, new_cursor)) }
                Err(None) => { cursor = new_cursor; continue; }
                Err(Some(a)) => { Err((Some(a), new_cursor)) }
            }
        }
    }
}


struct JsonAny;
struct JsonBool;
struct JsonString;
struct JsonNumber;
struct JsonArray<T>(T);

// Deliberately no JsonInterp<JsonAny> for anything but DropInterp.

#[derive(Clone, Copy, Debug)]
enum DropInterpStateEnum {
    Start,
    InName,
    InString,
    InNumber,
    ObjectNamePosition,
    ObjectNameSeparator,
    AfterValue
}

use arrayvec::ArrayVec;
struct DropInterpJsonState {
    stack : ArrayVec<bool, 32>, // True = Object, False = Array
    seen_item : bool,
    state : DropInterpStateEnum
}

impl JsonInterp<JsonAny> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>) -> Result<Self::Returning, Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, ref mut seen_item } = full_state;
        println!("TRACE: {:?}", (stack.last(), *state, token));
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
            (true, DropInterpStateEnum::AfterValue) => { Ok(()) }
            _ => { Err(None) }
        }
    }
}

use core::fmt::Debug;
#[cfg(test)]
fn test_json_interp<T: JsonInterp<A>, A>(p: &T, pairs: &[(JsonToken, Result<T::Returning, Option<OOB>>)]) where <T as JsonInterp<A>>::Returning: Debug + PartialEq + Clone {
    let mut state = T::init(p);
    for (token, expected) in pairs { 
        let rv = T::parse(p, &mut state, *token);
        assert_eq!(rv, *expected);
    }

}

#[cfg(test)]
fn test_json_interp_parser<T: InterpParser<A>, A>(p: &T, chunk: &[u8], expected: Result<(T::Returning, &[u8]), (Option<OOB>, &[u8])>) where <T as InterpParser<A>>::Returning: Debug + PartialEq + Clone {
    let mut state = T::init(p);
    let rv = T::parse(p, &mut state, chunk);
    assert_eq!(rv, expected);
}

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


    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[[],[]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[[]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"{}", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[{}]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[{},[],[[[{}]]]]", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"[}", Err((Some(OOB::Reject), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"{]", Err((Some(OOB::Reject), b"")));
    
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"\"\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"\"foo bar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"\"foo\nbar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"\"foo\nb\\\"ar\"", Ok(((), b"")));

    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"{\"five\": {}}", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"{\"five\": {}, \"six\": [[[[[[[]]]]]]]}", Ok(((), b"")));

    // test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"1", Ok(((), b"")));
    // test_json_interp_parser::<Json<DropInterp>, JsonAny>(&Json(DropInterp), b"{\"five\": 5, \"six\", [[[[[[[]]]]]]]}", Ok(((), b"")));
}

impl JsonInterp<JsonBool> for DropInterp {
    type State = ();
    type Returning = ();
    fn init(&self) -> Self::State { () }
    fn parse<'a>(&self, _state: &mut Self::State, token: JsonToken<'a>) -> Result<Self::Returning, Option<OOB>> {
        match token {
            JsonToken::True | JsonToken::False => { Ok(()) }
            _ => { Err(Some(OOB::Reject)) }
        }
    }
}

impl JsonInterp<JsonString> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>) -> Result<Self::Returning, Option<OOB>> {
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

#[cfg(test)]
#[test]
fn test_json_string_drop() {
    test_json_interp_parser::<Json<DropInterp>, JsonString>(&Json(DropInterp), b"\"foo\nbar\"", Ok(((), b"")));
    test_json_interp_parser::<Json<DropInterp>, JsonString>(&Json(DropInterp), b"{\"foo\nbar\": \"\"}", Err((Some(OOB::Reject), b"\"foo\nbar\": \"\"}")));
    test_json_interp_parser::<Json<DropInterp>, JsonString>(&Json(DropInterp), b"[\"foo\nbar\"]", Err((Some(OOB::Reject), b"\"foo\nbar\"]")));
    test_json_interp_parser::<Json<DropInterp>, JsonString>(&Json(DropInterp), b"{}", Err((Some(OOB::Reject), b"}")));
    test_json_interp_parser::<Json<DropInterp>, JsonString>(&Json(DropInterp), b"[]", Err((Some(OOB::Reject), b"]")));
}

impl JsonInterp<JsonNumber> for DropInterp {
    type State = DropInterpJsonState;
    type Returning = ();
    fn init(&self) -> Self::State { DropInterpJsonState { stack: ArrayVec::new(), state: DropInterpStateEnum::Start, seen_item: false } }
    fn parse<'a>(&self, full_state: &mut Self::State, token: JsonToken<'a>) -> Result<Self::Returning, Option<OOB>> {
        let DropInterpJsonState { ref mut stack, ref mut state, ref mut seen_item } = full_state;
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
            (true, DropInterpStateEnum::AfterValue) => { Ok(()) }
            _ => { Err(None) }
        }
    }
}


