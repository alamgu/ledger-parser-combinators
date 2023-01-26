use crate::async_parser::*;
use crate::core_parsers::*;
use crate::interp::*;

use core::future::Future;
#[cfg(feature = "logging")]
use ledger_log::*;

impl HasOutput<bool> for DefaultInterp {
    type Output = bool;
}

impl<BS: Readable> AsyncParser<bool, BS> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output> + 'c where BS: 'c;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let [byte]: [u8; 1] = input.read().await;
            match byte {
                0 => false,
                1 => true,
                _ => reject_on(core::file!(), core::line!()).await,
            }
        }
    }
}

/*
#### ULEB128-Encoded Integers

The BCS format also uses the [ULEB128 encoding](https://en.wikipedia.org/wiki/LEB128) internally
to represent unsigned 32-bit integers in two cases where small values are usually expected:
(1) lengths of variable-length sequences and (2) tags of enum values (see the corresponding
sections below).

|Type                       |Original data          |Hex representation |Serialized bytes   |
|---                        |---                    |---                |---                |
|ULEB128-encoded u32-integer|2^0 = 1                |0x00000001         |01                 |
|                           |2^7 = 128              |0x00000080         |80 01              |
|                           |2^14 = 16384           |0x00004000         |80 80 01           |
|                           |2^21 = 2097152         |0x00200000         |80 80 80 01        |
|                           |2^28 = 268435456       |0x10000000         |80 80 80 80 01     |
|                           |9487                   |0x0000250f         |8f 4a              |

In general, a ULEB128 encoding consists of a little-endian sequence of base-128 (7-bit)
digits. Each digit is completed into a byte by setting the highest bit to 1, except for the
last (highest-significance) digit whose highest bit is set to 0.

In BCS, the result of decoding ULEB128 bytes is required to fit into a 32-bit unsigned
integer and be in canonical form. For instance, the following values are rejected:
* 80 80 80 80 80 01 (2^36) is too large.
* 80 80 80 80 10 (2^33) is too large.
* 80 00 is not a minimal encoding of 0.
*/

pub struct ULEB128;

impl HasOutput<ULEB128> for DefaultInterp {
    type Output = u32;
}

// Parsing logic copied from bcs repo
impl<BS: Readable> AsyncParser<ULEB128, BS> for DefaultInterp {
    type State<'c> = impl Future<Output = Self::Output> + 'c where BS: 'c;
    fn parse<'a: 'c, 'b: 'c, 'c>(&'b self, input: &'a mut BS) -> Self::State<'c> {
        async move {
            let mut value: u64 = 0;
            for shift in (0..32).step_by(7) {
                let [byte]: [u8; 1] = input.read().await;
                let digit = byte & 0x7f;
                value |= u64::from(digit) << shift;
                // If the highest bit of `byte` is 0, return the final value.
                if digit == byte {
                    if shift > 0 && digit == 0 {
                        // We only accept canonical ULEB128 encodings, therefore the
                        // heaviest (and last) base-128 digit must be non-zero.
                        reject_on(core::file!(), core::line!()).await
                    }
                    break;
                }
            }
            // Decoded integer must not overflow.
            use core::convert::TryFrom;
            match u32::try_from(value) {
                Ok(v) => v,
                Err(_) => reject_on::<u32>(core::file!(), core::line!()).await,
            }
        }
    }
}

pub type Vec<T, const N: usize> = DArray<ULEB128, T, N>;
