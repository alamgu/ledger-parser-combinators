#![allow(incomplete_features)]
#![feature(generic_associated_types)]
#![feature(min_type_alias_impl_trait)]

use ledger_parser_combinators::{
    async_parser::*,
    define_message,
    protobufs::{async_parser::*, schema::*},
};
use std::future::Future;

mod proto {
    include!(concat!(env!("OUT_DIR"), "/proto_defs/mod.rs"));
}

fn main() {
    println!("Hello, world!");
    let _a: Option<proto::test::proto::Test> = None;
    let _a: Option<proto::test::proto::Thing> = None;
    let _a: Option<proto::test::proto::thing::Stuff> = None;
    let _a: Option<proto::test::proto::thing::stuff::Blag> = None;
}

define_message! {
    Thing {
        foo: bytes = 1
    }
}
