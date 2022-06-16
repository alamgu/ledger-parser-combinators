#![allow(incomplete_features)]
#![feature(generic_associated_types)]
#![feature(min_type_alias_impl_trait)]

use ledger_parser_combinators::define_message;

mod proto {
    include!(concat!(env!("OUT_DIR"),"/proto_defs/mod.rs"));
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
