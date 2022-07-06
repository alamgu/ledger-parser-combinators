use std::env;
use std::fs;
use std::io::BufReader;
use std::path::PathBuf;
use std::path::Path;
use std::fs::File;
use proto::descriptor::FileDescriptorSet;

use crate::proto;

mod file_descriptor;

pub fn generate_rust_code(file_descriptor_set_bin: &Path, out_path_in_out_dir: &Path) {
    let f = File::open(file_descriptor_set_bin)
        .unwrap();
    let mut reader = BufReader::new(f);

    let proto_file_descriptor_set: FileDescriptorSet = protobuf::Message::parse_from_reader(&mut reader)
        .unwrap();

    if out_path_in_out_dir.is_absolute() {
        panic!("Must provide relative path from $OUT_DIR");
    }

    let mut out_dir: PathBuf = env::var("OUT_DIR")
        .expect("OUT_DIR env var not set")
        .into();

    out_dir.push(out_path_in_out_dir);

    // posible race condition if two parallel calls to this function use the same dir or two hieracacly related dirs (parent/child)
    if out_dir.exists() {
        fs::remove_dir_all(&out_dir)
            .expect("Could not remove old dir");
    }

    fs::create_dir(&out_dir)
        .expect("Could not create new dir");

    file_descriptor::add_to_mod(
        &out_dir,
        &[],
br#"#[allow(non_camel_case_types)]
#[allow(dead_code)]
"#
    );

    for file in proto_file_descriptor_set.file {
        file.gen_rust(&out_dir);
    }
}

#[cfg(test)]
pub mod tests {
    use std::env;
    use std::path::PathBuf;
    use std::fs;
    use std::path::Path;
    use std::process;
    use pretty_assertions::assert_eq;

    #[test]
    fn cosmos_sign_doc() {
        let temp_dir = tempfile::Builder::new()
            .prefix("proto-test")
            .tempdir()
            .unwrap();
        let fds_file = temp_dir.path().join("fds.bin");
        parse_proto_to_file(&fds_file, br#"syntax = "proto3";
package cosmos.tx.v1beta1;
// SignDoc is the type used for generating sign bytes for SIGN_MODE_DIRECT.
message SignDoc {
  // body_bytes is protobuf serialization of a TxBody that matches the
  // representation in TxRaw.
  bytes body_bytes = 1;

  // auth_info_bytes is a protobuf serialization of an AuthInfo that matches the
  // representation in TxRaw.
  bytes auth_info_bytes = 2;

  // chain_id is the unique identifier of the chain this transaction targets.
  // It prevents signed transactions from being used on another chain by an
  // attacker
  string chain_id = 3;

  // account_number is the account number of the account in state
  uint64 account_number = 4;
}
"#);

        let mod_dir = Path::new("cosmos_test");

        super::generate_rust_code(&fds_file, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod cosmos;
"#);
        assert_eq!(string_from_path(&mod_dir.join("cosmos/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
pub mod tx;
"#);
        assert_eq!(string_from_path(&mod_dir.join("cosmos/tx/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
pub mod v1beta1;
"#);
        assert_eq!(string_from_path(&mod_dir.join("cosmos/tx/v1beta1/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
define_message! {
    SignDoc {
        body_bytes: bytes = 1,
        auth_info_bytes: bytes = 2,
        chain_id: string = 3,
        account_number: uint64 = 4
    }
}
"#);
    }

    #[test]
    fn empty_message() {
       let temp_dir = tempfile::Builder::new()
            .prefix("proto-test")
            .tempdir()
            .unwrap();
        let fds_file = temp_dir.path().join("fds.bin");
        parse_proto_to_file(&fds_file, br#"syntax = "proto3";
message Test { }
"#);

        let mod_dir = Path::new("empty_message_test");

        super::generate_rust_code(&fds_file, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
#[allow(non_camel_case_types)]
#[allow(dead_code)]
define_message! {
    Test {

    }
}
"#);
    }

    #[test]
    fn mod_less_sub_messag() {
       let temp_dir = tempfile::Builder::new()
            .prefix("proto-test")
            .tempdir()
            .unwrap();
        let fds_file = temp_dir.path().join("fds.bin");
        parse_proto_to_file(&fds_file, br#"syntax = "proto3";
message Test {
    Foo test_thing = 1;
    Foo.Bar test_other = 2;
    message Foo {
        Test foo_thing = 1;
        Bar foo_other = 2;
        message Bar {
            Test thing = 1;
            Foo other = 2;
        }
    }
}

message Bizz {
    Test thing = 1;
    Test.Foo thing2 = 2;
    Test.Foo.Bar thing3 = 3;
}
"#);

        let mod_dir = Path::new("sub_message");

        super::generate_rust_code(&fds_file, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod test;
define_message! {
    Test {
        test_thing: message(test::Foo) = 1,
        test_other: message(test::foo::Bar) = 2
    }
}
define_message! {
    Bizz {
        thing: message(Test) = 1,
        thing2: message(test::Foo) = 2,
        thing3: message(test::foo::Bar) = 3
    }
}
"#);
        assert_eq!(string_from_path(&mod_dir.join("test/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
pub mod foo;
define_message! {
    Foo {
        foo_thing: message(super::Test) = 1,
        foo_other: message(super::test::foo::Bar) = 2
    }
}
"#);

        assert_eq!(string_from_path(&mod_dir.join("test/foo/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
define_message! {
    Bar {
        thing: message(super::super::Test) = 1,
        other: message(super::super::test::Foo) = 2
    }
}
"#);
    }

    #[test]
    fn google_protos() {
        let google_proto_include = PathBuf::from(env::var("PROTO_INCLUDE")
            .unwrap());

        let temp_dir = tempfile::Builder::new()
            .prefix("buf-out")
            .tempdir()
            .unwrap();
        let fds_file = temp_dir.path().join("fds.bin");

        let output = process::Command::new("protoc")
            .arg("--include_imports")
            .arg(format!("--proto_path={}", google_proto_include.display()))
            .arg(google_proto_include.join("google/protobuf/api.proto"))
            .arg(format!("--descriptor_set_out={}", fds_file.display()))
            .output()
            .unwrap();

        assert!(output.status.success(), "protoc command returned non success status {}\nstderr:\n{}", output.status, String::from_utf8_lossy(&output.stderr));

        let mod_dir = Path::new("google_protos_test");

        super::generate_rust_code(&fds_file, &mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
#[allow(non_camel_case_types)]
#[allow(dead_code)]
pub mod google;
"#);
        assert_eq!(string_from_path(&mod_dir.join("google/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
pub mod protobuf;
"#);
        assert_eq!(string_from_path(&mod_dir.join("google/protobuf/field/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
define_enum! {
    Kind {
        TYPE_UNKNOWN = 0,
        TYPE_DOUBLE = 1,
        TYPE_FLOAT = 2,
        TYPE_INT64 = 3,
        TYPE_UINT64 = 4,
        TYPE_INT32 = 5,
        TYPE_FIXED64 = 6,
        TYPE_FIXED32 = 7,
        TYPE_BOOL = 8,
        TYPE_STRING = 9,
        TYPE_GROUP = 10,
        TYPE_MESSAGE = 11,
        TYPE_BYTES = 12,
        TYPE_UINT32 = 13,
        TYPE_ENUM = 14,
        TYPE_SFIXED32 = 15,
        TYPE_SFIXED64 = 16,
        TYPE_SINT32 = 17,
        TYPE_SINT64 = 18,
    }
}
define_enum! {
    Cardinality {
        CARDINALITY_UNKNOWN = 0,
        CARDINALITY_OPTIONAL = 1,
        CARDINALITY_REQUIRED = 2,
        CARDINALITY_REPEATED = 3,
    }
}
"#);
        assert_eq!(string_from_path(&mod_dir.join("google/protobuf/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;
define_message! {
    SourceContext {
        file_name: string = 1
    }
}
define_message! {
    Any {
        type_url: string = 1,
        value: bytes = 2
    }
}
define_message! {
    Type {
        name: string = 1,
        fields: repeated(message(super::super::google::protobuf::Field)) = 2,
        oneofs: repeated(string) = 3,
        options: repeated(message(super::super::google::protobuf::Option)) = 4,
        source_context: message(super::super::google::protobuf::SourceContext) = 5,
        syntax: enum(super::super::google::protobuf::Syntax) = 6
    }
}
pub mod field;
define_message! {
    Field {
        kind: enum(super::super::google::protobuf::field::Kind) = 1,
        cardinality: enum(super::super::google::protobuf::field::Cardinality) = 2,
        number: int32 = 3,
        name: string = 4,
        type_url: string = 6,
        oneof_index: int32 = 7,
        packed: bool = 8,
        options: repeated(message(super::super::google::protobuf::Option)) = 9,
        json_name: string = 10,
        default_value: string = 11
    }
}
define_message! {
    Enum {
        name: string = 1,
        enumvalue: repeated(message(super::super::google::protobuf::EnumValue)) = 2,
        options: repeated(message(super::super::google::protobuf::Option)) = 3,
        source_context: message(super::super::google::protobuf::SourceContext) = 4,
        syntax: enum(super::super::google::protobuf::Syntax) = 5
    }
}
define_message! {
    EnumValue {
        name: string = 1,
        number: int32 = 2,
        options: repeated(message(super::super::google::protobuf::Option)) = 3
    }
}
define_message! {
    Option {
        name: string = 1,
        value: message(super::super::google::protobuf::Any) = 2
    }
}
define_enum! {
    Syntax {
        SYNTAX_PROTO2 = 0,
        SYNTAX_PROTO3 = 1,
    }
}
define_message! {
    Api {
        name: string = 1,
        methods: repeated(message(super::super::google::protobuf::Method)) = 2,
        options: repeated(message(super::super::google::protobuf::Option)) = 3,
        version: string = 4,
        source_context: message(super::super::google::protobuf::SourceContext) = 5,
        mixins: repeated(message(super::super::google::protobuf::Mixin)) = 6,
        syntax: enum(super::super::google::protobuf::Syntax) = 7
    }
}
define_message! {
    Method {
        name: string = 1,
        request_type_url: string = 2,
        request_streaming: bool = 3,
        response_type_url: string = 4,
        response_streaming: bool = 5,
        options: repeated(message(super::super::google::protobuf::Option)) = 6,
        syntax: enum(super::super::google::protobuf::Syntax) = 7
    }
}
define_message! {
    Mixin {
        name: string = 1,
        root: string = 2
    }
}
"#);
    }

    pub fn string_from_path(parth_from_out_dir: &Path) -> String {
        let out_dir = env::var("OUT_DIR")
            .expect("OUT_DIR env var is not set");

        let path = Path::new(&out_dir).join(parth_from_out_dir);

        let bytes = fs::read(path)
            .expect("Could not read file");

        String::from_utf8_lossy(&bytes)
            .parse()
            .expect("Could not covert bytes to utf8")
    }

    pub fn parse_proto_to_file(out_path: &Path, proto: &[u8]) {
        let test_dir_name = "proto_test";
        let temp_dir = tempfile::Builder::new()
            .prefix(test_dir_name)
            .tempdir()
            .expect("Could not get temp dir");

        let temp_dir = temp_dir.path();

        let test_file_name = "test.proto";

        let temp_file_path = temp_dir.join(test_file_name);

        fs::write(&temp_file_path, proto)
            .expect("Could not write to temp file");

        let output = process::Command::new("protoc")
            .arg("--include_imports")
            .arg(format!("--proto_path={}", temp_dir.display()))
            .arg(&temp_file_path)
            .arg(format!("--descriptor_set_out={}", out_path.display()))
            .output()
            .unwrap();

        assert!(output.status.success(), "protoc command returned non success status {}\nstderr:\n{}", output.status, String::from_utf8_lossy(&output.stderr));
    }
}
