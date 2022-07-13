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

define_message! { @impl
    SignDoc {
        , body_bytes : (LengthDelimitedParser, Bytes, false) = 1
        , auth_info_bytes : (LengthDelimitedParser, Bytes, false) = 2
        , chain_id : (LengthDelimitedParser, String, false) = 3
        , account_number : (AsyncParser, uint64, false) = 4
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
define_message! { @impl
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

define_message! { @impl
    Test {
        , test_thing : (LengthDelimitedParser, test::Foo, false) = 1
        , test_other : (LengthDelimitedParser, test::foo::Bar, false) = 2
    }
}

define_message! { @impl
    Bizz {
        , thing : (LengthDelimitedParser, Test, false) = 1
        , thing2 : (LengthDelimitedParser, test::Foo, false) = 2
        , thing3 : (LengthDelimitedParser, test::foo::Bar, false) = 3
    }
}

"#);
        assert_eq!(string_from_path(&mod_dir.join("test/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;

pub mod foo;

define_message! { @impl
    Foo {
        , foo_thing : (LengthDelimitedParser, super::Test, false) = 1
        , foo_other : (LengthDelimitedParser, super::test::foo::Bar, false) = 2
    }
}

"#);

        assert_eq!(string_from_path(&mod_dir.join("test/foo/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;

define_message! { @impl
    Bar {
        , thing : (LengthDelimitedParser, super::super::Test, false) = 1
        , other : (LengthDelimitedParser, super::super::test::Foo, false) = 2
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
        TYPE_SINT64 = 18
    }
}

define_enum! {
    Cardinality {
        CARDINALITY_UNKNOWN = 0,
        CARDINALITY_OPTIONAL = 1,
        CARDINALITY_REQUIRED = 2,
        CARDINALITY_REPEATED = 3
    }
}

"#);
        assert_eq!(string_from_path(&mod_dir.join("google/protobuf/mod.rs")),
r#"#[allow(unused_imports)]
use ledger_parser_combinators::{define_message, define_enum, interp_parser::DefaultInterp, async_parser::{HasOutput, AsyncParser, Readable, reject}, protobufs::{schema::*, async_parser::*}};
#[allow(unused_imports)]
use core::future::Future;

define_message! { @impl
    SourceContext {
        , file_name : (LengthDelimitedParser, String, false) = 1
    }
}

define_message! { @impl
    Any {
        , type_url : (LengthDelimitedParser, String, false) = 1
        , value : (LengthDelimitedParser, Bytes, false) = 2
    }
}

define_message! { @impl
    Type {
        , name : (LengthDelimitedParser, String, false) = 1
        , fields : (LengthDelimitedParser, super::super::google::protobuf::Field, true) = 2
        , oneofs : (LengthDelimitedParser, String, true) = 3
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 4
        , source_context : (LengthDelimitedParser, super::super::google::protobuf::SourceContext, false) = 5
        , syntax : (AsyncParser, super::super::google::protobuf::Syntax, false) = 6
    }
}

pub mod field;

define_message! { @impl
    Field {
        , kind : (AsyncParser, super::super::google::protobuf::field::Kind, false) = 1
        , cardinality : (AsyncParser, super::super::google::protobuf::field::Cardinality, false) = 2
        , number : (AsyncParser, int32, false) = 3
        , name : (LengthDelimitedParser, String, false) = 4
        , type_url : (LengthDelimitedParser, String, false) = 6
        , oneof_index : (AsyncParser, int32, false) = 7
        , packed : (AsyncParser, bool, false) = 8
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 9
        , json_name : (LengthDelimitedParser, String, false) = 10
        , default_value : (LengthDelimitedParser, String, false) = 11
    }
}

define_message! { @impl
    Enum {
        , name : (LengthDelimitedParser, String, false) = 1
        , enumvalue : (LengthDelimitedParser, super::super::google::protobuf::EnumValue, true) = 2
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 3
        , source_context : (LengthDelimitedParser, super::super::google::protobuf::SourceContext, false) = 4
        , syntax : (AsyncParser, super::super::google::protobuf::Syntax, false) = 5
    }
}

define_message! { @impl
    EnumValue {
        , name : (LengthDelimitedParser, String, false) = 1
        , number : (AsyncParser, int32, false) = 2
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 3
    }
}

define_message! { @impl
    Option {
        , name : (LengthDelimitedParser, String, false) = 1
        , value : (LengthDelimitedParser, super::super::google::protobuf::Any, false) = 2
    }
}

define_enum! {
    Syntax {
        SYNTAX_PROTO2 = 0,
        SYNTAX_PROTO3 = 1
    }
}

define_message! { @impl
    Api {
        , name : (LengthDelimitedParser, String, false) = 1
        , methods : (LengthDelimitedParser, super::super::google::protobuf::Method, true) = 2
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 3
        , version : (LengthDelimitedParser, String, false) = 4
        , source_context : (LengthDelimitedParser, super::super::google::protobuf::SourceContext, false) = 5
        , mixins : (LengthDelimitedParser, super::super::google::protobuf::Mixin, true) = 6
        , syntax : (AsyncParser, super::super::google::protobuf::Syntax, false) = 7
    }
}

define_message! { @impl
    Method {
        , name : (LengthDelimitedParser, String, false) = 1
        , request_type_url : (LengthDelimitedParser, String, false) = 2
        , request_streaming : (AsyncParser, bool, false) = 3
        , response_type_url : (LengthDelimitedParser, String, false) = 4
        , response_streaming : (AsyncParser, bool, false) = 5
        , options : (LengthDelimitedParser, super::super::google::protobuf::Option, true) = 6
        , syntax : (AsyncParser, super::super::google::protobuf::Syntax, false) = 7
    }
}

define_message! { @impl
    Mixin {
        , name : (LengthDelimitedParser, String, false) = 1
        , root : (LengthDelimitedParser, String, false) = 2
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
