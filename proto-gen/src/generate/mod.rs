use std::env;
use std::fs;
use std::path::PathBuf;
use std::path::Path;
use std::fs::OpenOptions;
use std::io::prelude::*;
use proto::descriptor::FileDescriptorSet;

use crate::proto;

mod file_descriptor;
mod descriptor;

pub fn generate_rust_code(proto_file_descriptor_set: FileDescriptorSet, out_path_in_out_dir: &Path) {
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

    for file in proto_file_descriptor_set.file {
        let (pagkage, rust) = file.gen_rust();
        // TODO: I dont like making empty vectors for every file.
        // I wanted to use slices of string slices. And defult to an empty slice.
        // But I had trouble doing things with Option/map/and/unwrap/to_slice

        add_to_mod(&out_dir, &pagkage, rust.as_bytes());

    }
}

// Add code into a module path, writing module decraltions where missing from root/mod.rs down.
// And writing the code to a mod.rs file at the end of the mod_path
fn add_to_mod(root: &Path, mod_path: &[&str], code: &[u8]){
    let op = OpenOptions::new()
        .create(true)
        .append(true)
        .to_owned();

    // If we have to add the code in a module relative to root
    if let Some((new_mod, rest)) = mod_path.split_first() {
        // Directory of the module
        let mod_dir = root.join(new_mod);

        // If the directory of the module does not exists
        // that means its a new modules and we never included it in root's module
        if !mod_dir.exists() {
            // Create a directory for the new module
            fs::create_dir(&mod_dir)
                .expect("Could not create module dir");
            // Include the module
            op.open(root.join("mod.rs"))
                .expect("Could not open module file")
                .write_all(&format!("mod {new_mod};\n").as_bytes())
                .expect("Could not write in module file");
        }

        // Add the code deeper in the module tree
        // Making the new module the new root
        // and the rest of the modules the relative path to root
        add_to_mod(&mod_dir, rest, code);
    }
    // If we dont need to put the code in a module relative to root
    else {
        // Add the code to root
        op.open(root.join("mod.rs"))
            .expect("Could not open module file")
            .write_all(code)
            .expect("Could not write in module file");
    }
}

#[cfg(test)]
pub mod tests {
    use std::env;
    use std::path::PathBuf;
    use std::fs;
    use std::path::Path;

    #[test]
    fn cosmos_sign_doc() {
        let fds = parse_from_bytes(br#"syntax = "proto3";
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
"#)
            .expect("Error parsing proto file");

        let mod_dir = Path::new("cosmos_test");

        super::generate_rust_code(fds, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")), "mod cosmos;\n");
        assert_eq!(string_from_path(&mod_dir.join("cosmos/mod.rs")), "mod tx;\n");
        assert_eq!(string_from_path(&mod_dir.join("cosmos/tx/mod.rs")), "mod v1beta1;\n");
        assert_eq!(string_from_path(&mod_dir.join("cosmos/tx/v1beta1/mod.rs")), r#"// Generated from proto file: test.proto
// Messages
define_message! {
    SignDoc {
        body_bytes: bytes = 1,
        auth_info_bytes: bytes = 2,
        chain_id: string = 3,
        account_number: uint64 = 4,

    }
}

"#);
    }

    #[test]
    fn empty_message() {
        let fds = parse_from_bytes(br#"syntax = "proto3";
message Test { }
"#)
            .expect("Error parsing proto file");

        let mod_dir = Path::new("empty_message_test");

        super::generate_rust_code(fds, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")), r#"// Generated from proto file: test.proto
// Messages
define_message! {
    Test {

    }
}

"#);
    }

    #[test]
    fn google_protos() {
        let google_proto_include = PathBuf::from(env::var("PROTO_INCLUDE")
            .unwrap());

        let fds = crate::parse::parse_proto_files(
            true,
            &[&google_proto_include],
            &[&google_proto_include.join("google/protobuf/api.proto")])
            .expect("!!");

        let mod_dir = Path::new("google_protos_test");

        super::generate_rust_code(fds, mod_dir);

        assert_eq!(string_from_path(&mod_dir.join("mod.rs")), "mod google;\n");
        assert_eq!(string_from_path(&mod_dir.join("google/mod.rs")), "mod protobuf;\n");
        assert_eq!(string_from_path(&mod_dir.join("google/protobuf/mod.rs")),
r#"// Generated from proto file: google/protobuf/source_context.proto
// Messages
define_message! {
    SourceContext {
        file_name: string = 1,

    }
}

// Generated from proto file: google/protobuf/any.proto
// Messages
define_message! {
    Any {
        type_url: string = 1,
        value: bytes = 2,

    }
}

// Generated from proto file: google/protobuf/type.proto
// Dependencies
// google/protobuf/any.proto
// google/protobuf/source_context.proto

// Messages
define_message! {
    Type {
        name: string = 1,
        fields: repeated super::super::google::protobuf::Field = 2,
        oneofs: repeated string = 3,
        options: repeated super::super::google::protobuf::Option = 4,
        source_context: super::super::google::protobuf::SourceContext = 5,
        syntax: super::super::google::protobuf::Syntax = 6,

    }
}
define_message! {
    Field {
        kind: super::super::google::protobuf::Field::Kind = 1,
        cardinality: super::super::google::protobuf::Field::Cardinality = 2,
        number: int32 = 3,
        name: string = 4,
        type_url: string = 6,
        oneof_index: int32 = 7,
        packed: bool = 8,
        options: repeated super::super::google::protobuf::Option = 9,
        json_name: string = 10,
        default_value: string = 11,

// Enums
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

    }
}
define_message! {
    Enum {
        name: string = 1,
        enumvalue: repeated super::super::google::protobuf::EnumValue = 2,
        options: repeated super::super::google::protobuf::Option = 3,
        source_context: super::super::google::protobuf::SourceContext = 4,
        syntax: super::super::google::protobuf::Syntax = 5,

    }
}
define_message! {
    EnumValue {
        name: string = 1,
        number: int32 = 2,
        options: repeated super::super::google::protobuf::Option = 3,

    }
}
define_message! {
    Option {
        name: string = 1,
        value: super::super::google::protobuf::Any = 2,

    }
}

// Enums
define_enum! {
    Syntax {
        SYNTAX_PROTO2 = 0,
        SYNTAX_PROTO3 = 1,
    }
}

// Generated from proto file: google/protobuf/api.proto
// Dependencies
// google/protobuf/source_context.proto
// google/protobuf/type.proto

// Messages
define_message! {
    Api {
        name: string = 1,
        methods: repeated super::super::google::protobuf::Method = 2,
        options: repeated super::super::google::protobuf::Option = 3,
        version: string = 4,
        source_context: super::super::google::protobuf::SourceContext = 5,
        mixins: repeated super::super::google::protobuf::Mixin = 6,
        syntax: super::super::google::protobuf::Syntax = 7,

    }
}
define_message! {
    Method {
        name: string = 1,
        request_type_url: string = 2,
        request_streaming: bool = 3,
        response_type_url: string = 4,
        response_streaming: bool = 5,
        options: repeated super::super::google::protobuf::Option = 6,
        syntax: super::super::google::protobuf::Syntax = 7,

    }
}
define_message! {
    Mixin {
        name: string = 1,
        root: string = 2,

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

    use crate::proto::descriptor::FileDescriptorSet;

    pub fn parse_from_bytes(proto: &[u8]) -> std::io::Result<FileDescriptorSet>{
        let test_dir_name = "proto";
        let temp_dir = tempfile::Builder::new()
            .prefix(test_dir_name)
            .tempdir()
            .expect("Could not get temp dir");

        let test_file_name = "test.proto";

        let temp_file_path = &temp_dir.path().join(test_file_name);

        fs::write(temp_file_path, proto)
            .expect("Could not write to temp file");

        crate::parse::parse_proto_files(
            true,
            &[&temp_dir.path().to_path_buf()],
            &[temp_file_path]
        )
    }

}
