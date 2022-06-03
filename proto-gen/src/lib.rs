use std::io::BufReader;
use std::fs::File;
use std::process;
use std::process::Stdio;
use std::io::Error;
use std::io::ErrorKind;
use std::ffi::OsString;
use std::path::PathBuf;

pub mod proto {
    include!(concat!(env!("OUT_DIR"), "/google/mod.rs"));
}

pub fn parse_proto_files(do_include_imports: bool, includes: &[PathBuf], inputs: &[PathBuf]) -> std::io::Result<proto::descriptor::FileDescriptorSet> {
    let include_flags = includes.iter()
        .map(|include| {
            let mut path = OsString::from("-I");
            path.push(include);
            path
        });

    let temp_dir = tempfile::Builder::new()
        .prefix("testttt")
        .tempdir()?;
    let temp_file = temp_dir.path().join("fds");
    let mut descriptor_set_out_flag = OsString::from("--descriptor_set_out=");
    descriptor_set_out_flag.push(& temp_file);

    let mut cmd_args = Vec::new();
    if do_include_imports {
        cmd_args.push(OsString::from("--include_imports"));
    }
    cmd_args.extend(include_flags);
    cmd_args.push(descriptor_set_out_flag);
    cmd_args.extend(inputs.iter().map(|path| path.as_os_str().to_owned()));

    let mut cmd = process::Command::new(&"protoc");

    cmd.stdin(process::Stdio::null());
    cmd.args(cmd_args);

    cmd.stderr(Stdio::piped());

    let child = cmd.spawn()?;

    let output = child.wait_with_output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr)
            .trim_end()
            .to_owned();
        return Err(Error::new(
            ErrorKind::Other,
            format!("command exited with non-zero error {:?}; stderr: {:?}", cmd, stderr)));
    }

    let f = File::open(temp_file)?;
    let mut reader = BufReader::new(f);

    protobuf::Message::parse_from_reader(&mut reader)
        .map_err(|e| e.into())
}

pub fn generate_rust_code(proto_file_descriptor_set: proto::descriptor::FileDescriptorSet) -> String {

    proto_file_descriptor_set.file
        .iter()
        .map(|fdp| fdp.generate_rust_code1().1)
        .collect::<Vec<String>>()
        .join("--FILE--\n")
        .to_string()
}
impl proto::descriptor::FileDescriptorProto {
    pub fn generate_rust_code1(&self) -> (Option<&str>, String) {
        let mut code = String::new();
        // name
        //
        // Might be nice to have the file name that the code was generated from in the header.
        if let Some(n) = &self.name {
            code.push_str(&format!("// Source file name: {n}\n").to_string());
        }

        // package
        //
        // Should be used for building the module namespace,
        // as far as I can think of right now, there are two ways to manage the package namespace
        // 1) For each proto file, make a file with the relative path+name of the module name
        // so if the package is google.protobuf.compiler
        // the file sturcute should be
        // google/mod.rs
        // ```
        // mod protobuf;
        // ```
        //
        // google/Protobuf/mod.rs
        // ```
        // mod compiler;
        // ```
        //
        // google/protobuf/compiler.rs
        // ```
        // CODE
        // ```
        //
        // 2) Putting everything in one file and using nested modules
        // so like
        // generated.rs
        // ```
        // mod google {
        //   mod protobuf {
        //     mod compiler {
        //       CODE
        //     }
        //   }
        // }
        // ```
        //
        // It seems like the only way to reference generated code is threw an include (at least if its in a file)
        // So something like
        // ```
        // pub mod proto {
        //    include!(concat!(env!("OUT_DIR"), "/google/mod.rs"));
        // }
        // ```
        // will bring in the code under the module "proto" so "proto::descriptor::FileDescriptorSet".
        //
        // A proto file can refence a file from a completly different package
        //
        // tendermint/abci/types.proto
        // ```
        // package tendermint.abci;

        // import "tendermint/crypto/proof.proto";
        // import "tendermint/types/types.proto";
        // import "tendermint/crypto/keys.proto";
        // import "tendermint/types/params.proto";
        // import "google/protobuf/timestamp.proto";
        // import "gogoproto/gogo.proto";
        // ```
        //
        // The resulting module needs to be able to refence the types from its imports, even if there are overlaping definitions
        let mut o_package = None;
        if let Some(p) = &self.package {
            code.push_str(&format!("// Package: {p}\n"));
            o_package = Some(p.as_str());
        }

        // dependency
        //
        // Imported Dependencies
        if !self.dependency.is_empty() {
            code.push_str("// Dependencies\n");

            for dep in &self.dependency {
                code.push_str(&format!("// {dep}\n"));
            }
            code.push('\n');
        }
        // public_dependency
        // weak_dependency

        // message_type
        if !self.message_type.is_empty() {
            code.push_str("// Messages\n");

            for msg in &self.message_type {
                code.push_str(&msg.generate_rust_for_descriptor_proto());
                code.push('\n');
            }
            code.push('\n');
        }

        // enum_type
        if !self.enum_type.is_empty() {
            code.push_str("// Enums\n");
            for enum_type in &self.enum_type {
                code.push_str(&enum_type.to_rust_macro());
                code.push('\n');
            }
            code.push('\n');
        }

        // service
        //
        // Services
        if !self.service.is_empty() {
            code.push_str("// Services\n");
            for service in &self.service {
                code.push_str(&format!("// {service}\n"));
            }
            code.push('\n');
        }

        // extension
        //
        // Extentions to other types

        // options
        //
        // Options for the proto file

        // source_code_info
        //
        // Info about the proto file

        // syntax
        //
        // Protobuf syntax version ("proto2", or "proto3")
        (o_package, code)
    }
}

impl proto::descriptor::DescriptorProto {
    pub fn generate_rust_for_descriptor_proto(&self) -> String {
        let mut code = String::new();
        // name
        //
        // The name of the message
        let name = self.name
            .as_ref()
            .expect("message name missing!");
        code.push_str(&format!("define_message! {{\n    {name} {{\n"));

        // field
        //
        // Fields in the message
        for f in &self.field {
            code.push_str(&format!("        {},\n",f.generate_macro_code_for_field_descriptor()));
        }
        code.push('\n');

        // extension
        //
        // Extentions to other types

        // nested_type
        //
        // Messages defined in this message
        if !self.nested_type.is_empty() {
            code.push_str("// Messages\n");
            for nested_type in &self.nested_type {
                code.push_str(&nested_type.generate_rust_for_descriptor_proto());
                code.push('\n');
            }
            code.push('\n');
        }
        // enum_type
        //
        // Enums defined in this message
        // TODO: Indent content
        // TODO: should this be nested?
        if !self.enum_type.is_empty(){
            code.push_str("// Enums\n");
            for enum_type in &self.enum_type {
                code.push_str(&enum_type.to_rust_macro());
                code.push('\n');
            }
            code.push('\n');
        }
        // extension_range
        // oneof_decl
        //
        // Not sure
        // TODO: Understand this!
        if !self.oneof_decl.is_empty() {
            code.push_str("// Oneof_decl\n");
            for oneof_decl in &self.oneof_decl {
                code.push_str(&oneof_decl.to_rust_macro());
            }
        }
        // options
        // reserved_range
        // reserved_name
        // special_fields

        code.push_str(&format!("    }}\n}}"));
        code
    }
}

impl proto::descriptor::EnumDescriptorProto {
    fn to_rust_macro(&self) -> String {
        let mut code = String::new();

        // name
        //
        // The name of the enum
        let name = self.name
            .as_ref()
            .expect("!!");

        // options
        //
        // options for enum type
        format!("define_enum! {{\n    {name} {{\n");

        for value in &self.value {
            code.push_str(&format!("        {},\n",&value.to_rust_macro()));
        }

        code.push_str("    }}\n}}");

        code
    }
}

impl proto::descriptor::EnumValueDescriptorProto {
    fn to_rust_macro(&self) -> String {
        let name = self.name
            .as_ref()
            .expect("!!");
        let number = self.number
            .as_ref()
            .expect("!!");

        // options
        //
        // options for enum value
        format!("{name} = {number}")
    }
}

impl proto::descriptor::OneofDescriptorProto {
    pub fn to_rust_macro(&self) -> String {
        let name = self.name
            .as_ref()
            .expect("!!");
        let mut code = String::new();
        code.push_str(&format!("oneof {name} {{"));

        code.push('}');
        code
    }
}

impl proto::descriptor::FieldDescriptorProto {
    pub fn generate_macro_code_for_field_descriptor(&self) -> String {
        let name = self.name
            .as_ref()
            .expect("!!");
        let label = self.label
            .as_ref()
            .expect("!!")
            .enum_value()
            .expect("!!")
            .macro_code()
            .to_string();
        let t = self.type_
            .as_ref()
            .expect("!!")
            .unwrap()
            .to_string()
            .to_owned();
        let number = self.number
            .as_ref()
            .expect("!!");
        format!("{name}: {label}{t} = {number}")
    }
}

impl proto::descriptor::field_descriptor_proto::Label {
    fn macro_code_explicit(&self) -> &str {
        match self{
            proto::descriptor::field_descriptor_proto::Label::LABEL_OPTIONAL => "optional ",
            proto::descriptor::field_descriptor_proto::Label::LABEL_REQUIRED => "required ",
            proto::descriptor::field_descriptor_proto::Label::LABEL_REPEATED => "repeated ",
        }
    }

    fn macro_code(&self) -> &str {
        match self{
            proto::descriptor::field_descriptor_proto::Label::LABEL_REPEATED => "repeated ",
            _ => ""
        }
    }
}

impl proto::descriptor::field_descriptor_proto::Type {
    fn to_string(&self) -> &str {
        match self{
            proto::descriptor::field_descriptor_proto::Type::TYPE_DOUBLE => "double",
            proto::descriptor::field_descriptor_proto::Type::TYPE_FLOAT => "float",
            proto::descriptor::field_descriptor_proto::Type::TYPE_INT64 => "int64",
            proto::descriptor::field_descriptor_proto::Type::TYPE_UINT64 => "uint64",
            proto::descriptor::field_descriptor_proto::Type::TYPE_INT32 => "int32",
            proto::descriptor::field_descriptor_proto::Type::TYPE_FIXED64 => "fixed64",
            proto::descriptor::field_descriptor_proto::Type::TYPE_FIXED32 => "fixed32",
            proto::descriptor::field_descriptor_proto::Type::TYPE_BOOL => "bool",
            proto::descriptor::field_descriptor_proto::Type::TYPE_STRING => "string",
            proto::descriptor::field_descriptor_proto::Type::TYPE_GROUP => "group",
            proto::descriptor::field_descriptor_proto::Type::TYPE_MESSAGE => "message",
            proto::descriptor::field_descriptor_proto::Type::TYPE_BYTES => "bytes",
            proto::descriptor::field_descriptor_proto::Type::TYPE_UINT32 => "uint32",
            proto::descriptor::field_descriptor_proto::Type::TYPE_ENUM => "enum",
            proto::descriptor::field_descriptor_proto::Type::TYPE_SFIXED32 => "sfixed32",
            proto::descriptor::field_descriptor_proto::Type::TYPE_SFIXED64 => "sfixed64",
            proto::descriptor::field_descriptor_proto::Type::TYPE_SINT32 => "sint32",
            proto::descriptor::field_descriptor_proto::Type::TYPE_SINT64 => "sint64",
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    #[test]
    fn parse_test() {
        let fds = super::parse_proto_files(
            true,
            &["test-data/protos/".into()],
            &["test-data/protos/tendermint/abci/types.proto".into()]
        );

        if let Err(e) = fds {
            assert!(false, "{:#?}", e);
        }

    }

    #[test]
    fn rust_generator_test() {
        let proto =
br#"syntax = "proto3";
package test.foo;
message Test { }
"#;

        bar(proto,
r#"// Package: test.foo
// Messages
define_message! {
    Test {

    }
}

"#);
    }

    fn bar(proto: &[u8], rust: &str) {
        let temp_dir = tempfile::Builder::new()
            .prefix("test1")
            .tempdir()
            .expect("could not get temp dir");
        let test_file_name = "test.proto";

        let temp_file_path = temp_dir.path().join(test_file_name);
        let mut temp_file = std::fs::File::create(&temp_file_path)
            .expect("could not create temp file");
        temp_file.write_all(proto)
            .expect("could not write to temp file");

        let fds = super::parse_proto_files(
            true,
                &[temp_dir.path().to_path_buf()],
                &[temp_file_path]
        )
            .expect("error parsing proto file");


        let rust_code = super::generate_rust_code(fds);

        assert_eq!(rust_code, format!("// Source file name: {test_file_name}\n{rust}"));
    }

    #[test]
    fn foo() {
        let proto = br#"syntax = "proto3";
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
"#;
        bar(proto,
r#"// Package: cosmos.tx.v1beta1
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
}
