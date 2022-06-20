use crate::proto;
use std::path::Path;

impl proto::descriptor::FileDescriptorProto {
    pub fn gen_rust(&self, root_dir: &Path) {
        assert_eq!(self.get_syntax(), "proto3", "Syntax specifer is not \"proto3\"");

        // package: Should be used for building the module namespace
        let package_path = self.get_package()
            .split('.')
            .skip_while(|s| s.is_empty())
            .collect::<Vec<_>>();

        // name: File name that the code was generated from.
        // dependency: Imported Dependencies
        // public_dependency: Indexs of re-exports from dependency list
        // weak_dependency: Google internal, do not use.

        // message_type
        for msg in &self.message_type {
            msg.gen_rust(root_dir, &package_path);
        }

        // enum_type
        for enum_type in &self.enum_type {
            enum_type.gen_rust(root_dir, &package_path);
        }
    }
}

impl proto::descriptor::DescriptorProto {
    pub fn gen_rust(&self, root_dir: &Path, package_path: &[&str]) {
        // name
        //
        // The name of the message
        let name = self.get_name();
        let rust_name = name.to_lowercase();

        let mut nested_path = Vec::from(package_path);
        nested_path.push(&rust_name);

        // nested_type
        //
        // Messages defined in this message
        for nested_type in &self.nested_type {
            nested_type.gen_rust(root_dir, &nested_path);
        }

        // enum_type
        //
        // Enums defined in this message
        for enum_type in &self.enum_type {
            enum_type.gen_rust(root_dir, &nested_path);
        }

        let mut code = String::new();
        code.push_str(&format!("define_message! {{\n    {} {{\n", name));

        let package_depth = package_path.len();
        // field
        //
        // Fields in the message
        for f in &self.field {
            code.push_str(&format!("        {},\n", f.generate_macro_code_for_field_descriptor(package_depth)));
        }

        // extension
        //
        // Extentions to other types

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

        code.push_str(&format!("    }}\n}}\n"));

        add_to_mod(root_dir, package_path, code.as_bytes());
    }
}

impl proto::descriptor::EnumDescriptorProto {
    pub fn gen_rust(&self, root_dir: &Path, package_path: &[&str]) {
        let mut code = String::new();

        // name
        //
        // The name of the enum
        let name = self.get_name();
        // options
        //
        // options for enum type
        code.push_str(&format!("define_enum! {{\n    {} {{\n", name));

        for value in &self.value {
            code.push_str(&format!("        {},\n",&value.to_rust_macro()));
        }

        code.push_str("    }\n}\n");

        add_to_mod(root_dir, package_path, code.as_bytes());
    }
}

impl proto::descriptor::EnumValueDescriptorProto {
    fn to_rust_macro(&self) -> String {
        let name = self.get_name();
        let number = self.get_number();

        // options
        //
        // options for enum value
        format!("{} = {}", name, number)
    }
}

impl proto::descriptor::OneofDescriptorProto {
    fn to_rust_macro(&self) -> String {
        let name = self.get_name();
        let mut code = String::new();
        code.push_str(&format!("oneof {} {{", name));

        code.push('}');
        code
    }
}

use proto::descriptor::FieldDescriptorProto_Label;

impl proto::descriptor::FieldDescriptorProto {
    fn generate_macro_code_for_field_descriptor(&self, reference_depth: usize) -> String {
        let name = self.get_name();
        let label = self.get_label();
        let t = self.get_field_type()
            .to_string(self.get_type_name(), reference_depth);

        let number = self.get_number();

        if label == FieldDescriptorProto_Label::LABEL_REPEATED {
            format!("{}: repeated({}) = {}", name, t, number)
        }
        else {
            format!("{}: {} = {}", name, t, number)
        }
    }
}

use proto::descriptor::FieldDescriptorProto_Type;

impl proto::descriptor::FieldDescriptorProto_Type {
    fn to_string(&self, type_name: &str, reference_depth: usize) -> String {
        match self{
            FieldDescriptorProto_Type::TYPE_DOUBLE => String::from("double"),
            FieldDescriptorProto_Type::TYPE_FLOAT => String::from("float"),
            FieldDescriptorProto_Type::TYPE_INT64 => String::from("int64"),
            FieldDescriptorProto_Type::TYPE_UINT64 => String::from("uint64"),
            FieldDescriptorProto_Type::TYPE_INT32 => String::from("int32"),
            FieldDescriptorProto_Type::TYPE_FIXED64 => String::from("fixed64"),
            FieldDescriptorProto_Type::TYPE_FIXED32 => String::from("fixed32"),
            FieldDescriptorProto_Type::TYPE_BOOL => String::from("bool"),
            FieldDescriptorProto_Type::TYPE_STRING => String::from("string"),
            FieldDescriptorProto_Type::TYPE_GROUP => String::from("group"),
            FieldDescriptorProto_Type::TYPE_BYTES => String::from("bytes"),
            FieldDescriptorProto_Type::TYPE_UINT32 => String::from("uint32"),
            FieldDescriptorProto_Type::TYPE_SFIXED32 => String::from("sfixed32"),
            FieldDescriptorProto_Type::TYPE_SFIXED64 => String::from("sfixed64"),
            FieldDescriptorProto_Type::TYPE_SINT32 => String::from("sint32"),
            FieldDescriptorProto_Type::TYPE_SINT64 => String::from("sint64"),
            FieldDescriptorProto_Type::TYPE_MESSAGE => format!("message({})", buff_type_ref_to_rust_ref(type_name, reference_depth)),
            FieldDescriptorProto_Type::TYPE_ENUM  => format!("enum({})", buff_type_ref_to_rust_ref(type_name, reference_depth)),
        }
    }
}

use std::iter;

fn buff_type_ref_to_rust_ref(buff_type_ref: &str, reference_depth: usize) -> String {
    let parts = buff_type_ref
        .split('.')
        .collect::<Vec<&str>>();

    let (last, rest) = parts
        .split_last()
        .expect("");

    let normalized = rest
        .iter()
        .map(|s| s.to_lowercase())
        .chain(iter::once(last.to_owned().into()))
        .collect::<Vec<String>>();

    if let Some((first, rest)) = normalized.split_first() {
        if first.is_empty() {
            let back_ref = iter::repeat("super")
                .take(reference_depth)
                .collect::<Vec<_>>()
                .join("::");
            return format!("{}::{}", back_ref, rest.join("::"));
        }
    }

    normalized.join("::")
}

use std::fs::OpenOptions;
use std::fs;
use std::io::Write;

// Add code into a module path, writing module decraltions where missing from root/mod.rs down.
// And writing the code to a mod.rs file at the end of the mod_path
pub fn add_to_mod(root: &Path, mod_path: &[&str], code: &[u8]){
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
                .write_all(&format!("pub mod {};\n", new_mod).as_bytes())
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

