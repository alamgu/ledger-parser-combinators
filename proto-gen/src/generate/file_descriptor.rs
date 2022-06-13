use crate::proto;
use std::path::Path;

impl proto::descriptor::FileDescriptorProto {
    pub fn gen_rust(&self, root_dir: &Path) {
        assert!(
            self.syntax.as_ref()
                .expect("Missing syntax specifer")
                .eq("proto3"),
            "Syntax specifer is not \"proto3\""
        );

        // package: Should be used for building the module namespace
        let package_path = self.package
            .as_ref()
            .map(|ps| ps.split('.').collect::<Vec<_>>())
            .unwrap_or(Vec::new());

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
        let name = self.name
            .as_ref()
            .expect("message name missing!");
        let mut foo = Vec::from(package_path);
        foo.push(name);

        // nested_type
        //
        // Messages defined in this message
        for nested_type in &self.nested_type {
            nested_type.gen_rust(root_dir, &foo);
        }

        // enum_type
        //
        // Enums defined in this message
        for enum_type in &self.enum_type {
            enum_type.gen_rust(root_dir, &foo);
        }

        let mut code = String::new();
        code.push_str(&format!("define_message! {{\n    {name} {{\n"));

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
        let name = self.name
            .as_ref()
            .expect("!!");

        // options
        //
        // options for enum type
        code.push_str(&format!("define_enum! {{\n    {name} {{\n"));

        for value in &self.value {
            code.push_str(&format!("        {},\n",&value.to_rust_macro()));
        }

        code.push_str("    }\n}\n");

        add_to_mod(root_dir, package_path, code.as_bytes());
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
    fn to_rust_macro(&self) -> String {
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

    fn generate_macro_code_for_field_descriptor(&self, reference_depth: usize) -> String {
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
            .to_string(&self.type_name, reference_depth);

        let number = self.number
            .as_ref()
            .expect("!!");
        format!("{name}: {label}{t} = {number}")
    }
}
use proto::descriptor::field_descriptor_proto::Label;

impl proto::descriptor::field_descriptor_proto::Label {
    fn macro_code_explicit(&self) -> &str {
        match self{
            Label::LABEL_OPTIONAL => "optional ",
            Label::LABEL_REQUIRED => "required ",
            Label::LABEL_REPEATED => "repeated ",
        }
    }

    fn macro_code(&self) -> &str {
        match self{
            proto::descriptor::field_descriptor_proto::Label::LABEL_REPEATED => "repeated ",
            _ => ""
        }
    }
}
use proto::descriptor::field_descriptor_proto::Type;

impl proto::descriptor::field_descriptor_proto::Type {
    fn to_string(&self, type_name: &Option<String>, reference_depth: usize) -> String {
        match self{
            Type::TYPE_DOUBLE => String::from("double"),
            Type::TYPE_FLOAT => String::from("float"),
            Type::TYPE_INT64 => String::from("int64"),
            Type::TYPE_UINT64 => String::from("uint64"),
            Type::TYPE_INT32 => String::from("int32"),
            Type::TYPE_FIXED64 => String::from("fixed64"),
            Type::TYPE_FIXED32 => String::from("fixed32"),
            Type::TYPE_BOOL => String::from("bool"),
            Type::TYPE_STRING => String::from("string"),
            Type::TYPE_GROUP => String::from("group"),
            Type::TYPE_BYTES => String::from("bytes"),
            Type::TYPE_UINT32 => String::from("uint32"),
            Type::TYPE_SFIXED32 => String::from("sfixed32"),
            Type::TYPE_SFIXED64 => String::from("sfixed64"),
            Type::TYPE_SINT32 => String::from("sint32"),
            Type::TYPE_SINT64 => String::from("sint64"),
            Type::TYPE_MESSAGE | Type::TYPE_ENUM => buff_type_ref_to_rust_ref(type_name.as_ref().unwrap(), reference_depth),
        }
    }
}

use std::iter;

fn buff_type_ref_to_rust_ref(buff_type_ref: &str, reference_depth: usize) -> String {
    iter::repeat("super")
        .take(reference_depth)
        .chain(buff_type_ref.trim_start_matches('.').split('.'))
        .collect::<Vec<_>>()
        .join("::")
}

use std::fs::OpenOptions;
use std::fs;
use std::io::Write;

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

