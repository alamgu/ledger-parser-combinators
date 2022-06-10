use crate::proto;

impl proto::descriptor::DescriptorProto {
    pub fn gen_rust(&self, reference_depth: usize) -> String {
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
            code.push_str(&format!("        {},\n",f.generate_macro_code_for_field_descriptor(reference_depth)));
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
                code.push_str(&nested_type.gen_rust(reference_depth + 1));
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
    pub fn to_rust_macro(&self) -> String {
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

        code.push_str("    }\n}");

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
