use crate::proto;

impl proto::descriptor::FileDescriptorProto {
    pub fn gen_rust(&self) -> (Vec<&str>, String) {
        assert!(
            self.syntax.as_ref()
                .expect("Missing syntax specifer")
                .eq("proto3"),
            "Syntax specifer is not \"proto3\""
        );

        let mut code = String::new();

        // name: File name that the code was generated from.
        let name = self.name
            .as_ref()
            .expect("Could not reterive proto file name");
        code.push_str(&format!("// Generated from proto file: {name}\n"));

        // package: Should be used for building the module namespace
        let package_path = self.package
            .as_ref()
            .map(|ps| ps.split('.').collect::<Vec<_>>())
            .unwrap_or(Vec::new());
        let package_depth = package_path.len();

        // dependency: Imported Dependencies
        // public_dependency: Indexs of re-exports from dependency list
        if !self.dependency.is_empty() {
            code.push_str("// Dependencies\n");

            for (i, dep) in self.dependency.iter().enumerate() {
                code.push_str("// ");
                let i_f = i32::try_from(i)
                    .expect("Could not conver iterator from usize to i32");

                if self.public_dependency.contains(&i_f) {
                    code.push_str("public ");
                }

                code.push_str(dep);
                code.push('\n');
            }
            code.push('\n');
        }


        // weak_dependency
        //
        // Google internal, do not use.

        // message_type
        if !self.message_type.is_empty() {
            code.push_str("// Messages\n");

            for msg in &self.message_type {
                code.push_str(&msg.gen_rust(package_depth));
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

        (package_path, code)
    }
}
