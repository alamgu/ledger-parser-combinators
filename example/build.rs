use std::path::PathBuf;
use std::path::Path;
use std::env;

fn main() -> std::io::Result<()> {
    let goole_proto_include = env::var("PROTO_INCLUDE")
        .map(PathBuf::from)
        .expect("!!");

    let f = proto_gen::parse::parse_proto_files(
        true,
        &[&goole_proto_include],
        &[&goole_proto_include.join("google/protobuf/api.proto")]
    )?;

    proto_gen::generate::generate_rust_code(
        f,
        Path::new("google")
    );
    Ok(())
}
