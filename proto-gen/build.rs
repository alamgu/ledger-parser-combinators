use std::env;
use std::fs;
use std::io::Result;
use std::path::Path;

fn main() -> Result<()> {
    let out_dir = format!("{}/google", env::var("OUT_DIR").unwrap());
    let fout_dir = Path::new(&out_dir);

    if fout_dir.exists() {
        fs::remove_dir_all(fout_dir)?;
    }

    fs::create_dir(fout_dir)?;

    // The shell.nix sets this var to where to find the protobuf proto files.
    let google_proto_include = env::var("PROTO_INCLUDE").unwrap();
    // Uses protoc from $PATH
    protoc_rust::Codegen::new()
        .customize(protoc_rust::Customize {
            gen_mod_rs: Some(true),
            ..Default::default()
        })
        .out_dir(fout_dir)
        .include(&google_proto_include)
        .input(google_proto_include + "/google/protobuf/descriptor.proto")
        .run()?;
    Ok(())
}
