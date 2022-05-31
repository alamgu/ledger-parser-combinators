use std::env;
use std::io::Result;

fn main() -> Result<()> {
    // The shell.nix sets this var to where to find the protobuf proto files.
    let google_proto_include = env::var("PROTO_INCLUDE").unwrap();
    // Uses protoc from $PATH
    protobuf_codegen::Codegen::new()
        .protoc()
        .includes(&[
            &google_proto_include
                ])
        .input(google_proto_include + "/google/protobuf/descriptor.proto")
        .cargo_out_dir("google")
        .run_from_script();
    Ok(())
}
