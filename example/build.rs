use std::path::PathBuf;
use std::path::Path;
use std::env;
use std::process;

fn main() -> std::io::Result<()> {
    let goole_proto_include = env::var("PROTO_INCLUDE")
        .map(PathBuf::from)
        .expect("!!");

    let temp_dir = tempfile::Builder::new()
        .prefix("proto_gen_sample")
        .tempdir()
        .unwrap();
    let fds_file = temp_dir.path().join("fds.bin");

    let output = process::Command::new("protoc")
        .arg("--include_imports")
        .arg(format!("--proto_path={}", goole_proto_include.display()))
        .arg("--proto_path=proto")
        .arg("proto/sample.proto")
        .arg(format!("--descriptor_set_out={}", fds_file.display()))
        .output()
        .unwrap();

    assert!(output.status.success(), "protoc command returned non success status {}\nstderr:\n{}", output.status, String::from_utf8_lossy(&output.stderr));

    proto_gen::generate::generate_rust_code(&fds_file, Path::new("proto_defs"));
    Ok(())
}
