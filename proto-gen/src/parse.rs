use crate::proto;

use std::io::BufReader;
use std::fs::File;
use std::process;
use std::process::Stdio;
use std::io::Error;
use std::io::ErrorKind;
use std::ffi::OsString;
use std::path::Path;

pub fn parse_proto_files(do_include_imports: bool, includes: &[&Path], inputs: &[&Path]) -> std::io::Result<proto::descriptor::FileDescriptorSet> {
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

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn parse_test() {
        let fds = super::parse_proto_files(
            true,
            &[Path::new("../test-data/protos/")],
            &[Path::new("../test-data/protos/tendermint/abci/types.proto")]
        );

        if let Err(e) = fds {
            assert!(false, "{:#?}", e);
        }

    }
}

pub fn testfn() -> String { "from parse".to_string()}
