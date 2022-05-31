use std::io::BufReader;
use std::fs::File;
use std::process;
use std::process::Stdio;
use std::io::Error;
use std::io::ErrorKind;

pub mod google_proto {
    include!(concat!(env!("OUT_DIR"), "/google/mod.rs"));
}

fn main() -> std::io::Result<()> {
    let mut cmd = process::Command::new(&"protoc");
    cmd.stdin(process::Stdio::null());
    cmd.args([ "--proto_path=src/protos/", "--descriptor_set_out=out.fds", "src/protos/tendermint/abci/types.proto" ]);

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

    let f = File::open("out.fds")?;
    let mut reader = BufReader::new(f);

    let boo : google_proto::descriptor::FileDescriptorSet = protobuf::Message::parse_from_reader(&mut reader)?;

    println!("{:#?}",boo);

    Ok(())
}
