fn main() -> std::io::Result<()> {
    let res = proto_gen::parse_proto_files(
        true,
        &["test-data/protos/".into()],
        &["test-data/protos/tendermint/abci/types.proto".into()]
    )?;
    println!("{:#?}", res);
    println!("--CODE--");
    println!("{}", proto_gen::generate_rust_code(res));

    Ok(())
}
