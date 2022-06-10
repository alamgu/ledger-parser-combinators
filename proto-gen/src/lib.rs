mod proto {
    include!(concat!(env!("OUT_DIR"), "/google/mod.rs"));
}
pub mod parse;
pub mod generate;
