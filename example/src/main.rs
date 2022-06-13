mod google {
    include!(concat!(env!("OUT_DIR"), "/google/mod.rs"));
}
fn main() {
    println!("Hello, world!");
}
