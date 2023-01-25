#![cfg_attr(target_family = "bolos", no_std)]
#![allow(incomplete_features)]
#![feature(cfg_version)]
#![cfg_attr(
    not(version("1.56")),
    feature(bindings_after_at),
    feature(const_generics)
)]
#![cfg_attr(version("1.56"), feature(adt_const_params))]
#![cfg_attr(all(target_family = "bolos", test), no_main)]
#![cfg_attr(target_family = "bolos", feature(custom_test_frameworks))]
#![reexport_test_harness_main = "test_main"]
#![cfg_attr(target_family = "bolos", test_runner(nanos_sdk::sdk_test_runner))]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

#[macro_use]
extern crate enum_init;

//#[cfg(all(not(target_os = "linux"), test))]
//use nanos_sdk::exit_app;
#[cfg(all(not(target_os = "linux"), test))]
use nanos_sdk::pic_rs;

#[allow(dead_code)]
#[allow(non_upper_case_globals)]
static refpos: usize = 1282;

#[no_mangle]
static mut ro_offset: isize = 0;

#[cfg(all(not(target_os = "linux"), test))]
#[no_mangle]
extern "C" fn sample_main() {
    let modfoo = pic_rs(&refpos);
    unsafe {
        let ptrdiff = (&refpos as *const usize).offset_from(modfoo);
        ro_offset = ptrdiff;
    }
    // let ptrdiff = ((foo as *const str) as usize) - ((modfoo as *const str) as usize);
    use nanos_sdk::exit_app;
    test_main();
    exit_app(0);
}

#[cfg(all(not(target_os = "linux"), test))]
use core::panic::PanicInfo;
#[cfg(all(not(target_os = "linux"), test))]
#[panic_handler]
fn handle_panic(_: &PanicInfo) -> ! {
    use nanos_sdk::exit_app;
    exit_app(0);
}

pub mod core_parsers;

pub mod endianness;

pub mod interp_parser;

pub mod json;
pub mod json_interp;

#[cfg(all(target_family = "bolos", test))]
mod test {
    #[allow(unused_imports)]
    use nanos_sdk::TestType;
    use testmacro::test_item as test;

    #[test]
    fn set_reloc_size_hack() {
        unsafe {
            ::core::arch::asm! {
                ".global _reloc_size",
                ".set _reloc_size, 1"
            }
        }
    }
}
