#![cfg_attr(target_os="nanos", no_std)]
#![allow(incomplete_features)]
#![feature(bindings_after_at)]
#![feature(const_generics)]

#![cfg_attr(all(target_os="nanos", test), no_main)]
#![cfg_attr(target_os="nanos", feature(custom_test_frameworks))]
#![reexport_test_harness_main = "test_main"]
#![cfg_attr(target_os="nanos", test_runner(nanos_sdk::sdk_test_runner))]

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
    let ptrdiff = (&refpos as * const usize).offset_from(modfoo);
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

// pub mod forward_parser;

pub mod endianness;

pub mod interp_parser;

pub mod json;
pub mod json_interp;
