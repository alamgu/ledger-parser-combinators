#![cfg_attr(target_family = "bolos", no_std)]
#![allow(incomplete_features)]
#![feature(min_specialization)]
#![feature(auto_traits)]
#![feature(negative_impls)]
#![feature(trace_macros)]
#![feature(log_syntax)]
#![feature(pin_macro)]
#![feature(type_alias_impl_trait)]
#![feature(generic_const_exprs)]
#![feature(cfg_version)]
#![cfg_attr(
    all(target_family = "bolos", not(version("1.56"))),
    feature(bindings_after_at),
    feature(const_generics)
)]
#![cfg_attr(
    all(target_family = "bolos", version("1.56")),
    feature(adt_const_params)
)]
#![cfg_attr(
    all(target_family = "bolos", not(version("1.64"))),
    feature(future_poll_fn)
)]
#![cfg_attr(
    all(target_family = "bolos", not(version("1.65"))),
    feature(generic_associated_types)
)]
#![cfg_attr(all(target_family = "bolos", test), no_main)]
#![cfg_attr(target_family = "bolos", feature(custom_test_frameworks))]
#![reexport_test_harness_main = "test_main"]
#![cfg_attr(target_family = "bolos", test_runner(nanos_sdk::sdk_test_runner))]
#![allow(clippy::not_unsafe_ptr_arg_deref)]

#[macro_use]
extern crate enum_init;

#[macro_use]
extern crate num_derive;

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

pub mod endianness;
pub mod interp;
pub mod schema;

pub mod core_parsers;

// pub mod forward_parser;

pub mod interp_parser;

pub mod json;
pub mod json_interp;

pub mod async_parser;

pub mod protobufs;
