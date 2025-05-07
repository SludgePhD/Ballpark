#![no_std]
#![no_main]

use core::ffi::c_int;

#[cfg(feature = "alloc")]
mod alloc;

extern crate ballpark;

#[cfg(target_os = "none")]
#[panic_handler]
fn on_panic(_info: &core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn main() -> c_int {
    0
}
