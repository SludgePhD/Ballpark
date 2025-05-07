use core::{
    alloc::{GlobalAlloc, Layout},
    ptr,
};

#[global_allocator]
static GLOBAL: Alloc = Alloc;

struct Alloc;

unsafe impl GlobalAlloc for Alloc {
    unsafe fn alloc(&self, _layout: Layout) -> *mut u8 {
        ptr::null_mut()
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}
}
