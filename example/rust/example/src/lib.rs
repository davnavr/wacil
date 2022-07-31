#[no_mangle]
pub extern "C" fn do_something(a: i32) -> i32 {
    a + a
}
