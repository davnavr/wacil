#![doc = include_str!("../README.md")]
#![no_std]

pub use wacil_macros as macros;

pub mod interface;

/*
// TODO: Any calls to runtime functions need to look like this:
extern "C" {
    fn __wacil_bg_my_name(a: Argument) -> ReturnValue;
}
*/
