#![doc = include_str!("../README.md")]
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod interface;

mod object;

pub use object::ClrObject;

/// Describes a CLR class used in Rust code.
///
/// # Examples
///
/// ```
/// wacil_bindgen::class_import! {
///     pub class MyClassName {
///     }
/// }
/// ```
#[macro_export]
macro_rules! class_import {
    {
        $(#[$class_meta:meta])*
        $access:vis class $class_name:ident {

        }
    } => {
        $(#[$class_meta])*
        #[repr(transparent)]
        $access struct $class_name($crate::ClrObject);

        impl $class_name {
            pub unsafe fn from_object_unchecked(o: $crate::ClrObject) -> Self {
                Self(o)
            }
        }
    }
}

/*
// TODO: Any calls to runtime functions need to look like this:
extern "C" {
    fn __wacil_bg_my_name(a: Argument) -> ReturnValue;
}
*/
