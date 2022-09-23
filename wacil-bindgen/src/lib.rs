#![doc = include_str!("../README.md")]
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod abi;
pub mod interface;
pub mod result;

mod object;
mod runtime;

pub use object::{ClrClass, ClrObject};

/// Wraps an instance of a CLR class.
#[macro_export]
macro_rules! wacil_object_wrapper {
    ($(#[$class_meta:meta])* $access:vis $class_name:ident;) => {
        $(#[$class_meta])*
        #[repr(transparent)]
        $access struct $class_name($crate::ClrObject);

        impl $class_name {
            pub unsafe fn from_object_unchecked(o: $crate::ClrObject) -> Self {
                Self(o)
            }
        }

        impl $crate::ClrClass for $class_name {
            fn into_object(self) -> $crate::ClrObject {
                self.0
            }

            fn as_object(&self) -> &$crate::ClrObject {
                &self.0
            }
        }

        impl<'a> $crate::abi::IntoWasmValue for &'a $class_name {
            type Value = <&'a $crate::ClrObject as $crate::abi::IntoWasmValue>::Value;

            fn into_value(self) -> Self::Value {
                $crate::abi::IntoWasmValue::into_value(&self.0)
            }
        }

        impl ::core::ops::Deref for $class_name {
            type Target = $crate::ClrObject;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}
