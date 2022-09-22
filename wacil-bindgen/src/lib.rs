#![doc = include_str!("../README.md")]
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod interface;

mod object;
mod runtime;

pub use object::{ClrClass, ClrObject};

/// Describes a CLR class used in Rust code.
///
/// # Examples
///
/// ```no_run
/// wacil_bindgen::wacil_import! {
///     pub class MyClassName {
///     }
/// }
/// ```
#[macro_export]
macro_rules! wacil_import {
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

        impl $crate::ClrClass for $class_name {
            fn into_object(self) -> $crate::ClrObject {
                self.0
            }

            fn as_object(&self) -> &$crate::ClrObject {
                &self.0
            }
        }

        impl ::core::ops::Deref for $class_name {
            type Target = $crate::ClrObject;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl $crate::interface::ClassImportDescriptor for $class_name {
            const DESCRIPTOR: &'static $crate::interface::ClassImport =
                &$crate::interface::ClassImport::new($crate::interface::TypeName::new($crate::interface::Namespace::GLOBAL, stringify!($class_name)));
        }
    }
}
