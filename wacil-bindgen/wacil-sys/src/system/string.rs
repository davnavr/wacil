//! Manipulation of .NET strings, which are represented as instances of the
//! [`System.String`](https://learn.microsoft.com/en-us/dotnet/api/system.string) class.
//!
//! See the documentation of [`ClrString`] for more information.

wacil_bindgen::wacil_object_wrapper! {
    /// Represents an instance of the [`System.String`](https://learn.microsoft.com/en-us/dotnet/api/system.string) class.
    ///
    /// In .NET, strings are composed of sequences of 16-bit values known as
    /// [`System.Char`](https://learn.microsoft.com/en-us/dotnet/api/system.char) and can potentially be ill-formed.
    pub ClrString;
}

/// Trait for conversion into a .NET string.
///
/// See the documentation of [`ClrString`] for more information.
pub trait IntoClrString {
    fn into_clr_string(self) -> ClrString;
}

impl IntoClrString for ClrString {
    fn into_clr_string(self) -> ClrString {
        self
    }
}

//impl IntoClrString for &str

//impl<T: alloc::string::ToString> IntoClrString
