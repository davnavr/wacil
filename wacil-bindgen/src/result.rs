//! Provides error handling for interoperation with .NET code.

/// Represents a .NET Exception, which is an instance of a class derived from
/// [`System.Exception`](https://learn.microsoft.com/en-us/dotnet/api/system.exception).
#[repr(transparent)]
pub struct Exception(crate::ClrObject);

/// Result type for expressions whose evaluation may result in the throwing of a .NET exception.
pub type Result<T> = core::result::Result<T, Exception>;

impl crate::ClrClass for Exception {
    fn as_object(&self) -> &crate::ClrObject {
        &self.0
    }

    fn into_object(self) -> crate::ClrObject {
        self.0
    }
}

impl core::ops::Deref for Exception {
    type Target = crate::ClrObject;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
