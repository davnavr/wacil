//! Bindings to the contents of the [`System`](https://learn.microsoft.com/en-us/dotnet/api/system) namespace in the .NET standard library.

pub use wacil_bindgen::ClrObject as Object;

pub use wacil_bindgen::result::Exception;

wacil_bindgen::wacil_import! {
    pub class Console {
        
    }
}
