//! Provides macros for generating the necessary metadata needed by the backend.

use proc_macro::TokenStream;

#[proc_macro]
pub fn wacil_bindgen(item: TokenStream) -> TokenStream {
    // TODO: Have the macros here delegate to some support crate for easier testing. (proc_macro2)
    item
}

// TODO
/*
wacil_import! {
    // TODO: Specifying assembly name should be advanced feature, not likely to be used
    class My.Namespace.Name.MyStaticClass {
        fn MyMethod(a: ty, b: ty) -> ty;
    }
}
*/
