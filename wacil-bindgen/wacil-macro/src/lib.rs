use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn wacil_bindgen(attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
