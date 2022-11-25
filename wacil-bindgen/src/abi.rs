/// Marker trait for WebAssembly types.
pub unsafe trait WasmValue {}

unsafe impl WasmValue for i32 {}
unsafe impl WasmValue for u32 {}
unsafe impl WasmValue for i64 {}
unsafe impl WasmValue for u64 {}
unsafe impl WasmValue for isize {}
unsafe impl WasmValue for usize {}
unsafe impl WasmValue for f32 {}
unsafe impl WasmValue for f64 {}
unsafe impl<T> WasmValue for *const T {}
unsafe impl<T> WasmValue for *mut T {}

pub trait IntoWasmValue {
    type Value: WasmValue;

    fn into_value(self) -> Self::Value;
}

impl<T: WasmValue> IntoWasmValue for T {
    type Value = Self;

    fn into_value(self) -> Self::Value {
        self
    }
}

impl IntoWasmValue for &crate::ClrObject {
    type Value = isize;

    fn into_value(self) -> Self::Value {
        self.raw_index()
    }
}

impl IntoWasmValue for &crate::result::Exception {
    type Value = isize;

    fn into_value(self) -> Self::Value {
        self.raw_index()
    }
}
