//! Contains the [`ClrObject`] struct.

/// Represents a Common Language Runtime object reference.
///
/// All classes in the Common Language Runtime derive from the base class
/// [`Object`](https://learn.microsoft.com/en-us/dotnet/api/system.object).
#[repr(transparent)]
pub struct ClrObject {
    index: isize,
    _phantom: core::marker::PhantomData<*mut u8>,
}

impl ClrObject {
    const unsafe fn from_raw_index(index: isize) -> Self {
        Self {
            index,
            _phantom: core::marker::PhantomData,
        }
    }

    /// A `null` object reference.
    pub const NULL: Self = unsafe {
        // Safety: -1 represents null
        Self::from_raw_index(-1)
    };

    /// Returns an integer representing this object reference.
    pub fn raw_index(&self) -> isize {
        self.index
    }
}

impl core::fmt::Debug for ClrObject {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ClrObject").field("index", &self.index).finish()
    }
}
