namespace Wacil.Runtime;

using System;
using System.Runtime.InteropServices;
using System.Threading;

/// <summary>Represents a WebAssembly linear memory backed by unmanaged memory.</summary>
/// <remarks>
/// This class is thread safe, in order to ensure that all memory accesses are valid and that all allocated memory is correctly
/// freed.
/// </remarks>
public unsafe sealed class UnmanagedMemory : IMemory32 {
    private readonly MemoryType limits;

    private readonly object locker = new();

    private byte* address;

    private IntPtr count;

    private bool disposed = false;

    public UnmanagedMemory(MemoryType limits) {
        this.limits = limits;
        count = (IntPtr)limits.Minimum;
        address = (byte*)Marshal.AllocHGlobal(count);
    }

    /// <inheritdoc/>
    public int PageCount => (int)count;

    /// <inheritdoc/>
    public MemoryType Limits => limits;

    private void DisposeCheck() {
        if (disposed) {
            throw new ObjectDisposedException(GetType().FullName);
        }
    }

    /// <inheritdoc/>
    public int Grow(int delta) {
        nint oldPageCount = count;
        nint newPageCount = count + (nint)delta;

        if (newPageCount < count || newPageCount < (nint)limits.Maximum) {
            return -1;
        }

        lock(locker) {
            DisposeCheck();
            nint newByteSize = newPageCount * (nint)MemoryHelpers.PageSize;
            address = (byte*)Marshal.ReAllocHGlobal((IntPtr)address, newByteSize);
        }

        return (int)oldPageCount;
    }

    private void BoundsCheck(int index) {
        uint offset = (uint)index;
        if ((nuint)offset > (nuint)(nint)count * (nuint)MemoryHelpers.PageSize) {
            throw new IndexOutOfRangeException();
        }
    }

    /// <inheritdoc/>
    public byte this[int index] {
        get {
            lock(locker) {
                DisposeCheck();
                BoundsCheck(index);
                return *(address + index);
            }
        }

        set {
            lock(locker) {
                DisposeCheck();
                BoundsCheck(index);
                *(address + index) = value;
            }
        }
    }

    private void Dispose(bool disposing) {
        try {
            if (disposing) {
                Monitor.Enter(locker);
            }

            if (!disposed) {
                Marshal.FreeHGlobal((IntPtr)address);
                address = null;
                count = IntPtr.Zero;
                disposed = true;
            }
        } finally {
            if (disposing) {
                Monitor.Exit(locker);
            }
        }
    }

    /// <inheritdoc/>
    public void Dispose() {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    ~UnmanagedMemory() => Dispose(false);
}
