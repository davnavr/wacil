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
    private readonly Limits limits;

    private readonly object locker = new();

    private byte* address;

    private int count;

    private bool disposed = false;

    public UnmanagedMemory(Limits limits) {
        this.limits = limits;
        count = limits.Minimum;
        address = (byte*)Marshal.AllocHGlobal(count);
    }

    /// <inheritdoc/>
    public int PageCount => count;

    /// <inheritdoc/>
    public Limits Limits => limits;

    private void DisposeCheck() {
        if (disposed) {
            throw new ObjectDisposedException(GetType().FullName);
        }
    }

    /// <inheritdoc/>
    public int Grow(int delta) {
        int oldPageCount = count;
        int newPageCount = count + delta;

        if (newPageCount < count || !limits.Contains(newPageCount)) {
            return -1;
        }

        lock(locker) {
            DisposeCheck();
            address = (byte*)Marshal.ReAllocHGlobal((nint)address, (nint)MemoryHelpers.ToByteSize(newPageCount));
        }

        return oldPageCount;
    }

    private void BoundsCheck(int index) {
        if (count == 0 || index >= MemoryHelpers.ToByteSize(count)) {
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

    /// <inheritdoc/>
    public void Write(int index, ReadOnlySpan<byte> bytes) {
        if (bytes.IsEmpty) {
            return;
        }

        lock(locker) {
            DisposeCheck();
            BoundsCheck(index + bytes.Length - 1);
            bytes.CopyTo(new Span<byte>(address + index, bytes.Length));
        }
    }

    /// <inheritdoc/>
    public void Fill(int index, int length, byte value) {
        if (length == 0) {
            return;
        }

        lock(locker) {
            DisposeCheck();
            BoundsCheck(index + length - 1);
            new Span<byte>(address + index, length).Fill(value);
        }
    }

    private void Dispose(bool disposing) {
        try {
            if (disposing) {
                Monitor.Enter(locker);
            }

            if (!disposed) {
                Marshal.FreeHGlobal((nint)address);
                address = null;
                count = 0;
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
