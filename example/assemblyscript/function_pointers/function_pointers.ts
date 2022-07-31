export function callFunctionPointer(f: (a: u64) => u64): u64 {
    return f(5)
}
