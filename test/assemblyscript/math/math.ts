
export function factorial(n: u64): u64 {
    let value : u64 = 1
    while (n > 0) {
        value *= n
        n--;
    }
    return value
}

export function is_even(n: i32): i32 {
    return n % 2 == 0
}
