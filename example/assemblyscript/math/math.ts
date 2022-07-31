
export function factorial(n: u64): u64 {
    let value: u64 = 1
    while (n > 0) {
        value *= n
        n--;
    }
    return value
}

export function is_even(n: i32): i32 {
    return n % 2 == 0
}

export const my_favorite_integer: i32 = 9 + 10

export function is_favorite_number(n: i32): bool {
    return n == my_favorite_integer
}
