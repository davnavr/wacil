
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

export function horners_method(coefficients: StaticArray<f64>, x: f64): f64 {
    let count = coefficients.length;

    if (count == 0) {
        return 0.0;
    }

    let value = coefficients[count - 1];
    for (let i = count - 2; i >= 0; i--) {
        value = (value * x) + coefficients[i];
    }

    return value;
}

export function quadratic(a: f64, b: f64, c: f64, x: f64): f64 {
    let coefficients = new StaticArray<f64>(3);
    coefficients[0] = c;
    coefficients[1] = b;
    coefficients[2] = a;
    return horners_method(coefficients, x);
}
