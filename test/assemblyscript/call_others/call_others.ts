
export function multiply2(n: i32): i32 {
    return n + n
}

function helper(a: i32, b: i32): i32 {
    return a + multiply2(b)
}

export function doSomething(n: i32): i32 {
    return 49 + multiply2(n)
}
