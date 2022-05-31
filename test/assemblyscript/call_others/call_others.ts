
function helper2(n: i32): i32 {
    return n + n
}

function helper(a: i32, b: i32): i32 {
    return a + helper2(b)
}

export function doSomething(n: i32): i32 {
    return helper(n, 32)
}
