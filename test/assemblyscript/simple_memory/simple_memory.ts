
memory.grow(1)

const index = 0

store<i32>(index, 42)

export function getFunnyNumber(): i32 {
    return load<i32>(index)
}
