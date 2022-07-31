(module
  (func (export "theProblematicFunction") (result i32)
    block $L (result i32)
    i32.const 7
    br $L
    unreachable
    end)
)
