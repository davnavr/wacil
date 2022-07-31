(module
  (func (export "MyFunction") (param i32) (result i32)
    i32.const 26
    i32.const 62
    local.get 0
    i32.const 7
    i32.eq
    select
  )
)
