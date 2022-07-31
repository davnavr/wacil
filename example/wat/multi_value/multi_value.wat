(module
  (type (func (param i64 i64) (result i64)))

  (func (export "doTheThing") (param i32) (result i32 i32)
    i32.const 5678
    local.get 0)

  (func $returnThreeThings (export "returnThreeThings") (result i32 i32 i64)
    i32.const 1
    i32.const 2
    i64.const 3)

  (func (export "addThreeThings") (result i32)
    call $returnThreeThings
    drop
    i32.add)
)
