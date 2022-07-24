(module
  (table $functions (export "MyFunctions") 1 funcref)

  (func $addFortyTwo (param i32) (result i32)
    local.get 0
    i32.const 42
    i32.add)

  (elem (table $functions) (offset i32.const 0) funcref (ref.func $addFortyTwo))

  (func $identity (param i32) (result i32)
    local.get 0)

  (elem $passive funcref (ref.func $identity))

  (func (export "UseIdentityFunction")
    i32.const 1
    i32.const 0
    i32.const 0
    table.init $functions $passive)

  (type $anIntegerFunction (func (param i32) (result i32)))

  (func (export "CallTheFunction") (result i32)
    i32.const 1234
    i32.const 0
    call_indirect $functions (type $anIntegerFunction))
)
