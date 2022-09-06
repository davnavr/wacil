(module
  (func (export "getFourIntegers") (result v128)
    v128.const i32x4 1 2 3 4)
  (func (export "addFourIntegers") (result v128)
    call 0
    i32.const 1
    i32x4.splat
    i32x4.add)
)
