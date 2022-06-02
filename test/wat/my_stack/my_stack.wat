(module
  (memory 1)
  (global $_stack_ptr (mut i32) (i32.const 65535))
  (func $_stack_push (export "_stack_push") (param $amount i32) (result i32)
    global.get $_stack_ptr
    i32.load align=4
    local.get $amount
    i32.sub
    global.set $_stack_ptr
    global.get $_stack_ptr
  )
  (func $_stack_pop (export "_stack_pop") (param $amount i32)
    global.get $_stack_ptr
    i32.load align=4
    local.get $amount
    i32.add
    global.set $_stack_ptr
  )
  (func (export "addition") (result i32)
    ;; Arguments are two integers on the stack, caller is responsible for pushing and popping
    global.get $_stack_ptr
    i32.load align=4
    global.get $_stack_ptr
    i32.load offset=4 align=4
    i32.add
  )
)
