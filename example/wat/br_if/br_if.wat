(module
  (func (export "isThirtyTwo") (param i32) (result i32)
    (local i32)
    block
      local.get 0
      i32.const 32
      i32.ne
      br_if 0
      i32.const 1
      local.set 1
	end
    local.get 0))
