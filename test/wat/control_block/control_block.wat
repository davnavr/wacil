(module
  (func (export "test") (result i32)
    (local i32) ;; wat2wasm does not seem to support block result values
    block
      i32.const 16
      local.set 0
      br 0
    end
    local.get 0
    i32.const 26
    i32.add))
