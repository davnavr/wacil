(module
    (table $functions (export "Functions") 1 funcref)
    (func (export "DoIt")
        i32.const 42
        i32.const 0
        call_indirect $functions (param i32)
    )
)
