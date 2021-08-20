(module
    (import "simple_memory" "factorial" (func $getFunnyNumber (param i64) (result i64)))

    (func $callImportedFunction (export "callImportedFunction") (result i64)
        (call $getFunnyNumber (i64.const 10))
    )
)
