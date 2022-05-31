(module
    (import "math" "factorial" (func $factorial (param i64) (result i64)))

    ;;(func $callImportedFunction (export "callImportedFunction") (result i64)
    ;;    (call $factorial (i64.const 10))
    ;;)
)
