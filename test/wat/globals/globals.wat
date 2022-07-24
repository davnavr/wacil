(module
  (import "env" "importedMutableGlobal" (global (mut i32)))
  (import "env" "importedConstGlobal" (global i32))
  (global $myGlobalConst (export "myGlobalConst") (mut i32) (i32.const 42))
)
