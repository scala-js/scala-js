;; wasm-tool parse minimal-wasm-i8array.wat -o minimal-wasm-i8array.wasm
(module
  (type $i8Array (array (mut i8)))

  (func (export "create") (param $length i32) (result (ref $i8Array))
    local.get $length
    array.new_default $i8Array)

  (func (export "length") (param $array (ref $i8Array)) (result i32)
    local.get $array
    array.len)

  (func (export "get") (param $array (ref $i8Array)) (param $index i32) (result i32)
    local.get $array
    local.get $index
    array.get_u $i8Array)

  (func (export "set") (param $array (ref $i8Array)) (param $index i32) (param $value i32)
    local.get $array
    local.get $index
    local.get $value
    array.set $i8Array))
