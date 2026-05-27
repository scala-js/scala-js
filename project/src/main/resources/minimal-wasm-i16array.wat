;; wasm-tool parse minimal-wasm-i16array.wat -o minimal-wasm-i16array.wasm
(module
  (type $i16Array (array (mut i16)))

  (func (export "create") (param $length i32) (result (ref $i16Array))
    local.get $length
    array.new_default $i16Array)

  (func (export "length") (param $array (ref $i16Array)) (result i32)
    local.get $array
    array.len)

  (func (export "get") (param $array (ref $i16Array)) (param $index i32) (result i32)
    local.get $array
    local.get $index
    array.get_u $i16Array)

  (func (export "set") (param $array (ref $i16Array)) (param $index i32) (param $value i32)
    local.get $array
    local.get $index
    local.get $value
    array.set $i16Array))
