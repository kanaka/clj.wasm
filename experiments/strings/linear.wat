(module
  ;;(source_mapping_url "strings-linear.wasm.map")

  (import "console" "readline" (func $js_readline))
  (import "console" "printline" (func $js_printline))

  ;; Increase initial memory pages from 1 to 2 (64KB -> 128KB)
  (memory (export "memory") 2)

  ;; Fixed buffer locations
  (global $input_buffer i32 (i32.const 0))
  (global $output_buffer i32 (i32.const 65536))
  (global $buffer_size i32 (i32.const 1048576))

  ;; Array type for GC strings
  (type $string (array (mut i8)))

  (data $prompt_data "input> ")
  (data $prefix_data "result: ")

  ;; Convert linear memory to GC string - Add bounds checking
  (func $mem_to_string (param $ptr i32) (param $len i32) (result (ref $string))
    (local $str (ref $string))
    (local $i i32)
    ;; Validate memory bounds
    (if (i32.gt_u
          (i32.add (local.get $ptr) (local.get $len))
          (i32.mul (memory.size) (i32.const 65536)))
      (then unreachable))
    (local.set $str
      (array.new $string (i32.const 0) (local.get $len)))
    (loop $copy
      (if (i32.lt_u (local.get $i) (local.get $len))
        (then
          (array.set $string
            (local.get $str)
            (local.get $i)
            (i32.load8_u (i32.add (local.get $ptr) (local.get $i))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $copy))))
    (local.get $str))

  ;; Convert GC string to linear memory - Add bounds checking
  (func $string_to_mem (param $str (ref $string)) (param $ptr i32) (result i32)
    (local $len i32)
    (local $i i32)
    (local.set $len (array.len (local.get $str)))
    ;; Validate memory bounds
    (if (i32.gt_u
          (i32.add (local.get $ptr) (local.get $len))
          (i32.mul (memory.size) (i32.const 65536)))
      (then unreachable))
    (loop $copy
      (if (i32.lt_u (local.get $i) (local.get $len))
        (then
          (i32.store8
            (i32.add (local.get $ptr) (local.get $i))
            (array.get_u $string (local.get $str) (local.get $i)))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $copy))))
    (local.get $len))

  ;; Read line with prompt - Write length prefix
  (func $readline (param $prompt (ref $string)) (result (ref $string))
    (local $len i32)
    ;; Write prompt length prefix
    (i32.store
      (global.get $output_buffer)
      (array.len (local.get $prompt)))
    ;; Write prompt to output buffer (after length)
    (local.set $len
      (call $string_to_mem
        (local.get $prompt)
        (i32.add (global.get $output_buffer) (i32.const 4))))

    ;; Call JS readline
    (call $js_readline)
    ;; Convert input buffer to GC string
    (call $mem_to_string
      (i32.add (global.get $input_buffer) (i32.const 4))
      (i32.load (global.get $input_buffer))))

  ;; concat two strings
  (func $concat (param $str1 (ref $string)) (param $str2 (ref $string)) (result (ref $string))
    (local $result (ref $string))
    (local $len1 i32)
    (local $len2 i32)
    (local.set $len1 (array.len (local.get $str1)))
    (local.set $len2 (array.len (local.get $str2)))
    (local.set $result
      (array.new $string
        (i32.const 0)
        (i32.add (local.get $len1) (local.get $len2))))
    (array.copy $string $string
      (local.get $result)
      (i32.const 0)
      (local.get $str1)
      (i32.const 0)
      (local.get $len1))
    (array.copy $string $string
      (local.get $result)
      (local.get $len1)
      (local.get $str2)
      (i32.const 0)
      (local.get $len2))
    (local.get $result))

  (func $main (export "main")
    (local $input (ref $string))
    (local $result (ref $string))
    (local $prompt (ref $string))
    (local $prefix (ref $string))
    (local $len i32)

    (local.set $prompt
      (array.new_data $string $prompt_data (i32.const 0) (i32.const 7)))
    (local.set $prefix
      (array.new_data $string $prefix_data (i32.const 0) (i32.const 8)))

    ;; Read input with prompt
    (local.set $input
      (call $readline (local.get $prompt)))

    ;; Create result with prefix
    (local.set $result
      (call $concat (local.get $prefix) (local.get $input)))

    ;; Write result length prefix
    (i32.store
      (global.get $output_buffer)
      (array.len (local.get $result)))
    ;; Write result string (after length)
    (local.set $len
      (call $string_to_mem
        (local.get $result)
        (i32.add (global.get $output_buffer) (i32.const 4))))
    (call $js_printline))
)
