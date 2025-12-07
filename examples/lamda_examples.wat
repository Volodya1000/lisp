(module

  (import "env" "print_number" (func $print_number (param f64)))
  (import "env" "princ" (func $princ (param f64)))
  (import "env" "read_num" (func $read_num (result f64))) 

  (memory (export "memory") 100)

  (type $type_0 (func (param f64) (result f64)))
  (type $type_1 (func (param f64) (param f64) (result f64)))
  (type $type_2 (func (param f64) (param f64) (param f64) (result f64)))
  (table 7 funcref)
  (elem (i32.const 0) $range $map $filter $print-list $start $lambda_0 $lambda_1)

  (global $heap_ptr (mut i32) (i32.const 8))
  (global $scratch (mut f64) (f64.const 0.0))
  (global $filtered (mut f64) (f64.const 0.0))
  (global $print-list (mut f64) (f64.const 0.0))
  (global $range (mut f64) (f64.const 0.0))
  (global $filter (mut f64) (f64.const 0.0))
  (global $squared (mut f64) (f64.const 0.0))
  (global $numbers (mut f64) (f64.const 0.0))
  (global $map (mut f64) (f64.const 0.0))
  (global $start (mut f64) (f64.const 0.0))


  (func $std_cons (param $car f64) (param $cdr f64) (result f64)
    (local $addr i32)
    global.get $heap_ptr
    local.tee $addr
    i32.const 16
    i32.add
    global.set $heap_ptr
    local.get $addr
    local.get $car
    f64.store
    local.get $addr
    i32.const 8
    i32.add
    local.get $cdr
    f64.store
    local.get $addr
    f64.convert_i32_u
  )

  (func $std_car (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    f64.load
  )

  (func $std_cdr (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.const 8
    i32.add
    f64.load
  )

  (func $std_is_nil (param $x f64) (result f64)
    local.get $x
    f64.const 0.0
    f64.eq
    (if (result f64) (then f64.const 1.0) (else f64.const 0.0))
  )

  (func $std_equal (param $a f64) (param $b f64) (result f64)
    (local $eq_base i32)
    local.get $a
    local.get $b
    f64.eq
    local.tee $eq_base
    if (result f64)
        f64.const 1.0
    else
        local.get $a
        f64.const 0.0
        f64.eq
        if (result f64)
            f64.const 0.0
        else
            local.get $b
            f64.const 0.0
            f64.eq
            if (result f64)
                 f64.const 0.0
            else
                 local.get $a
                 call $std_car
                 local.get $b
                 call $std_car
                 call $std_equal
                 f64.const 0.0
                 f64.eq
                 if (result f64)
                    f64.const 0.0
                 else
                    local.get $a
                    call $std_cdr
                    local.get $b
                    call $std_cdr
                    call $std_equal
                 end
            end
        end
    end
  )

  (func $std_length (param $ptr f64) (result f64)
    local.get $ptr
    i32.trunc_f64_u
    i32.load
    f64.convert_i32_u
  )

  (func $std_str_concat (param $a f64) (param $b f64) (result f64)
    (local $addr_a i32)
    (local $len_a i32)
    (local $addr_b i32)
    (local $len_b i32)
    (local $new_ptr i32)
    (local $new_len i32)
    (local $i i32)

    local.get $a
    i32.trunc_f64_u
    local.set $addr_a

    local.get $b
    i32.trunc_f64_u
    local.set $addr_b

    local.get $addr_a
    i32.load
    local.set $len_a

    local.get $addr_b
    i32.load
    local.set $len_b

    local.get $len_a
    local.get $len_b
    i32.add
    local.set $new_len

    global.get $heap_ptr
    local.set $new_ptr

    local.get $new_ptr
    local.get $new_len
    i32.store

    ;; Copy A
    i32.const 0
    local.set $i
    (block $break_a (loop $loop_a
        local.get $i
        local.get $len_a
        i32.ge_u
        br_if $break_a

        local.get $new_ptr
        i32.const 4
        i32.add
        local.get $i
        i32.add

        local.get $addr_a
        i32.const 4
        i32.add
        local.get $i
        i32.add
        i32.load8_u

        i32.store8

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $loop_a
    ))

    ;; Copy B
    i32.const 0
    local.set $i
    (block $break_b (loop $loop_b
        local.get $i
        local.get $len_b
        i32.ge_u
        br_if $break_b

        local.get $new_ptr
        i32.const 4
        i32.add
        local.get $len_a
        i32.add
        local.get $i
        i32.add

        local.get $addr_b
        i32.const 4
        i32.add
        local.get $i
        i32.add
        i32.load8_u

        i32.store8

        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $loop_b
    ))

    global.get $heap_ptr
    local.get $new_len
    i32.const 4
    i32.add
    i32.add
    global.set $heap_ptr

    local.get $new_ptr
    f64.convert_i32_u
  )

  (func $range (param $env f64) (param f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 104) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 104
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 16
    i32.add
    local.get 2
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    f64.gt
    f64.convert_i32_s
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    f64.const 0.0
    )
    (else
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    global.get $range
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    f64.const 1.0
    f64.add
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    call $std_cons
    )
    )
  )
  (func $map (param $env f64) (param f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 104) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 104
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 16
    i32.add
    local.get 2
    f64.store
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    f64.const 0.0
    f64.eq
    f64.convert_i32_s
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    f64.const 0.0
    )
    (else
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_car
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    global.get $map
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_cdr
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    call $std_cons
    )
    )
  )
  (func $filter (param $env f64) (param f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 104) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 104
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 16
    i32.add
    local.get 2
    f64.store
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    f64.const 0.0
    f64.eq
    f64.convert_i32_s
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    f64.const 0.0
    )
    (else
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_car
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_car
    global.get $filter
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_cdr
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    call $std_cons
    )
    (else
    f64.const 1.0
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    global.get $filter
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.get $new_env_addr
    i32.const 16
    i32.add
    f64.load
    call $std_cdr
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    )
    (else
    f64.const 0.0
    )
    )
    )
    )
    )
    )
  )
  (func $print-list (param $env f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 96) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 96
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    f64.const 0.0
    f64.eq
    f64.convert_i32_s
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 1
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 10
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
    )
    (else
    f64.const 1.0
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    call $std_car
    call $print_number
    f64.const 0.0
    drop
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    call $std_cdr
    f64.const 0.0
    f64.eq
    f64.convert_i32_s
    f64.const 0.0
    f64.ne
    (if (result f64)
    (then
    f64.const 0.0
    )
    (else
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 1
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
    )
    )
    drop
    global.get $print-list
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    call $std_cdr
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    )
    (else
    f64.const 0.0
    )
    )
    )
    )
  )
  (func $lambda_0 (param $env f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 96) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 96
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    f64.const 4.0
    f64.gt
    f64.convert_i32_s
  )
  (func $lambda_1 (param $env f64) (param f64) (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 96) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 96
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    local.get 1
    f64.store
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    local.get $new_env_addr
    i32.const 8
    i32.add
    f64.load
    f64.mul
  )
  (func $start (param $env f64)  (result f64)
    (local $new_env_addr i32)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
    ;; -- Prologue: Alloc Frame (size 88) --
    global.get $heap_ptr
    local.tee $new_env_addr
    i32.const 88
    i32.add
    global.set $heap_ptr
    local.get $new_env_addr
    local.get $env
    f64.store
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 35
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 71
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 6
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 7
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 8
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 9
    i32.add
    i32.const 97
    i32.store8
    global.get $heap_ptr
    i32.const 10
    i32.add
    i32.const 116
    i32.store8
    global.get $heap_ptr
    i32.const 11
    i32.add
    i32.const 105
    i32.store8
    global.get $heap_ptr
    i32.const 12
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 13
    i32.add
    i32.const 103
    i32.store8
    global.get $heap_ptr
    i32.const 14
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 15
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 16
    i32.add
    i32.const 117
    i32.store8
    global.get $heap_ptr
    i32.const 17
    i32.add
    i32.const 109
    i32.store8
    global.get $heap_ptr
    i32.const 18
    i32.add
    i32.const 98
    i32.store8
    global.get $heap_ptr
    i32.const 19
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 20
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 21
    i32.add
    i32.const 115
    i32.store8
    global.get $heap_ptr
    i32.const 22
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 23
    i32.add
    i32.const 102
    i32.store8
    global.get $heap_ptr
    i32.const 24
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 25
    i32.add
    i32.const 111
    i32.store8
    global.get $heap_ptr
    i32.const 26
    i32.add
    i32.const 109
    i32.store8
    global.get $heap_ptr
    i32.const 27
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 28
    i32.add
    i32.const 49
    i32.store8
    global.get $heap_ptr
    i32.const 29
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 30
    i32.add
    i32.const 116
    i32.store8
    global.get $heap_ptr
    i32.const 31
    i32.add
    i32.const 111
    i32.store8
    global.get $heap_ptr
    i32.const 32
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 33
    i32.add
    i32.const 49
    i32.store8
    global.get $heap_ptr
    i32.const 34
    i32.add
    i32.const 48
    i32.store8
    global.get $heap_ptr
    i32.const 35
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 36
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 37
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 38
    i32.add
    i32.const 10
    i32.store8
    global.get $heap_ptr
    i32.const 39
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
    drop
    global.get $range
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    f64.const 1.0
    f64.const 10.0
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    global.set $numbers
    global.get $numbers
    drop
    global.get $print-list
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    global.get $numbers
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    drop
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 26
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 70
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    i32.const 105
    i32.store8
    global.get $heap_ptr
    i32.const 6
    i32.add
    i32.const 108
    i32.store8
    global.get $heap_ptr
    i32.const 7
    i32.add
    i32.const 116
    i32.store8
    global.get $heap_ptr
    i32.const 8
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 9
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 10
    i32.add
    i32.const 105
    i32.store8
    global.get $heap_ptr
    i32.const 11
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 12
    i32.add
    i32.const 103
    i32.store8
    global.get $heap_ptr
    i32.const 13
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 14
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 15
    i32.add
    i32.const 118
    i32.store8
    global.get $heap_ptr
    i32.const 16
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 17
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 18
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 19
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 20
    i32.add
    i32.const 117
    i32.store8
    global.get $heap_ptr
    i32.const 21
    i32.add
    i32.const 109
    i32.store8
    global.get $heap_ptr
    i32.const 22
    i32.add
    i32.const 98
    i32.store8
    global.get $heap_ptr
    i32.const 23
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 24
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 25
    i32.add
    i32.const 115
    i32.store8
    global.get $heap_ptr
    i32.const 26
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 27
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 28
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 29
    i32.add
    i32.const 10
    i32.store8
    global.get $heap_ptr
    i32.const 30
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
    drop
    global.get $filter
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    f64.const 5.0
    local.get $new_env_addr
    f64.convert_i32_u
    call $std_cons
    global.get $numbers
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    global.set $filtered
    global.get $filtered
    drop
    global.get $print-list
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    global.get $filtered
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    drop
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 33
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 83
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    i32.const 113
    i32.store8
    global.get $heap_ptr
    i32.const 6
    i32.add
    i32.const 117
    i32.store8
    global.get $heap_ptr
    i32.const 7
    i32.add
    i32.const 97
    i32.store8
    global.get $heap_ptr
    i32.const 8
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 9
    i32.add
    i32.const 105
    i32.store8
    global.get $heap_ptr
    i32.const 10
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 11
    i32.add
    i32.const 103
    i32.store8
    global.get $heap_ptr
    i32.const 12
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 13
    i32.add
    i32.const 116
    i32.store8
    global.get $heap_ptr
    i32.const 14
    i32.add
    i32.const 104
    i32.store8
    global.get $heap_ptr
    i32.const 15
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 16
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 17
    i32.add
    i32.const 102
    i32.store8
    global.get $heap_ptr
    i32.const 18
    i32.add
    i32.const 105
    i32.store8
    global.get $heap_ptr
    i32.const 19
    i32.add
    i32.const 108
    i32.store8
    global.get $heap_ptr
    i32.const 20
    i32.add
    i32.const 116
    i32.store8
    global.get $heap_ptr
    i32.const 21
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 22
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 23
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 24
    i32.add
    i32.const 100
    i32.store8
    global.get $heap_ptr
    i32.const 25
    i32.add
    i32.const 32
    i32.store8
    global.get $heap_ptr
    i32.const 26
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 27
    i32.add
    i32.const 117
    i32.store8
    global.get $heap_ptr
    i32.const 28
    i32.add
    i32.const 109
    i32.store8
    global.get $heap_ptr
    i32.const 29
    i32.add
    i32.const 98
    i32.store8
    global.get $heap_ptr
    i32.const 30
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 31
    i32.add
    i32.const 114
    i32.store8
    global.get $heap_ptr
    i32.const 32
    i32.add
    i32.const 115
    i32.store8
    global.get $heap_ptr
    i32.const 33
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 34
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 35
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 36
    i32.add
    i32.const 10
    i32.store8
    global.get $heap_ptr
    i32.const 37
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
    drop
    global.get $map
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    f64.const 6.0
    local.get $new_env_addr
    f64.convert_i32_u
    call $std_cons
    global.get $filtered
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_2)
    global.set $squared
    global.get $squared
    drop
    global.get $print-list
    local.set $closure_0
    local.get $closure_0
    call $std_cdr
    global.get $squared
    local.get $closure_0
    call $std_car
    i32.trunc_f64_u
    call_indirect (type $type_1)
    drop
    global.get $heap_ptr
    global.get $heap_ptr
    i32.const 6
    i32.store
    global.get $heap_ptr
    i32.const 4
    i32.add
    i32.const 68
    i32.store8
    global.get $heap_ptr
    i32.const 5
    i32.add
    i32.const 111
    i32.store8
    global.get $heap_ptr
    i32.const 6
    i32.add
    i32.const 110
    i32.store8
    global.get $heap_ptr
    i32.const 7
    i32.add
    i32.const 101
    i32.store8
    global.get $heap_ptr
    i32.const 8
    i32.add
    i32.const 46
    i32.store8
    global.get $heap_ptr
    i32.const 9
    i32.add
    i32.const 10
    i32.store8
    global.get $heap_ptr
    i32.const 10
    i32.add
    global.set $heap_ptr
    f64.convert_i32_u
    call $princ
    f64.const 0.0
  )
  (func $entry (export "main") (result f64)
    (local $env f64)
    (local $closure_0 f64)
    (local $closure_1 f64)
    (local $closure_2 f64)
    (local $closure_3 f64)
    (local $closure_4 f64)
    (local $closure_5 f64)
    (local $closure_6 f64)
    (local $closure_7 f64)
    (local $closure_8 f64)
    (local $closure_9 f64)
    (local $closure_10 f64)
    (local $closure_11 f64)
    (local $closure_12 f64)
    (local $closure_13 f64)
    (local $closure_14 f64)
    (local $closure_15 f64)
    (local $closure_16 f64)
    (local $closure_17 f64)
    (local $closure_18 f64)
    (local $closure_19 f64)
f64.const 0.0
local.set $env
    ;; Init range
f64.const 0.0
f64.const 0.0
call $std_cons
global.set $range
    ;; Init map
f64.const 1.0
f64.const 0.0
call $std_cons
global.set $map
    ;; Init filter
f64.const 2.0
f64.const 0.0
call $std_cons
global.set $filter
    ;; Init print-list
f64.const 3.0
f64.const 0.0
call $std_cons
global.set $print-list
    ;; Init start
f64.const 4.0
f64.const 0.0
call $std_cons
global.set $start
global.get $start
local.set $closure_0
local.get $closure_0
call $std_cdr
local.get $closure_0
call $std_car
i32.trunc_f64_u
call_indirect (type $type_0)
  )
)