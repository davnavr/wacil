(module
  (func (export "nthTriangleNumber") (param $n i32) (result i64)
    (local $acc i64)
    (local $n_ i64)
    block $CALCULATE
      block $CASE_5
        block $CASE_4
          block $CASE_3
            block $CASE_2
              block $CASE_1
                block $CASE_0
                  local.get $n
                  br_table $CASE_0 $CASE_1 $CASE_2 $CASE_3 $CASE_4 $CASE_5 $CALCULATE
                end
                i64.const 0
                return
              end
              i64.const 1
              return
            end
            i64.const 3
            return
          end
          i64.const 6
          return
        end
        i64.const 10
        return
      end
      i64.const 15
      return
    end

    local.get $n
    i64.extend_i32_u
    local.set $n_
    block $CALCULATE_END
      loop $CALCULATE_LOOP
        local.get $n_
        i64.eqz
        br_if $CALCULATE_END

        local.get $acc
        local.get $n_
        i64.add
        local.set $acc

        local.get $n_
        i64.const 1
        i64.sub
        local.set $n_

        br $CALCULATE_LOOP
      end
    end
    local.get $acc
    return))
