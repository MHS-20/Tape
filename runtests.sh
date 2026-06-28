#!/usr/bin/env bash
# Test runner for tape interpreter
set -euo pipefail

MODE="${1:-replay}"
PASS=0
FAIL=0
TOTAL=0

expect_dir="tests/expected"
mkdir -p "$expect_dir"

get_output() {
    local subcmd="$1"
    local file="$2"
    sbt --no-colors --batch "run $subcmd $file" 2>&1 | \
        sed -n '/^\[info\] running tape\.Main/,/^\[success\]/p' | \
        grep -v '^\[info\] running' | \
        grep -v '^\[success\]' | \
        grep -v '^$'
    return ${PIPESTATUS[0]}
}

run_test() {
    local name="$1"
    local subcmd="$2"
    local file="$3"
    TOTAL=$((TOTAL + 1))
    local expect_file="$expect_dir/$name.out"
    
    echo -n "[$TOTAL] $name ... "
    
    local output exitcode=0
    output=$(get_output "$subcmd" "$file") || exitcode=$?
    
    if [ "$MODE" = "record" ]; then
        printf '%s\n' "$output" > "$expect_file"
        echo "RECORDED"
        return
    fi
    
    if [ -f "$expect_file" ]; then
        if diff -q <(printf '%s\n' "$output") "$expect_file" > /dev/null 2>&1; then
            echo "PASS"
            PASS=$((PASS + 1))
        else
            echo "FAIL"
            echo "  diff:"
            diff -u "$expect_file" <(printf '%s\n' "$output") | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    else
        echo "SKIP (no expected output)"
    fi
}

trap 'echo "---"; echo "Results: $PASS/$TOTAL passed, $FAIL failed"' EXIT

run_test "01-inc"         "run" "examples/01-inc.tape"
run_test "02-pairs"       "run" "examples/02-pairs.tape"
run_test "03-magical"     "run" "examples/03-magical.tape"
run_test "04-eval"        "run" "examples/04-eval.tape"
run_test "real"           "run" "tests/real.tape"
run_test "string"         "run" "tests/string.tape"
run_test "left-tape"      "run" "tests/left-tape.tape"
run_test "unused-vars"    "run" "tests/unused-vars.tape"
run_test "01-inc-expand"  "expand" "examples/01-inc.tape"
run_test "enum-expand"    "expand --enum" "examples/01-inc.tape"
