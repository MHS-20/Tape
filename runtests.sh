#!/usr/bin/env bash
# Test runner for tape interpreter
# Usage: ./runtests.sh [replay|record]
#   replay - run tests and compare against expected output (default)
#   record - run tests and save output as expected

set -euo pipefail

MODE="${1:-replay}"
TAPE_CMD="sbt --no-colors --batch 'runMain tape.Main'"
PASS=0
FAIL=0
TOTAL=0

expect_dir="tests/expected"
mkdir -p "$expect_dir"

run_test() {
    local name="$1"
    shift
    local cmd=("$@")
    TOTAL=$((TOTAL + 1))
    local expect_file="$expect_dir/$name.out"
    
    echo -n "[$TOTAL] $name ... "
    
    local output
    if output=$("${cmd[@]}" 2>&1); then
        local exitcode=0
    else
        local exitcode=$?
    fi
    
    if [ "$MODE" = "record" ]; then
        echo "$output" > "$expect_file"
        echo "RECORDED"
        return
    fi
    
    if [ -f "$expect_file" ]; then
        if echo "$output" | diff -q - "$expect_file" > /dev/null 2>&1; then
            echo "PASS"
            PASS=$((PASS + 1))
        else
            echo "FAIL"
            echo "  diff:"
            echo "$output" | diff -u "$expect_file" - | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    else
        echo "SKIP (no expected output)"
    fi
}

trap 'echo "---"; echo "Results: $PASS/$TOTAL passed, $FAIL failed"' EXIT

# Run tests
run_test "01-inc"      $TAPE_CMD "run examples/01-inc.tape"
run_test "02-pairs"    $TAPE_CMD "run examples/02-pairs.tape"
run_test "03-magical"  $TAPE_CMD "run examples/03-magical.tape"
run_test "04-eval"     $TAPE_CMD "run examples/04-eval.tape"
