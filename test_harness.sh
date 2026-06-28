#!/usr/bin/env bash
set -euo pipefail

PASS=0
FAIL=0

run_test() {
    local desc="$1"
    local cmd="$2"
    local expected_file="$3"
    
    echo -n "TEST: $desc ... "
    if output=$(eval "$cmd" 2>&1); then
        if [ -f "$expected_file" ]; then
            if echo "$output" | diff - "$expected_file" > /dev/null 2>&1; then
                echo "PASS"
                PASS=$((PASS + 1))
            else
                echo "FAIL (output differs)"
                echo "--- Expected:"
                cat "$expected_file"
                echo "--- Got:"
                echo "$output"
                FAIL=$((FAIL + 1))
            fi
        else
            echo "PASS (no expected file)"
            PASS=$((PASS + 1))
        fi
    else
        exit_code=$?
        if [ -f "$expected_file" ] && echo "$output" | diff - "$expected_file" > /dev/null 2>&1; then
            echo "PASS (expected error)"
            PASS=$((PASS + 1))
        else
            echo "FAIL (exit code: $exit_code)"
            echo "$output"
            FAIL=$((FAIL + 1))
        fi
    fi
}

trap 'echo "---"; echo "Results: $PASS passed, $FAIL failed"' EXIT