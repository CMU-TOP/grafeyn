#!/bin/bash

# TODO: Refactor and add error handling

contains_nocapture() {
    for arg in "$@"; do
        if [ "$arg" == "--nocapture" ]; then
            return 0
        fi
    done
    return 1
}

remove_nocapture() {
    local new_args=()
    for arg in "$@"; do
        if [ "$arg" != "--nocapture" ]; then
            new_args+=("$arg")
        fi
    done
    echo "${new_args[@]}"
}

# Check if there's at least one argument passed
if [ "$#" -lt 1 ]; then
    echo "Usage: ./run.sh [sml|rust] <other_args>"
    exit 1
fi

# Get the simulator type from the first argument
SIM_TYPE=$1

# Shift to remove the first argument and pass the rest to the simulator
shift

# Run the appropriate simulator based on the first argument


pipenv run python random_test.py -r --output random_test

case $SIM_TYPE in
    sml)
        echo "todo"
        ;;
    rust)
        cd ../feynsum-rust
        if contains_nocapture "$@"; then
            args=$(remove_nocapture "$@")
            cargo run --release -- \
            --input ../unit-test/random_test.qasm \
            --output ../unit-test/random_test_state_feynsum.txt \
            $args
        else
            cargo run --release -- \
            --input ../unit-test/random_test.qasm \
            --output ../unit-test/random_test_state_feynsum.txt \
            "$@" > /dev/null 2>&1
        fi
        cd ../unit-test
        ;;
    *)
        echo "Error: Invalid simulator type. Please choose 'sml' or 'rust'."
        exit 1
        ;;
esac

pipenv run python random_test.py -c

# rm random_test.qasm
# rm random_test_state_feynsum.txt
# rm random_test_state_feynsum.pkl
