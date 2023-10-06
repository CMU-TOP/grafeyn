+ Append "fsim" and "sy" to line 1680 of qiskit/circuit/quantumcircuit.py to make qiskit support exporting these gates.
+ Run `python random_test.py -r` to generate test file.
+ Run `cargo run -- --input ../unit-test/random_test.qasm --output ../unit-test/random_test_state_feynsum.txt` to simulate.
+ Run `python random_test.py -c` to compare the results.
