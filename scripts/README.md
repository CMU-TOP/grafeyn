For running experiments, there are two key scripts:
  * `gencmds` defines what simulators and inputs are used. To add a simulator
  and/or input, `gencmds` needs to be modified.
  * `runcmds` is used to execute a large batch of experiments. This script
  does NOT need to be modified.

# Overview of `gencmds`

`gencmds` outputs a collection of rows, where each row is a JSON object
describing one shell command that needs to be run. Each row must be a single
line of the output.

For example, if you run it, you'll see a bunch of JSON printed to stdout, one
object per line.
```bash
$ gencmds
{"cmd": "/usr/bin/time -f \"maxrss %M kb\" ./main.mpl @mpl procs 144 set-affinity megablock-threshold 14 cc-threshold-ratio 1.1 collection-threshold-ratio 2.0 max-cc-depth 1 -- -scheduler gfq -input /usr0/home/swestric/proj/quantum-benchmarks/qasm-src/qram_n20.qasm", "cwd": "feynsum-sml", "simulator": "feynsum-sml", "simulator_options": {"scheduler": "gfq"}, "threads": 144, "input": "qram_n20.qasm", "machine": "aware.aladdin.cs.cmu.edu"}
{"cmd": "python3 main.py /usr0/home/swestric/proj/quantum-benchmarks/qasm-src/qram_n20.qasm --threads 144", "cwd": "qiskit-sim", "simulator": "qiskit", "simulator_options": null, "threads": 144, "input": "qram_n20.qasm", "machine": "aware.aladdin.cs.cmu.edu"}
{"cmd": "python3 main.py /usr0/home/swestric/proj/quantum-benchmarks/qasm-src/qram_n20.qasm --threads 144", "cwd": "qsim-cirq-sim", "simulator": "qsim", "simulator_options": null, "threads": 144, "input": "qram_n20.qasm", "machine": "aware.aladdin.cs.cmu.edu"}
...
```

To add another simulator to the experiments, modify `gencmds` so that it
additionally outputs rows for the new simulator. This should be fairly
self-explanatory, e.g., see `def qiskitsim(inputFile, threads): ...` in the
script, and then look for where `qiskitsim(...)` is called.

Each row should have the following fields:
```json
{ 
  "cmd": ...,               // (required, string) shell command to run
  "cwd": ...,               // (required, string) directory to run the command in
  "simulator": ...,         // (required, string) name of the simulator, e.g. "qiskit"
  "threads": ...,           // (required, int) number of threads that this shell command is using
  "input": ...,             // (required, string) .qasm file that this shell command is using
  "simulator_options": ..., // (optional, anything) e.g. could be version number, etc.
  "machine": ...            // (optional, string) name of the machine this command will be run on
}
```

# Running experiments

To run all experiments, do:
```bash
$ gencmds --bench-dir BENCH_DIR --threads THREADS --machine-name MACHINE | runcmds --output RESULTS_FILE
```

For example:
```bash
$ gencmds --bench-dir /usr0/home/swestric/proj/quantum-benchmarks --threads 72 --machine-name aware.aladdin.cs.cmu.edu | runcmds --output ../results/foobar
```

The purpose of `--machine-name MACHINE` is just to keep track of which machine
was used. This is recorded for each entry of the output results file.

`BENCH_DIR` needs to point to a clone of
`git@github.com:CMU-TOP/quantum-benchmarks.git`.

The `runcmds` script will take each JSON row from `gencmds`, run the command
described by the row, and fill out the JSON row with additional info, such as
data printed to stdout/stderr, timestamp, return code, etc. The results are
written to the file given by `--output RESULTS_FILE`.
