# GraFeyn: Efficient Parallel Sparse Simulation of Quantum Circuits

![](https://cs.nyu.edu/~shw8119/images/grafeyn.png)

This repo contains the source code for GraFeyn, a pure quantum circuit
simulator with support for a subset of [OpenQASM](https://openqasm.com/intro.html).
GraFeyn excels at executing "sparse" circuits whose intermediate state vectors
contain a small number of non-zero amplitudes. GraFeyn will perform
gate reorderings (commutations) on-the-fly to encourage more sparsity. More
details are available in the accompanying paper:

> [GraFeyn: Efficient Parallel Sparse Simulation of Quantum Circuits](https://cs.nyu.edu/~shw8119/24/qce24-grafeyn.pdf).
> Sam Westrick, Pengyu Liu, Byeongjee Kang, Colin McDonald, Mike Rainey, Mingkuan Xu, Jatin Arora, Yongshan Ding, and Umut A. Acar.
> [QCE 2024](https://qce.quantum.ieee.org/2024/).

## Input Circuit Format

GraFeyn accepts circuits in the [OpenQASM](https://openqasm.com/intro.html)
format. Only a subset OpenQASM functionality is supported. Circuits
given to GraFeyn should have the following form:

```openqasm
OPENQASM 2.0;
include "qelib1.inc";
// sequence of qreg declarations (example with 2 qubits shown here)
qreg q[2];
...
// sequence of gate applications (a few example gates shown here)
h q[0];
cx q[0], q[1];
...
```

GraFeyn currently supports the following gates:
{`h`, `y`, `z`, `s`, `sdg`, `t`, `tdg`, `x`, `sxdg`, `sx`, `cx`, `cz`, `ccx`, `phase`, `cphase`, `rx`, `ry`, `rz`, `swap`, `cswap`, `u`, `u1`, `u2`, `u3`}.

Non-pure OpenQASM statements (such as `measure`, `barrier`, etc.) are not currently
supported. GraFeyn will silently ignore these statements.

GraFeyn implicitly computes the full final state vector of the simulation.
By default, it then reports a "fingerprint" of the result, consisting
of the top-10 basis indices with largest probability amplitude. GraFeyn can
also dump the final state vector to a file, if desired. (Although, be careful,
this could require a prohibitively large amount of disk space. State vectors can
be **big**.)

## Dependencies

  - GraFeyn has only been tested on `x86_64` Linux, specifically
  Ubuntu. It may work on other systems, but we have not tested this.
  - Install [MPL](https://github.com/mpllang/mpl), v0.5 or later.
  Please refer to the [MPL README](https://github.com/MPLLang/mpl/blob/main/README.md)
  for installation instructions. We recommend using
  [`mpl-switch`](https://github.com/mpllang/mpl-switch) to select and install
  the most recent version of MPL.
      - MPL is a parallel functional language actively being developed by
      researchers at CMU, RIT, and NYU.
      - If you have trouble with MPL, you can instead use
      [MLton](https://github.com/mlton/mlton). The performance with MLton will
      be slower due to lack of parallelism. (MLton is a robust, mature compiler,
      with excellent sequential performance but no support for multi-core execution.)
  - [`smlpkg`](https://github.com/diku-dk/smlpkg)
  - For our benchmarking scripts, you will need Python 3 and [`jq`](https://jqlang.github.io/jq/)

## Build and Run

To build and run using MPL (with parallel multi-core execution):
```bash
$ cd feynsum-sml
$ (cd src && smlpkg sync) # only have to do this once
$ make main.mpl           # this will take a minute
$ ./main.mpl @mpl procs <NUMBER_OF_PROCESSORS> -- -input FILE.qasm
```

Alternatively, you can use MLton. (Sequential execution only; no multi-core speedup.)
```bash
$ cd feynsum-sml
$ (cd src && smlpkg sync) # only have to do this once
$ make main.mlton
$ ./main.mlton -input FILE.qasm
```

## Sample Execution

Here's an example on a large sparse circuit. This circuit in particular is
a 280-qubit instance of the
[Bernstein-Vazirani algorithm](https://en.wikipedia.org/wiki/Bernstein%E2%80%93Vazirani_algorithm).

First, we download [`bv_n280.qasm`](https://raw.githubusercontent.com/pnnl/QASMBench/master/large/bv_n280/bv_n280.qasm) from [QASMBench](https://github.com/pnnl/QASMBench):
```
$ wget https://raw.githubusercontent.com/pnnl/QASMBench/master/large/bv_n280/bv_n280.qasm
```

Next we run GraFeyn. It dumps a bunch of info, include a trace of the execution
and a final fingerprint of the result. The fingerprint (reported at the bottom,
with the labels `fp0`, `fp1`, etc.) shows two basis indices and their
probability amplitudes. **Note that this particular circuit only has two non-zero
amplitudes in the final state vector**. GraFeyn reports this in less than a second.

```
$ ./main.mpl @mpl procs 10 -- -input bv_n280.qasm
precision 32
expand-block-size 10000
expand-max-load 0.9
dense-thresh 0.25
pull-thresh 0.8
measure-zeros? no
scheduler gfq
input ../bv_n280.qasm
-------------------------------------
--- scheduler-specific args
-------------------------------------
scheduler-max-branching-stride 2
scheduler-disable-fusion? no
-------------------------------------
-------------------------------
--- input-specific specs
-------------------------------
-------------------------------
gates  712
qubits 280
show-circuit? no
impl lockfree
warmup 0.0000
repeat 1
full-sim-bfs
▎          gate   0 density 0.00000000 nonzero          1 hop   2 push sparse 0.0002s throughput 0.02
▎          gate   2 density 0.00000000 nonzero          1 hop   3 push sparse 0.0004s throughput 0.01
▎          gate   5 density 0.00000000 nonzero          4 hop   3 push sparse 0.0006s throughput 0.03
▎          gate   8 density 0.00000000 nonzero          4 hop   3 push sparse 0.0020s throughput 0.01
▎          gate  11 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate  14 density 0.00000000 nonzero          4 hop   3 push sparse 0.0013s throughput 0.01
▎          gate  17 density 0.00000000 nonzero          4 hop   3 push sparse 0.0012s throughput 0.01
▎          gate  20 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate  22 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate  25 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate  27 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate  30 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate  32 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate  34 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate  37 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate  39 density 0.00000000 nonzero          4 hop   3 push sparse 0.0011s throughput 0.01
▎          gate  42 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate  45 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate  48 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate  51 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate  53 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate  56 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate  59 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate  61 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  63 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate  66 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  68 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate  71 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate  74 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate  76 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  78 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  80 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate  82 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  84 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  86 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate  89 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate  91 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate  93 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate  96 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate  99 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 101 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 103 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 105 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 108 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 110 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 113 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 115 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 117 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 119 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 122 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 125 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 127 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 129 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 132 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 135 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 138 density 0.00000000 nonzero          4 hop   2 push sparse 0.0012s throughput 0.01
▎          gate 140 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 142 density 0.00000000 nonzero          4 hop   3 push sparse 0.0011s throughput 0.01
▎          gate 145 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 148 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 151 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 153 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 156 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 158 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 161 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 164 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 166 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 168 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 170 density 0.00000000 nonzero          4 hop   3 push sparse 0.0013s throughput 0.01
▎          gate 173 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 175 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate 177 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 180 density 0.00000000 nonzero          4 hop   3 push sparse 0.0011s throughput 0.01
▎          gate 183 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 185 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 188 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 191 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 193 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 196 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 198 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 201 density 0.00000000 nonzero          4 hop   2 push sparse 0.0013s throughput 0.01
▎          gate 203 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 206 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 208 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 211 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 214 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 216 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 218 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 221 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 224 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 227 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 229 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 231 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 233 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 236 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 239 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 242 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 245 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 248 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 250 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 253 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 256 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 259 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 261 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 264 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 267 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate 269 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 272 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 275 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 278 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 281 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 283 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 286 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 288 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 290 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 292 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate 294 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 297 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 299 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 302 density 0.00000000 nonzero          4 hop   3 push sparse 0.0011s throughput 0.01
▎          gate 305 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 308 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 311 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 314 density 0.00000000 nonzero          4 hop   3 push sparse 0.0011s throughput 0.01
▎          gate 317 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 320 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 322 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 324 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 327 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 329 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 331 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 334 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 336 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate 338 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 341 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 343 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 345 density 0.00000000 nonzero          4 hop   2 push sparse 0.0011s throughput 0.01
▎          gate 347 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 349 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 351 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 354 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 357 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 360 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 363 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 365 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 368 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 370 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 372 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 375 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 377 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 379 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 381 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 383 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 385 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 388 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 390 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 392 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 394 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 397 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 400 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 403 density 0.00000000 nonzero          4 hop   3 push sparse 0.0006s throughput 0.03
▎          gate 406 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 409 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 411 density 0.00000000 nonzero          4 hop   2 push sparse 0.0010s throughput 0.01
▎          gate 413 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 416 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 418 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 421 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 423 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 425 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 428 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 430 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 432 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 435 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 438 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 440 density 0.00000000 nonzero          4 hop   3 push sparse 0.0006s throughput 0.03
▎          gate 443 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 445 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 448 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 450 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 452 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 455 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 458 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 460 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 463 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 466 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 469 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 472 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 474 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 476 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 479 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 482 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 485 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 488 density 0.00000000 nonzero          4 hop   3 push sparse 0.0010s throughput 0.02
▎          gate 491 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 493 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 495 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 497 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 499 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 501 density 0.00000000 nonzero          4 hop   3 push sparse 0.0006s throughput 0.03
▎          gate 504 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 506 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 508 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 511 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 513 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 516 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 519 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 521 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 524 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 526 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 529 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 532 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 534 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 536 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 538 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 540 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 543 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 545 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 548 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 551 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 553 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.01
▎          gate 555 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 558 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 560 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 563 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 566 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 568 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 571 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 574 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 577 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 580 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 583 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 586 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 589 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 592 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 595 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 597 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 599 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 602 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 604 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 607 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 610 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 612 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 615 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 617 density 0.00000000 nonzero          4 hop   2 push sparse 0.0006s throughput 0.02
▎          gate 619 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 621 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 624 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 627 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 629 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 632 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 634 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 637 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 640 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 643 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 645 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 648 density 0.00000000 nonzero          4 hop   3 push sparse 0.0008s throughput 0.02
▎          gate 651 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 654 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 656 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 659 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 661 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 664 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 667 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 669 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 672 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 675 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 677 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 680 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 683 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 686 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 689 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 692 density 0.00000000 nonzero          4 hop   2 push sparse 0.0007s throughput 0.02
▎          gate 694 density 0.00000000 nonzero          4 hop   3 push sparse 0.0009s throughput 0.02
▎          gate 697 density 0.00000000 nonzero          4 hop   2 push sparse 0.0009s throughput 0.01
▎          gate 699 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 702 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 705 density 0.00000000 nonzero          4 hop   2 push sparse 0.0008s throughput 0.02
▎          gate 707 density 0.00000000 nonzero          4 hop   3 push sparse 0.0007s throughput 0.02
▎          gate 710 density 0.00000000 nonzero          4 hop   2 push sparse 0.0003s throughput 0.03
▎          gate 712 density 0.00000000 nonzero          2
gate app count 3944
time 0.2485s

average 0.2485s
total   0.2485s
end-to-end 0.2485s
avg-density 0.000000000000
max-density 0.000000000000
computed fingerprint in 0.0001s
fp0 0110110101111101101101011101110101100010110100111111111011010011010000110101101001000001111100111101100101011001001010011111000100000100101111000001001001001111111010000101111011011101111100011100110101010110110010001101011100111001100010100011001000000110100110111101001010111110 0.70710671+0.00000000i
fp1 1110110101111101101101011101110101100010110100111111111011010011010000110101101001000001111100111101100101011001001010011111000100000100101111000001001001001111111010000101111011011101111100011100110101010110110010001101011100111001100010100011001000000110100110111101001010111110 -0.70710671+0.00000000i
use -output FILE to see output state vector
use -output-densities FILE to see densities
bv_n280,280,712,0.000000000000,0.000000000000
```