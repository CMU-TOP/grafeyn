from qiskit import QuantumCircuit
from qiskit.circuit.library import (
    HGate,
    YGate,
    ZGate,
    TGate,
    XGate,
    SXGate,
    CXGate,
    CZGate,
    CCXGate,
    CPhaseGate,
    RYGate,
    RZGate,
    CSwapGate,
    SwapGate,
    U3Gate,
    U2Gate,
    U1Gate,
    UGate,
    SGate,
)
import numpy as np
import argparse
from qiskit.quantum_info import Statevector
import pickle
from typing import Optional, Union
import numpy
from qiskit.circuit.controlledgate import ControlledGate
from qiskit.circuit.gate import Gate
from qiskit.circuit.quantumregister import QuantumRegister
from qiskit.circuit._utils import with_gate_array, with_controlled_gate_array
from qiskit.circuit.parameterexpression import ParameterValueType
from qiskit.extensions import UnitaryGate
from qiskit.circuit.quantumcircuit import QuantumCircuit


class FsimGate(Gate):
    def __init__(
        self,
        theta: ParameterValueType,
        phi: ParameterValueType,
        label: Optional[str] = None,
    ):
        super().__init__("fsim", 2, [theta, phi], label=label)

    def _define(self):
        q = QuantumRegister(2, "q")
        qc = QuantumCircuit(q, name=self.name)

        theta, phi = self.params

        gate = UnitaryGate(
            np.array(
                [
                    [1, 0, 0, 0],
                    [0, np.cos(theta), -1j * np.sin(theta), 0],
                    [0, -1j * np.sin(theta), np.cos(theta), 0],
                    [0, 0, 0, np.exp(1j * phi)],
                ]
            )
        )
        rules = [
            (gate, [q[0], q[1]], []),
        ]
        for instr, qargs, cargs in rules:
            qc._append(instr, qargs, cargs)

        self.definition = qc

    def inverse(self):
        return FsimGate(-self.params[0], -self.params[1])


class SYGate(Gate):
    def __init__(self, label: Optional[str] = None):
        super().__init__("sy", 1, [], label=label)

    def _define(self):
        q = QuantumRegister(1, "q")
        qc = QuantumCircuit(q, name=self.name)
        gate = UnitaryGate(
            1
            / 2
            * np.array(
                [[1 - 1j, 1 - 1j], [-1 + 1j, 1 - 1j]],
            )
        )
        rules = [(gate, [q[0]], [])]
        for operation, qubits, clbits in rules:
            qc._append(operation, qubits, clbits)
        self.definition = qc

    def inverse(self):
        return SYdgGate()


class SYdgGate(Gate):
    def __init__(self, label: Optional[str] = None):
        super().__init__("sydg", 1, [], label=label)

    def _define(self):
        q = QuantumRegister(1, "q")
        qc = QuantumCircuit(q, name=self.name)
        gate = UnitaryGate(
            1
            / 2
            * np.array(
                [[1 + 1j, -1 - 1j], [1 + 1j, 1 + 1j]],
            )
        )
        rules = [(gate, [q[0]], [])]
        for operation, qubits, clbits in rules:
            qc._append(operation, qubits, clbits)
        self.definition = qc

    def inverse(self):
        return SYGate()


def compare():
    with open("random_test_state_qiskit.pkl", "rb") as f:
        statevector = pickle.load(f)
    with open("random_test_state_feynsum.txt", "r") as f:
        output = f.read()
    feynsum_state = np.zeros_like(statevector)
    for line in output.split("\n"):
        if line == "":
            continue
        idx = line.split(" ")[0]
        amp = line.split(" ")[1]
        idx = eval("0b" + idx)
        amp = eval(amp.replace("i", "j"))
        feynsum_state[idx] = amp
    print(
        f"The distance between two results are: {np.linalg.norm(statevector - feynsum_state)}"
    )
    print(
        f"The fidelity of the two results are: {np.abs((statevector*feynsum_state.conj()).sum())}"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--compare", action="store_true")
    parser.add_argument("-r", "--reverse", action="store_true")  # test U U^\dagger
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--n_qubits", type=int, default=5)
    parser.add_argument("--n_gates", type=int, default=100)
    parser.add_argument("--output", type=str, default="random_test")
    args = parser.parse_args()
    if args.compare:
        compare()
    else:
        n_qubits, n_gates, seed = args.n_qubits, args.n_gates, args.seed
        rng = np.random.default_rng(seed)
        # gate_name: (num_qubits, num_params, gate_builder)
        supported_gates = {
            "h": (1, 0, HGate),
            "y": (1, 0, YGate),
            "z": (1, 0, ZGate),
            "t": (1, 0, TGate),
            "x": (1, 0, XGate),
            "s": (1, 0, SGate),
            "sx": (1, 0, SXGate),
            # "sy": (1, 0, SYGate),  #!not implemented by qiskit
            "cx": (2, 0, CXGate),
            "cz": (2, 0, CZGate),
            "ccx": (3, 0, CCXGate),
            "cphase": (2, 1, CPhaseGate),  #!identical to cz
            "ry": (1, 1, RYGate),
            "rz": (1, 1, RZGate),
            "cswap": (3, 0, CSwapGate),
            "swap": (2, 0, SwapGate),
            "u": (1, 3, UGate),
            "u2": (1, 2, U2Gate),
            "u1": (1, 1, U1Gate),
            "fsim": (2, 2, FsimGate),  #!not implemented by qiskit
        }

        circ = QuantumCircuit(n_qubits)
        for i in range(n_gates):
            gate_name = np.random.choice(list(supported_gates.keys()))
            n_used_qubits, n_params, gate_builder = supported_gates[gate_name]
            used_qubits = np.random.choice(
                n_qubits, size=n_used_qubits, replace=False
            ).tolist()
            params = np.random.random(n_params).tolist()
            gate = gate_builder(*params)
            circ.append(gate, used_qubits)
        if args.reverse:
            new_circ = QuantumCircuit(n_qubits)
            new_circ = new_circ.compose(circ)
            new_circ.barrier()
            new_circ = new_circ.compose(circ.inverse())
            circ = new_circ
        qasm = circ.qasm()
        print(qasm)
        statevector = Statevector(circ).data
        print(statevector)
        with open(args.output + ".qasm", "w") as f:
            f.write(qasm)
        with open(args.output + "_state_qiskit.pkl", "wb") as f:
            pickle.dump(statevector, f)
