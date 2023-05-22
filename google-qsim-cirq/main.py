import cirq
import qsimcirq
import numpy as np
import sys
from cirq.contrib.qasm_import import circuit_from_qasm

if len(sys.argv) < 3:
    print("usage: python main.py <QASMFILE> <NUM_THREADS>")
    sys.exit(1)

filename = sys.argv[1]
numprocs = int(sys.argv[2])

f = open(filename)
circuit = circuit_from_qasm(f.read())
print(cirq.qasm(circuit))

# TODO: Mike: what does this do? Do we need it?
qubits = circuit.all_qubits()
circuit.append(cirq.measure(*qubits))

opt = qsimcirq.QSimOptions(cpu_threads=numprocs, use_gpu=False)
qsim_simulator = qsimcirq.QSimSimulator(opt)
result = qsim_simulator.simulate(circuit)
print(np.around(result.final_state_vector, 3))
