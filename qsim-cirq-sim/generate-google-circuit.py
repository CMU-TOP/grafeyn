import cirq
import qsimcirq
import numpy as np
import sys

rows = int(sys.argv[1])
cols = int(sys.argv[2])
depth = int(sys.argv[3])

qubits = [cirq.GridQubit(i,j) for i in range(rows) for j in range(cols)]

circuit = (cirq.experiments.
    random_rotations_between_grid_interaction_layers_circuit(
    qubits=qubits, depth=depth))

print(cirq.qasm(circuit))