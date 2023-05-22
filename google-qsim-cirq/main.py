import cirq
import qsimcirq
import numpy as np
import sys

from cirq.contrib.qasm_import import circuit_from_qasm

# Pick up to ~25 qubits to simulate (requires ~256MB of RAM)
#qubits = [cirq.GridQubit(i,j) for i in range(2) for j in range(2)]

# Define a circuit to run
# (Example is from the 2019 "Quantum Supremacy" experiement)
#circuit = (cirq.experiments.
#    random_rotations_between_grid_interaction_layers_circuit(
#    qubits=qubits, depth=16))

f = open(sys.argv[1])
circuit = circuit_from_qasm(f.read())

print(cirq.qasm(circuit))

# Measure qubits at the end of the circuit
#circuit.append(cirq.measure(*qubits, key='all_qubits'))
qubits = circuit.all_qubits()
circuit.append(cirq.measure(*qubits))

# Simulate the circuit with qsim and return just the measurement values
# just like you would with Cirq
qsim_simulator = qsimcirq.QSimSimulator()
result = qsim_simulator.simulate(circuit)
print(np.around(result.final_state_vector, 3))
