import argparse
import logging as log
import qiskit

from qiskit import QuantumCircuit
from qiskit_braket_provider import BraketLocalBackend

from qiskit.visualization import plot_histogram


def from_file(inputfile):
    c = qiskit.QuantumCircuit.from_qasm_file(inputfile)
    return c


def simple_circuit():
    circuit = QuantumCircuit(3)

    # Apply H-gate to the first qubit:
    circuit.h(0)

    # Apply a CNOT to each qubit:
    for qubit in range(1, 3):
        circuit.cx(0, qubit)

    return circuit
    
# Simulate
def simulate(circuit, outfile):
    local_simulator = BraketLocalBackend()
    print("Simulationg circuit")
    task = local_simulator.run(circuit, shots=100)
    print("Done.")
    plot_histogram(task.result().get_counts(), filename=outfile)

    
def main():

    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", help="input qasm file")
    parser.add_argument("-o", "--output", help="output image file")        
    parser.add_argument("-v", "--verbose", action="store_true")
    
    args = parser.parse_args()    
    inputfile = args.input
    outfile = args.output    
    verbose_mode = args.verbose

    if verbose_mode:
        log.basicConfig(format="%(levelname)s: %(message)s", level=log.INFO)
        log.info("Verbose mode is on")
    else:
        log.basicConfig(format="%(levelname)s: %(message)s", level=log.WARNING)
        
    if inputfile:
        circuit = from_file(inputfile, outfile)
        simulate(circuit, outfile)                        
    else:
        circuit = simple_circuit()
        simulate(circuit, outfile)

if __name__ == "__main__":
    main()                            
