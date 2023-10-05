import argparse
import logging as log
import qiskit

from qiskit import QuantumCircuit
from qiskit_braket_provider import BraketLocalBackend

from qiskit.visualization import plot_histogram


def ppl(heading, text):
    """Pretty print long (multiline) text with heading"""    
    log.info(f"# BEGIN {heading.upper()}")
    log.info(f"{text}")
    log.info(f"# END {heading.upper()}")

    
def pps(text):
    """Pretty print short (single line) text"""        
    log.info(text)

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
    pps("Simulationg circuit")
    task = local_simulator.run(circuit, shots=100)
    pps("Done.")
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
