from qiskit.circuit import QuantumCircuit
from qiskit.compiler import transpile
import sys


def main():
    cfile = sys.argv[1]
    
    c = QuantumCircuit.from_qasm_file(cfile)
    print(c.qasm())
    c2 = transpile(c, basis_gates=['x', 'y', 'z', 'h', 't', 'cx', 'ccx'])
    
    print(c2.qasm())


if __name__ == "__main__":
    main()    
