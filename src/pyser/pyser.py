from qiskit.circuit import QuantumCircuit
from qiskit.compiler import transpile



def main():
    cfile = "../../inputs/ghz_state_n23.qasm"

    c = QuantumCircuit.from_qasm_file(cfile)
    print(c.draw())


if __name__ == "__main__":
    main()    
