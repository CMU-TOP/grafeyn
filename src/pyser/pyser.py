from qiskit.circuit import QuantumCircuit
from qiskit.compiler import transpile
import sys

# from qiskit.transpiler.passes import SolovayKitaevDecomposition
from qiskit.synthesis import generate_basic_approximations
from qiskit.transpiler.passes.synthesis import SolovayKitaev

# https://qiskit.org/documentation/stubs/qiskit.transpiler.passes.SolovayKitaev.html
def skd(c):
  basis_gates = ['x', 'y', 'z', 'h', 't']
  approx = generate_basic_approximations(basis_gates, depth=3)
  f = SolovayKitaev(recursion_degree=2, basic_approximations=approx)
  return f(c)


def main():
    cfile = sys.argv[1]
    
    c = QuantumCircuit.from_qasm_file(cfile)
    print(c.qasm())
    c2 = transpile(c, basis_gates=['x', 'y', 'z', 'h', 't', 'cx', 'ccx'])
    
    print(c2.qasm())


if __name__ == "__main__":
    main()    
