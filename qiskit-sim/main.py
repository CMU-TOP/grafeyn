import qiskit
# from qiskit import QuantumCircuit
import sys

cfile = sys.argv[1]
circ = qiskit.QuantumCircuit.from_qasm_file(cfile)

sim = qiskit.Aer.get_backend('aer_simulator_statevector')
sim.set_options(max_parallel_threads=144)
circ = qiskit.transpile(circ, sim)
# print(circ.qasm())
circ.save_statevector()
result = sim.run(circ).result().get_statevector(circ).data

def basisidx(i):
  maxlen = len(bin(len(result)-1)) - 2
  bidx = bin(i)[2:]
  return bidx.rjust(maxlen, '0')

for i in range(len(result)):
  if result[i] != 0j:
    print('|{}⟩ {}'.format(basisidx(i), result[i]))
