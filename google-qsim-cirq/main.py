import sys

if len(sys.argv) < 3:
    print("usage: python main.py <QASMFILE> <NUM_THREADS> [<PRINT_OUTPUT?>]")
    sys.exit(1)

import cirq
import qsimcirq
import numpy as np
from cirq.contrib.qasm_import import circuit_from_qasm
import time

filename = sys.argv[1]
numprocs = int(sys.argv[2])
doOutput = bool(sys.argv[3]) if len(sys.argv) > 3 else False

f = open(filename)
circuit = circuit_from_qasm(f.read())
print(cirq.qasm(circuit))

# TODO: Mike: what does this do? Do we need it?
qubits = circuit.all_qubits()
circuit.append(cirq.measure(*qubits))

print('running simulator...')
t0 = time.time()
opt = qsimcirq.QSimOptions(cpu_threads=numprocs, use_gpu=False)
qsim_simulator = qsimcirq.QSimSimulator(opt)
result = qsim_simulator.simulate(circuit).final_state_vector
t1 = time.time()
print('simulation completed')
print('elapsed {}s'.format(round(t1-t0, 4)))
# result = np.around(result.final_state_vector, 3)

def basisidx(i):
  maxlen = len(bin(len(result)-1)) - 2
  bidx = bin(i)[2:]
  return bidx.rjust(maxlen, '0')

if doOutput:
  print('computing non-zeros...')
  ind = np.arange(len(result))
  nonzeros = ind[result[ind] != 0j]
  for i in nonzeros:
    print('|{}⟩ {}'.format(basisidx(i), result[i]))
else:
  print('skipping output')
