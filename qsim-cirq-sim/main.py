import cirq
from cirq.contrib.qasm_import import circuit_from_qasm
import qsimcirq
import time
import numpy as np
import resource
import argparse
import multiprocessing
NCPU = multiprocessing.cpu_count()

parser = argparse.ArgumentParser()
parser.add_argument('FILE', help='input .qasm file')
parser.add_argument('-t', '--threads', dest='threads', type=int, default=NCPU, help='number of CPU threads (defaults to max)')
parser.add_argument('--no-fingerprint', dest='no_fingerprint', action='store_true')
args = parser.parse_args()

with open(args.FILE, 'r') as f:
  circuit = circuit_from_qasm(f.read())

qubit_ids = sorted(list(circuit.all_qubits()))
qubit_order = reversed(qubit_ids)

opt = qsimcirq.QSimOptions(cpu_threads=args.threads, use_gpu=False)
qsim_simulator = qsimcirq.QSimSimulator(opt)
# qubit_order = cirq.QubitOrder.sorted_by(key=)

print('running simulator...')
t0 = time.time()
result = qsim_simulator.simulate(circuit, qubit_order=qubit_order).final_state_vector
t1 = time.time()
print('simulation completed')
print(f"time {t1-t0} s")

maxrss_kb = resource.getrusage(resource.RUSAGE_SELF)[2]
print(f"maxrss {maxrss_kb} kb")

def basisidx(i):
  maxlen = len(bin(len(result)-1)) - 2
  bidx = bin(i)[2:]
  return "".join(bidx.rjust(maxlen, '0'))

def fmt_complex(c):
  mid = '-' if c.imag < 0 else '+'
  aimag = abs(c.imag)
  return f'{c.real:.8f}{mid}{aimag:.8f}i'

if args.no_fingerprint:
  pass
else:
  ### TODO: faster method than stable sort??
  neg_mags = -np.round(np.abs(result), 8)
  ind = np.argsort(neg_mags, kind='stable')
  limit = min(10, len(result))
  for fpi in range(0, limit):
    i = ind[fpi]
    if -neg_mags[i] >= 0.00000001:
      print('fp{} {} {}'.format(fpi, basisidx(i), fmt_complex(result[i])))

# def isNonzero(x):
#   return abs(x.real) > 1e-8 or abs(x.imag) > 1e-8

# if doOutput:
#   print('computing non-zeros...')
#   ind = np.arange(len(result))
#   nonzeros = ind[result[ind] != 0j]
#   # print('num non-zero {}'.format(len(nonzeros)))
#   for i in reversed(nonzeros):
#     if isNonzero(result[i]):
#       print('|{}⟩ {}'.format(basisidx(i), result[i]))
# else:
#   print('skipping output')
