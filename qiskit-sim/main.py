import qiskit
# from qiskit import QuantumCircuit
import sys
import time
import numpy
import resource
import argparse
import multiprocessing
NCPU = multiprocessing.cpu_count()

parser = argparse.ArgumentParser()
parser.add_argument('FILE', help='input .qasm file')
parser.add_argument('-t', '--threads', dest='threads', type=int, default=NCPU, help='number of CPU threads (defaults to max)')
parser.add_argument('--no-fingerprint', dest='no_fingerprint', action='store_true')
args = parser.parse_args()

circ = qiskit.QuantumCircuit.from_qasm_file(args.FILE)

sim = qiskit.Aer.get_backend('aer_simulator_statevector')
sim.set_options(max_parallel_threads=args.threads)
circ = qiskit.transpile(circ, sim)
# print(circ.qasm())
circ.save_statevector()

t0 = time.time()
result = sim.run(circ).result().get_statevector(circ).data
t1 = time.time()
print(f"time {t1-t0} s")

maxrss_kb = resource.getrusage(resource.RUSAGE_SELF)[2]
print(f"maxrss {maxrss_kb} kb")

def basisidx(i):
  maxlen = len(bin(len(result)-1)) - 2
  bidx = bin(i)[2:]
  return bidx.rjust(maxlen, '0')

def fmt_complex(c):
  mid = '-' if c.imag < 0 else '+'
  aimag = abs(c.imag)
  return f'{c.real:.8f}{mid}{aimag:.8f}i'

if args.no_fingerprint:
  pass
else:
  ### TODO: faster method than stable sort??
  neg_mags = -numpy.round(numpy.abs(result), 8)
  ind = numpy.argsort(neg_mags, kind='stable')
  limit = min(10, len(result))
  for fpi in range(0, limit):
    i = ind[fpi]
    if -neg_mags[i] >= 0.00000001:
      print('fp{} {} {}'.format(fpi, basisidx(i), fmt_complex(result[i])))
