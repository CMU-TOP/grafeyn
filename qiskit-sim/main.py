import qiskit
# from qiskit import QuantumCircuit
import sys
import time
import numpy
import resource

cfile = sys.argv[1]
doOutput = False if len(sys.argv) <= 2 else bool(sys.argv[2])

circ = qiskit.QuantumCircuit.from_qasm_file(cfile)

sim = qiskit.Aer.get_backend('aer_simulator_statevector')
sim.set_options(max_parallel_threads=144)
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

if doOutput:
  mags = numpy.abs(result)
  if len(mags) <= 10:
    for i in range(len(result)):
      if mags[i] > 0.00000001:
        print('{} {}'.format(basisidx(i), fmt_complex(result[i])))
  else:
    ind = numpy.argpartition(mags, -10)[-10:]
    for i in ind:
      if mags[i] > 0.00000001:
        print('{} {}'.format(basisidx(i), fmt_complex(result[i])))
