#!/usr/bin/python3

import sys

pow2 = int(sys.argv[1])
numQubits = 2**pow2

def reverse(xs):
  return [xs[len(xs) - i - 1] for i in range(0, len(xs))]

print("qreg q[{}];".format(numQubits))
for qi in reverse(range(0, numQubits)):
  print("h q[{}];".format(qi))
  for qj in range(0, qi):
    print("cphase(pi / {}) q[{}], q[{}];".format(2**(qj+1), qi-qj-1, qi))