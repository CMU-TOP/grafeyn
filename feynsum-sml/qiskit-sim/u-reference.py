
import numpy
import math
from cmath import exp

# https://github.com/Qiskit/qiskit-terra/blob/901e3b89731f9437ccbbe5d9eb5dff657854568f/qiskit/circuit/library/standard_gates/u.py#L117
def f(theta, phi, lam):
  theta, phi, lam = (float(p) for p in [theta,phi,lam])
  cos = math.cos(theta / 2)
  sin = math.sin(theta / 2)
  print('exp(1j * (phi + lam)) = {}'.format(exp(1j * (phi + lam))))
  return numpy.array(
      [
          [cos, -exp(1j * lam) * sin],
          [exp(1j * phi) * sin, exp(1j * (phi + lam)) * cos],
      ],
      dtype=complex
  )