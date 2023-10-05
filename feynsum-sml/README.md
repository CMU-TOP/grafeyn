# Approach

(Almost) All modern quantum simulation techniques rely on state-vector
simulation.  They represent the quantum states as a vector mapping
basis states to weight and use Schroedinger-style simulation.

Feynman-style simulation has also been explored but is considered a
poor approach because of its massive time complexity.

In this work, we propose an approach to quantum simulation that takes
advantage of sparsity of the state in most quantum computations.

To this end, we pair state-vectors with Feynman-style simulation.  We
represent the quantum state as a sparse state vector and apply Feynman
sums approach to compute the next sparse state vector.

We propose a number of techniques and optimization that exploits the
natural sparsity of the state vectors.




(CHECK: Density matrix representation based simulation)






State vector methods represent the state as a dense vector.

We notice that over the lifetime of a quantum algorithm, the density
of the state space varies over time, usually smoothly, and is usually
sparse.  We therefore represent the quantum state as a sparse vector.
Many quantum simulators are based on Schroedinger technique.
Feynman's technique is a natural fit for sparse state representation,
because it ...

When the state space does get dense, in principle 

