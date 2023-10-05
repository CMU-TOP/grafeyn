// Generated from Cirq v1.1.0

OPENQASM 2.0;
include "qelib1.inc";


// Qubits: [q(0, 0), q(0, 1), q(1, 0), q(1, 1)]
qreg q[4];


u2(pi*-0.25, pi*0.25) q[0];
u2(pi*-0.25, pi*0.25) q[1];
u2(pi*-0.25, pi*0.25) q[2];
u2(pi*-0.25, pi*0.25) q[3];
cz q[0],q[2];
ry(pi*0.5) q[0];
sx q[1];
ry(pi*0.5) q[2];
sx q[3];
cz q[1],q[3];
u2(pi*-0.25, pi*0.25) q[0];
ry(pi*0.5) q[1];
sx q[2];
u2(pi*-0.25, pi*0.25) q[3];

