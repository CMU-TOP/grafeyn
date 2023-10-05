// Generated from Cirq v1.1.0

OPENQASM 2.0;
include "qelib1.inc";


// Qubits: [q(0, 0), q(0, 1), q(0, 2), q(0, 3), q(1, 0), q(1, 1), q(1, 2), q(1, 3), q(2, 0), q(2, 1), q(2, 2), q(2, 3), q(3, 0), q(3, 1), q(3, 2), q(3, 3)]
qreg q[16];


ry(pi*0.5) q[0];
sx q[1];
sx q[2];
sx q[3];
u2(pi*-0.25, pi*0.25) q[4];
ry(pi*0.5) q[5];
u2(pi*-0.25, pi*0.25) q[6];
ry(pi*0.5) q[7];
sx q[8];
sx q[9];
ry(pi*0.5) q[10];
ry(pi*0.5) q[11];
u2(pi*-0.25, pi*0.25) q[12];
u2(pi*-0.25, pi*0.25) q[13];
u2(pi*-0.25, pi*0.25) q[14];
ry(pi*0.5) q[15];
cz q[0],q[4];
cz q[2],q[6];
cz q[5],q[9];
cz q[7],q[11];
cz q[8],q[12];
cz q[10],q[14];
sx q[0];
u2(pi*-0.25, pi*0.25) q[1];
ry(pi*0.5) q[2];
u2(pi*-0.25, pi*0.25) q[3];
sx q[4];
sx q[5];
sx q[6];
sx q[7];
u2(pi*-0.25, pi*0.25) q[8];
ry(pi*0.5) q[9];
sx q[10];
sx q[11];
ry(pi*0.5) q[12];
sx q[13];
sx q[14];
sx q[15];

