// Generated from Cirq v1.1.0

OPENQASM 2.0;
include "qelib1.inc";


// Qubits: [addr_0, addr_1, addr_2, qout_0, ram_0, ram_1, ram_2, ram_3, ram_4, ram_5, ram_6, ram_7, rout_0, rout_1, rout_2, rout_3, rout_4, rout_5, rout_6, rout_7]
qreg q[20];
creg m_cout_0[1];


x q[1];
x q[4];
x q[6];
x q[11];
x q[19];
cx q[0],q[15];
cx q[15],q[19];
ccx q[1],q[15],q[13];
cx q[13],q[15];
ccx q[1],q[19],q[17];
cx q[17],q[19];
ccx q[2],q[13],q[12];
ccx q[2],q[15],q[14];
cx q[12],q[13];
ccx q[2],q[17],q[16];
cx q[14],q[15];
ccx q[2],q[19],q[18];
cx q[16],q[17];
cx q[18],q[19];
ccx q[19],q[11],q[3];
ccx q[18],q[10],q[3];
ccx q[17],q[9],q[3];
ccx q[16],q[8],q[3];
ccx q[15],q[7],q[3];
ccx q[14],q[6],q[3];
ccx q[13],q[5],q[3];
ccx q[12],q[4],q[3];
//measure q[3] -> m_cout_0[0];