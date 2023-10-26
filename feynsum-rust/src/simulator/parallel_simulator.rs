mod state;
mod state_expander;

pub use state::State;

use log::{debug, error, info};

use crate::circuit::Circuit;
use crate::config::Config;
use crate::gate_scheduler;
use crate::profile;
use crate::types::basis_idx::MAX_QUBITS;
use crate::types::BasisIdx;
use crate::types::{Complex, Real};

use super::SimulatorError;
use state::SparseStateTable;
use state_expander::ExpandResult;

pub fn run<B: BasisIdx>(config: &Config, circuit: Circuit<B>) -> Result<State<B>, SimulatorError> {
    let num_gates = circuit.num_gates();
    let num_qubits = circuit.num_qubits;

    if num_qubits > MAX_QUBITS {
        error!("Too many qubits: {}", num_qubits);
        return Err(SimulatorError::TooManyQubits);
    }

    let mut num_gates_visited = 0;
    let mut state = State::Sparse(SparseStateTable::singleton(
        B::zeros(),
        Complex::new(1.0, 0.0),
        config.maxload,
        1,
    )); // initial state
    let mut num_nonzeros = 1;
    let mut num_gate_apps = 0;
    let mut prev_num_nonzeros = 1;

    let gate_touches = circuit
        .gates
        .iter()
        .map(|gate| gate.touches.as_slice())
        .collect();
    let gate_is_branching = circuit
        .gates
        .iter()
        .map(|gate| gate.is_branching())
        .collect();

    let mut gate_scheduler = gate_scheduler::create_gate_scheduler(
        &config.gate_scheduling_policy,
        num_gates,
        num_qubits,
        gate_touches,
        gate_is_branching,
    );

    info!("starting gate application loop.");

    let (duration, _) = profile!(loop {
        let these_gates = gate_scheduler
            .pick_next_gates()
            .into_iter()
            .map(|idx| &circuit.gates[idx])
            .collect::<Vec<_>>();

        debug!("applying gates: {:?}", these_gates);

        if these_gates.is_empty() {
            break;
        }

        let num_gates_visited_here = these_gates.len();

        let (
            duration,
            ExpandResult {
                state: new_state,
                num_nonzeros: new_num_nonzeros,
                num_gate_apps: num_gate_apps_here,
                method,
            },
        ) = profile!(state_expander::expand(
            these_gates,
            config,
            num_qubits,
            prev_num_nonzeros,
            state
        ));

        let density = {
            let max_num_states: u64 = 1 << num_qubits;
            num_nonzeros as Real / max_num_states as Real
        };

        let throughput = (num_gate_apps_here as Real / 1e6) / duration.as_secs_f32();

        println!(
            "gate: {:<3} density: {:.8} nonzero: {:>10} hop: {:<2} {} time: {:.4}s throughput: {:.2}M gates/s",
            num_gates_visited,
            density,
            num_nonzeros,
            num_gates_visited_here,
            method,
            duration.as_secs_f32(),
            throughput
        );

        num_gates_visited += num_gates_visited_here;
        num_gate_apps += num_gate_apps_here;
        prev_num_nonzeros = num_nonzeros;
        num_nonzeros = new_num_nonzeros;
        state = new_state;
    });

    let final_density = {
        let max_num_states: u64 = 1 << num_qubits;
        num_nonzeros as f64 / max_num_states as f64
    };

    println!(
        "gate: {:<2} density: {:.8} nonzero: {:>10}\ngate app count: {}, time: {}s",
        num_gates_visited,
        final_density,
        num_nonzeros,
        num_gate_apps,
        duration.as_secs_f32()
    );

    assert!(num_gates_visited >= num_gates);
    Ok(state)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::types::constants;
    use crate::types::BasisIdx64;
    use approx::abs_diff_eq;

    #[test]
    fn test_run() {
        let config = Config::default();
        // bv_n30.qasm
        let circuit = Circuit::<BasisIdx64>::new(
            parser::parse_program(
                r#"
OPENQASM 2.0;
include "qelib1.inc";
qreg q0[30];
creg c0[30];
h q0[0];
h q0[1];
h q0[2];
h q0[3];
h q0[4];
h q0[5];
h q0[6];
h q0[7];
h q0[8];
h q0[9];
h q0[10];
h q0[11];
h q0[12];
h q0[13];
h q0[14];
h q0[15];
h q0[16];
h q0[17];
h q0[18];
h q0[19];
h q0[20];
h q0[21];
h q0[22];
h q0[23];
h q0[24];
h q0[25];
h q0[26];
h q0[27];
h q0[28];
x q0[29];
h q0[29];
//barrier q0[0],q0[1],q0[2],q0[3],q0[4],q0[5],q0[6],q0[7],q0[8],q0[9],q0[10],q0[11],q0[12],q0[13],q0[14],q0[15],q0[16],q0[17],q0[18],q0[19],q0[20],q0[21],q0[22],q0[23],q0[24],q0[25],q0[26],q0[27],q0[28],q0[29];
cx q0[0],q0[29];
cx q0[4],q0[29];
cx q0[5],q0[29];
cx q0[7],q0[29];
cx q0[8],q0[29];
cx q0[10],q0[29];
cx q0[11],q0[29];
cx q0[13],q0[29];
cx q0[15],q0[29];
cx q0[17],q0[29];
cx q0[21],q0[29];
cx q0[22],q0[29];
cx q0[23],q0[29];
cx q0[24],q0[29];
cx q0[25],q0[29];
cx q0[26],q0[29];
cx q0[27],q0[29];
cx q0[28],q0[29];
//barrier q0[0],q0[1],q0[2],q0[3],q0[4],q0[5],q0[6],q0[7],q0[8],q0[9],q0[10],q0[11],q0[12],q0[13],q0[14],q0[15],q0[16],q0[17],q0[18],q0[19],q0[20],q0[21],q0[22],q0[23],q0[24],q0[25],q0[26],q0[27],q0[28],q0[29];
h q0[0];
h q0[1];
h q0[2];
h q0[3];
h q0[4];
h q0[5];
h q0[6];
h q0[7];
h q0[8];
h q0[9];
h q0[10];
h q0[11];
h q0[12];
h q0[13];
h q0[14];
h q0[15];
h q0[16];
h q0[17];
h q0[18];
h q0[19];
h q0[20];
h q0[21];
h q0[22];
h q0[23];
h q0[24];
h q0[25];
h q0[26];
h q0[27];
h q0[28];
            "#,
            )
            .unwrap(),
        )
        .unwrap();

        let state = run(&config, circuit).unwrap();

        //        println!("{:?}", state);

        //assert!(matches!(state, State::Sparse(SparseStateTable { .. })));

        let table = match state {
            //State::Sparse(table) => table,
            State::Sparse(table) => table,
            _ => panic!(),
        };

        assert_eq!(table.num_nonzeros(), 2);
        let nonzero_field_1 = table
            .get(&BasisIdx64::new("11111111000101010110110110001"))
            .unwrap();
        assert!(abs_diff_eq!(
            nonzero_field_1.re,
            constants::RECP_SQRT_2,
            epsilon = 0.0001
        ));

        let nonzero_field_2 = table
            .get(&BasisIdx64::new("111111111000101010110110110001"))
            .unwrap();
        assert!(abs_diff_eq!(
            nonzero_field_2.re,
            -constants::RECP_SQRT_2,
            epsilon = 0.0001
        ));
    }
}
