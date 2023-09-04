pub mod bfs_simulator;
mod state;
mod state_expander;
mod table;

#[derive(Debug)]
pub enum SimulatorError {
    TooManyQubits,
}
