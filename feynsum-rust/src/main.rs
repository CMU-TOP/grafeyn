mod circuit;
mod config;
mod expand_state;
mod options;
mod parser;
mod simulator;
mod types;

use std::fs;
use std::io;
use structopt::StructOpt;

use circuit::Circuit;
use options::Options;

fn main() -> io::Result<()> {
    let options = Options::from_args();
    let source = fs::read_to_string(&options.input)?;

    let Ok(program) = parser::parse_program(&source) else {
        panic!("Failed to parse program")
    };
    let Ok(circuit) = Circuit::new(program) else {
        panic!("Failed to construct circuit")
    };

    let _result = simulator::simulate(&circuit);

    Ok(())
}
