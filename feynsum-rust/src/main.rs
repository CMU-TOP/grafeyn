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

    let program = parser::parse_program(&source);
    let circuit = Circuit::new(&program);

    let result = simulator::simulate(&circuit);

    Ok(())
}
