mod circuit;
mod config;
mod options;
mod parser;
mod simulator;
mod state_expander;
mod types;

use std::fs;
use std::io;
use structopt::StructOpt;

use circuit::Circuit;
use config::Config;
use options::Options;

fn main() -> io::Result<()> {
    let options = Options::from_args();
    let source = fs::read_to_string(&options.input)?;

    let config = Config::new();

    let program = match parser::parse_program(&source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Failed to parse program: {:?}", err);
        }
    };
    let circuit = match Circuit::new(program) {
        Ok(circuit) => circuit,
        Err(err) => {
            panic!("Failed to construct circuit: {:?}", err);
        }
    };

    let _result = simulator::run(&config, circuit);

    Ok(())
}
