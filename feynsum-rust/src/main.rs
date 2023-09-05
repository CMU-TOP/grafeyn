mod circuit;
mod config;
mod options;
mod parser;
mod simulator;
mod types;
mod utility;

use env_logger;
use log::info;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use structopt::StructOpt;

use circuit::Circuit;
use config::Config;
use options::Options;
use simulator::State;

fn main() -> io::Result<()> {
    env_logger::init();

    let options = Options::from_args();

    info!("Input file name: {}", options.input.display());

    let source = fs::read_to_string(&options.input)?;

    let config = Config::new(); // TODO: use options to set block size, max load, gate scheduling policy, etc.

    let program = match parser::parse_program(&source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Failed to parse program: {:?}", err);
        }
    };
    info!("parse complete");

    let circuit = match Circuit::new(program) {
        Ok(circuit) => circuit,
        Err(err) => {
            panic!("Failed to construct circuit: {:?}", err);
        }
    };

    let result = match simulator::bfs_simulator::run(&config, circuit) {
        Ok(result) => result,
        Err(err) => {
            panic!("Failed to run simulator: {:?}", err);
        }
    };

    if let Some(output) = options.output {
        info!("dumping densities to: {}", output.display());
        dump_densities(&output, result)?;
        info!("output written to: {}", output.display());
    }

    Ok(())
}

fn dump_densities(path: &PathBuf, state: State) -> io::Result<()> {
    let mut file = fs::File::create(path)?;
    for (bidx, weight) in state.compactify() {
        file.write_fmt(format_args!("{} {:.4}\n", bidx, weight))?;
    }
    Ok(())
}
