mod circuit;
mod config;
mod gate_scheduler;
mod options;
mod parser;
mod simulator;
mod types;
mod utility;

use log::info;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use structopt::StructOpt;

use circuit::Circuit;
use config::Config;
use options::Options;
use simulator::Compactifiable;

fn main() -> io::Result<()> {
    env_logger::init();

    let options = Options::from_args();

    info!("reading file: {}", options.input.display());

    let source = fs::read_to_string(&options.input)?;

    let config = Config::new(&options);

    let program = match parser::parse_program(&source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Failed to parse program: {:?}", err);
        }
    };
    info!("parse complete. starting circuit construction.");

    let circuit = match Circuit::new(program) {
        Ok(circuit) => circuit,
        Err(err) => {
            panic!("Failed to construct circuit: {:?}", err);
        }
    };

    info!("circuit construction complete. starting simulation");

    if options.parallelism {
        info!("using parallel simulator");
        let result = match simulator::parallel::bfs_simulator::run(&config, circuit) {
            Ok(result) => result,
            Err(err) => {
                panic!("Failed to run simulator: {:?}", err);
            }
        };
        if let Some(output) = options.output {
            info!("dumping densities to: {}", output.display());
            dump_densities(&output, result)?;
            info!("output written to: {}", output.display());
        };
    } else {
        info!("using sequential simulator");
        let result = match simulator::sequential::bfs_simulator::run(&config, circuit) {
            Ok(result) => result,
            Err(err) => {
                panic!("Failed to run simulator: {:?}", err);
            }
        };
        if let Some(output) = options.output {
            info!("dumping densities to: {}", output.display());
            dump_densities(&output, result)?;
            info!("output written to: {}", output.display());
        };
    };

    info!("simulation complete");

    Ok(())
}

fn dump_densities(path: &PathBuf, state: impl Compactifiable) -> io::Result<()> {
    let mut file = fs::File::create(path)?;
    for (bidx, weight) in state.compactify() {
        file.write_fmt(format_args!("{} {:.4}\n", bidx, weight))?;
    }
    Ok(())
}
