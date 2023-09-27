mod circuit;
mod config;
mod gate_scheduler;
mod options;
mod parser;
mod simulator;
mod types;
mod utility;

use log::info;
use rayon::ThreadPoolBuilder;
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

    info!("input file: {}", options.input.display());
    if let Some(output) = &options.output {
        info!("output file: {}", output.display());
    } else {
        info!("output file: none");
    }
    info!("gate scheduling policy: {}", options.gate_schduling_policy);
    info!("dense threshold: {}", options.dense_threshold);
    info!("pull threshold: {}", options.pull_threshold);
    info!("parallelism: {}", options.parallelism);
    info!("block size: {}", options.block_size);

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

    let num_qubits = circuit.num_qubits;

    ThreadPoolBuilder::new()
        .num_threads(options.parallelism)
        .build_global()
        .unwrap();

    if options.parallelism > 1 {
        info!("using parallel simulator");
        let result = match simulator::parallel::bfs_simulator::run(&config, circuit) {
            Ok(result) => result,
            Err(err) => {
                panic!("Failed to run simulator: {:?}", err);
            }
        };
        if let Some(output) = options.output {
            info!("dumping densities to: {}", output.display());
            dump_densities(&output, result, num_qubits)?;
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
            dump_densities(&output, result, num_qubits)?;
            info!("output written to: {}", output.display());
        };
    };

    info!("simulation complete");

    Ok(())
}

fn dump_densities(path: &PathBuf, state: impl Compactifiable, bidx_width: usize) -> io::Result<()> {
    let mut file = fs::File::create(path)?;
    for (bidx, weight) in state.compactify() {
        file.write_fmt(format_args!(
            "{:0width$} {:.4}\n",
            bidx,
            weight,
            width = bidx_width,
        ))?;
    }
    Ok(())
}
