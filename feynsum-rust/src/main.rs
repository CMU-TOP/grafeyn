mod circuit;
mod config;
mod fingerprint;
mod futhark;
mod gate_scheduler;
mod options;
mod parser;
mod simulator;
mod types;
mod utility;

use parser::QasmStatement;
use rayon::ThreadPoolBuilder;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::sync::{atomic::AtomicU64, RwLock};
use structopt::StructOpt;

use circuit::Circuit;
use config::Config;
use fingerprint::Fingerprint;
use options::Options;
use simulator::{Compactifiable, Simulator};
use types::{AtomicBasisIdx, BasisIdx, BasisIdx64, BasisIdxUnlimited, Complex};

#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

const BASIS_IDX_64_OKAY_THRESHOLD: usize = 62;

fn main() -> io::Result<()> {
    env_logger::init();

    let options = Options::from_args();

    log::info!("input file: {}", options.input.display());
    if let Some(output) = &options.output {
        log::info!("output file: {}", output.display());
    } else {
        log::info!("output file: none");
    }
    log::info!("gate scheduling policy: {}", options.gate_schduling_policy);
    log::info!("dense threshold: {}", options.dense_threshold);
    log::info!("pull threshold: {}", options.pull_threshold);
    log::info!("parallelism: {}", options.parallelism);
    log::info!("block size: {}", options.block_size);

    let source = fs::read_to_string(&options.input)?;

    let config = Config::new(&options);

    let program = match parser::parse_program(&source) {
        Ok(program) => program,
        Err(err) => {
            panic!("Failed to parse program: {:?}", err);
        }
    };
    log::info!("parse complete. starting circuit construction.");

    if circuit::num_qubits(&program) <= BASIS_IDX_64_OKAY_THRESHOLD {
        build_circuit_and_run::<BasisIdx64, AtomicU64>(options, config, program)
    } else {
        build_circuit_and_run::<BasisIdxUnlimited, RwLock<BasisIdxUnlimited>>(
            options, config, program,
        )
    }
}

fn build_circuit_and_run<B: BasisIdx, AB: AtomicBasisIdx<B>>(
    options: Options,
    config: Config,
    program: Vec<QasmStatement>,
) -> io::Result<()> {
    let circuit = match Circuit::<B>::new(program) {
        Ok(circuit) => circuit,
        Err(err) => {
            panic!("Failed to construct circuit: {:?}", err);
        }
    };

    log::info!("circuit construction complete. starting simulation");

    let num_qubits = circuit.num_qubits;

    ThreadPoolBuilder::new()
        .num_threads(options.parallelism)
        .build_global()
        .unwrap();

    let result = run::<B, AB>(&options, config, circuit);

    process_output(result, options.output, num_qubits)?;

    log::info!("simulation complete");

    Ok(())
}

fn run<B: BasisIdx, AB: AtomicBasisIdx<B>>(
    options: &Options,
    config: Config,
    circuit: Circuit<B>,
) -> Box<dyn Iterator<Item = (B, Complex)>> {
    match options.simulator {
        Simulator::Sequential => {
            log::info!("using sequential simulator");
            simulator::sequential_simulator::run::<B>(&config, circuit).compactify()
        }
        Simulator::Parallel => {
            log::info!("using parallel simulator");
            simulator::parallel_simulator::run::<B, AB>(&config, circuit).compactify()
        }
        Simulator::Dense => {
            log::info!("using dense simulator");
            simulator::dense_simulator::run(&config, circuit).compactify()
        }
    }
}

fn process_output<B: BasisIdx>(
    densities: Box<dyn Iterator<Item = (B, Complex)>>,
    output: Option<PathBuf>,
    bidx_width: usize,
) -> io::Result<()> {
    if let Some(path) = output.as_ref() {
        log::info!("writing output to file {}", path.display());
    }

    let mut file = output.map(fs::File::create).transpose()?;
    let mut fingerprint = Fingerprint::new(10);

    for (bidx, weight) in densities {
        fingerprint.insert(bidx.clone(), weight);

        if let Some(f) = file.as_mut() {
            f.write_fmt(format_args!(
                "{:0width$} {:.10}\n",
                bidx,
                weight,
                width = bidx_width,
            ))?;
        }
    }

    println!("computed fingerprint:");
    fingerprint
        .iter()
        .enumerate()
        .for_each(|(idx, (bidx, weight))| {
            println!(
                "fp{idx} {:0width$} {}",
                bidx,
                utility::print_complex(&weight),
                width = bidx_width,
            );
        });

    Ok(())
}
