use std::path::PathBuf;
use structopt::StructOpt;

use crate::gate_scheduler::GateSchedulingPolicy;
use crate::simulator::Simulator;
use crate::types::Real;

#[derive(Debug, StructOpt)]
#[structopt(name = "feynsum", about = "Feynsum quantum simulator")]
pub struct Options {
    #[structopt(
        parse(from_os_str),
        name = "input",
        short = "i",
        long = "input",
        help = "path to the input qasm file"
    )]
    pub input: PathBuf,

    #[structopt(
        parse(from_os_str),
        name = "output",
        short = "o",
        long = "output",
        help = "output file path to dump densities to. if not specified, densities are not printed"
    )]
    pub output: Option<PathBuf>,

    #[structopt(
        name = "gate scheduling policy",
        long = "scheduler",
        short = "s",
        default_value = "naive",
        help = "gate scheduling policy to use"
    )]
    pub gate_schduling_policy: GateSchedulingPolicy,

    #[structopt(long = "dense-threshold", default_value = "0.25")]
    pub dense_threshold: Real,

    #[structopt(long = "pull-threshold", default_value = "0.8")]
    pub pull_threshold: Real,

    #[structopt(
        name = "simulator",
        long = "simulator",
        default_value = "parallel",
        help = "simulator to use"
    )]
    pub simulator: Simulator,

    #[structopt(
        name = "parallelism",
        long = "parallelism",
        default_value = "1",
        help = "number of threads to use",
        required_if("simulator", "parallel")
    )]
    pub parallelism: usize,

    #[structopt(
        name = "block size",
        long = "block-size",
        default_value = "10000",
        help = "block size for parallelism"
    )]
    pub block_size: usize,
}
