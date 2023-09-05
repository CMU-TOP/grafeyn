use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "feynsum", about = "Feynsum quantum simulator")]
pub struct Options {
    #[structopt(
        parse(from_os_str),
        name = "input",
        help = "path to the input qasm file"
    )]
    pub input: PathBuf,
    #[structopt(
        parse(from_os_str),
        name = "output",
        help = "output file path to dump densities to. if not specified, densities are not printed"
    )]
    pub output: Option<PathBuf>,
}
