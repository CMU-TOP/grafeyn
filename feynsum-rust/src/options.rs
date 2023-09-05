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
}
