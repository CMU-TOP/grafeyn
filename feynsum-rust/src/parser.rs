use qasmsim::{self, grammar::ast::OpenQasmProgram, QasmSimError};

pub type Program = OpenQasmProgram;

pub fn parse_program(source: &str) -> Result<Program, QasmSimError> {
    qasmsim::parse_and_link(source)
}
