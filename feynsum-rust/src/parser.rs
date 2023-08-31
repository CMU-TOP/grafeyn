pub use qasmsim::grammar::ast::{Argument, Expression};
use qasmsim::{
    self,
    grammar::ast::{QuantumOperation, Statement as OpenQasmStatement, UnitaryOperation},
    QasmSimError,
};

pub enum QasmStatement {
    QReg {
        name: String,
        size: usize,
    },
    GateCall {
        name: String,
        params: Vec<Expression>,
        args: Vec<Argument>,
    },
}

pub fn parse_program(source: &str) -> Result<Vec<QasmStatement>, QasmSimError> {
    let open_qasm_program = qasmsim::parse_and_link(source)?;

    Ok(open_qasm_program
        .program
        .into_iter()
        .filter_map(|span| match *span.node {
            OpenQasmStatement::QRegDecl(name, size) => Some(QasmStatement::QReg { name, size }),
            OpenQasmStatement::QuantumOperation(QuantumOperation::Unitary(UnitaryOperation(
                name,
                params,
                args,
            ))) => Some(QasmStatement::GateCall { name, params, args }),
            _ => {
                println!("Ignored unsupported statement: {:?}", *span.node);
                None
            }
        })
        .collect())
}
