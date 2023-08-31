pub use qasmsim::grammar::ast::{Argument, Expression, OpCode};
use qasmsim::{
    self,
    grammar::ast::{QuantumOperation, Statement as OpenQasmStatement, UnitaryOperation},
    QasmSimError,
};

#[derive(Debug)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let source = r#"
        OPENQASM 2.0;
        include "qelib1.inc";
        qreg q[4];
        creg c[4];
        x q[3];
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        cx q[0],q[3];
        cx q[1],q[3];
        cx q[2],q[3];
        h q[0];
        h q[1];
        h q[2];
        h q[3];
        measure q[0] -> c[0];
        measure q[1] -> c[1];
        measure q[2] -> c[2];
        measure q[3] -> c[3];
        "#;

        let program = parse_program(&source).unwrap();

        println!("{:?}", program);
    }
}
