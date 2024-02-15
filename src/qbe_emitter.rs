use std::io;

use crate::{
    emitter::Emitter,
    type_checker::{CheckedExpression, CheckedStatement, Module},
};

pub struct QBE_Emitter {}

impl Emitter for QBE_Emitter {
    fn emit_module(&mut self, module: Module) -> io::Result<()> {
        todo!()
    }

    fn emit_statement(&mut self, statement: &CheckedStatement) -> io::Result<()> {
        todo!()
    }

    fn emit_expression(&mut self, expression: &CheckedExpression) -> io::Result<()> {
        todo!()
    }
}

impl QBE_Emitter {}
