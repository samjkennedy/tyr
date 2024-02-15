use std::io;

use crate::type_checker::{CheckedExpression, CheckedStatement, Module};

pub trait Emitter {
    fn emit_module(&mut self, module: Module) -> io::Result<()>;
    fn emit_statement(&mut self, statement: &CheckedStatement) -> io::Result<()>;
    fn emit_expression(&mut self, expression: &CheckedExpression) -> io::Result<()>;
}
