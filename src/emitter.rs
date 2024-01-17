use std::{
    fs::File,
    io::{self, Write},
};

use crate::{
    parser::BinaryOpKind,
    type_checker::{
        CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedStatementKind, TypeKind,
    },
};

pub struct CEmitter {
    out_file: File,
}

impl CEmitter {
    pub fn new(out_file: File) -> CEmitter {
        return CEmitter { out_file };
    }

    pub fn emit_program(&mut self, statements: Vec<CheckedStatement>) -> io::Result<()> {
        //preamble
        self.out_file.write(b"#include <stdio.h>\n")?;
        self.out_file.write(b"#include <stdbool.h>\n")?;
        self.out_file.write(b"#include <stdint.h>\n")?;
        self.out_file.write(b"#include <stdlib.h>\n")?;
        self.out_file.write(
            b"#
        #define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
        
        #define SAFE_EXIT(message) \
            do                     \
            {                      \
                perror(message);   \
                exit(1);           \
            } while (0)
        
        #define SAFE_ACCESS_ARRAY(arr, index)                              \
            ({                                                             \
                typeof(arr[0]) _result;                                    \
                if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))     \
                {                                                          \
                    fprintf(stderr, \"Error: Array index out of bounds\\n\"); \
                    SAFE_EXIT(\"Exiting due to array index out of bounds\"); \
                }                                                          \
                _result = (arr)[index];                                    \
                _result;                                                   \
            })
        
        #define SAFE_SET_ARRAY(arr, index, value)                          \
            do                                                             \
            {                                                              \
                if ((index) < 0 || (size_t)(index) >= ARRAY_SIZE(arr))     \
                {                                                          \
                    fprintf(stderr, \"Error: Array index out of bounds\\n\"); \
                    SAFE_EXIT(\"Exiting due to array index out of bounds\"); \
                }                                                          \
                (arr)[index] = value;                                      \
            } while (0)\n",
        )?;
        for statement in statements {
            self.emit_statement(&statement)?;
        }
        Ok(())
    }

    fn emit_statement(&mut self, statement: &CheckedStatement) -> io::Result<()> {
        match &statement.kind {
            CheckedStatementKind::Block { statements } => {
                writeln!(self.out_file, "{{")?;
                for statement in statements {
                    self.emit_statement(statement)?;
                }
                writeln!(self.out_file, "}}")?;
            }
            CheckedStatementKind::Expression { expression } => {
                self.emit_expression(expression)?;
                writeln!(self.out_file, ";")?;
            }
            CheckedStatementKind::VariableDeclaration {
                name,
                type_kind,
                initialiser,
            } => {
                match type_kind {
                    TypeKind::Array(size, el_type) => {
                        write!(
                            self.out_file,
                            "{} {}[{}] = ",
                            Self::get_c_type(el_type),
                            name,
                            size
                        )?;
                    }
                    _ => {
                        write!(self.out_file, "{} {} = ", Self::get_c_type(type_kind), name)?;
                    }
                }

                self.emit_expression(initialiser)?;
                writeln!(self.out_file, ";")?;
            }
            CheckedStatementKind::FunctionDeclaration {
                name,
                args,
                return_type,
                body,
            } => {
                if name == "main" {
                    write!(self.out_file, "int main() {{")?;
                    if let CheckedStatementKind::Block { statements } = &body.kind {
                        for statement in statements {
                            self.emit_statement(statement)?;
                        }
                        writeln!(self.out_file, "return 0;")?;
                        writeln!(self.out_file, "}}")?;
                    }
                    return Ok(());
                }

                match return_type {
                    Some(return_type) => write!(
                        self.out_file,
                        "{} {}( ",
                        Self::get_c_type(return_type),
                        name
                    )?,
                    None => write!(self.out_file, "void {}( ", name)?,
                }

                for (i, arg) in args.iter().enumerate() {
                    write!(
                        self.out_file,
                        "{} {}",
                        Self::get_c_type(&arg.type_kind),
                        arg.name
                    )?;
                    if i < args.len() - 1 {
                        write!(self.out_file, ", ")?;
                    }
                }
                write!(self.out_file, ")")?;

                self.emit_statement(&body)?;
            }
            CheckedStatementKind::If {
                condition,
                body,
                else_body,
            } => {
                write!(self.out_file, "if (")?;
                self.emit_expression(condition)?;
                write!(self.out_file, ")")?;
                self.emit_statement(body)?;

                if let Some(else_body) = else_body {
                    write!(self.out_file, " else ")?;
                    self.emit_statement(else_body)?;
                }
            }
            CheckedStatementKind::While { condition, body } => {
                write!(self.out_file, "while (")?;
                self.emit_expression(condition)?;
                write!(self.out_file, ")")?;
                self.emit_statement(body)?;
            }
            CheckedStatementKind::Return { return_value } => {
                write!(self.out_file, "return ")?;
                if let Some(return_value) = return_value {
                    self.emit_expression(return_value)?;
                }
                writeln!(self.out_file, ";")?;
            }
            CheckedStatementKind::Record { name, members } => {
                writeln!(self.out_file, "typedef struct {} {{", name)?;
                for member in members {
                    writeln!(
                        self.out_file,
                        "{} {};",
                        Self::get_c_type(&member.type_kind),
                        member.name
                    )?;
                }
                writeln!(self.out_file, "}} {};", name)?;
            }
            CheckedStatementKind::Break => writeln!(self.out_file, "break;")?,
            CheckedStatementKind::Continue => writeln!(self.out_file, "continue;")?,
        }
        Ok(())
    }

    fn emit_expression(&mut self, expression: &CheckedExpression) -> io::Result<()> {
        match &expression.kind {
            CheckedExpressionKind::BoolLiteral { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::U8Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::U16Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::U32Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::U64Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::I8Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::I16Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::I32Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::I64Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::F32Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::F64Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::Binary { left, op, right } => {
                self.emit_expression(left)?;
                match op {
                    BinaryOpKind::Add => write!(self.out_file, " + ")?,
                    BinaryOpKind::Sub => write!(self.out_file, " - ")?,
                    BinaryOpKind::Mul => write!(self.out_file, " * ")?,
                    BinaryOpKind::Div => write!(self.out_file, " / ")?,
                    BinaryOpKind::Rem => write!(self.out_file, " % ")?,
                    BinaryOpKind::And => write!(self.out_file, " && ")?,
                    BinaryOpKind::Or => write!(self.out_file, " || ")?,
                    BinaryOpKind::Xor => write!(self.out_file, " ^ ")?,
                    BinaryOpKind::Eq => write!(self.out_file, " == ")?,
                    BinaryOpKind::Lt => write!(self.out_file, " < ")?,
                    BinaryOpKind::LtEq => write!(self.out_file, " <= ")?,
                    BinaryOpKind::Gt => write!(self.out_file, " > ")?,
                    BinaryOpKind::GtEq => write!(self.out_file, " >= ")?,
                    BinaryOpKind::NEq => write!(self.out_file, " != ")?,
                }
                self.emit_expression(right)?;

                Ok(())
            }
            CheckedExpressionKind::Parenthesised { expression } => {
                write!(self.out_file, "(")?;
                self.emit_expression(expression)?;
                write!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpressionKind::Variable { name } => write!(self.out_file, "{}", name),
            CheckedExpressionKind::Assignment { lhs, rhs } => {
                if let CheckedExpressionKind::ArrayIndex { array, index } = &lhs.kind {
                    writeln!(self.out_file, "SAFE_SET_ARRAY(")?;
                    self.emit_expression(&array)?;
                    writeln!(self.out_file, ", ")?;
                    self.emit_expression(&index)?;
                    writeln!(self.out_file, ", ")?;
                    self.emit_expression(&rhs)?;
                    writeln!(self.out_file, ")")?;
                    return Ok(());
                }

                self.emit_expression(lhs)?;
                write!(self.out_file, " = ")?;
                self.emit_expression(rhs)?;
                Ok(())
            }
            CheckedExpressionKind::FunctionCall { name, args } => {
                write!(self.out_file, "{}(", name)?;

                if name == "printf" {
                    write!(
                        self.out_file,
                        "\"{}\\n\", ",
                        Self::get_print_format_for_type(&args[0].type_kind)
                    )?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }

                for (i, arg) in args.iter().enumerate() {
                    self.emit_expression(arg)?;
                    if i < args.len() - 1 {
                        write!(self.out_file, ", ")?;
                    }
                }
                write!(self.out_file, ")")?;

                Ok(())
            }
            CheckedExpressionKind::ArrayLiteral { elements } => {
                writeln!(self.out_file, "{{")?;
                for (i, element) in elements.iter().enumerate() {
                    self.emit_expression(element)?;

                    if i < elements.len() - 1 {
                        write!(self.out_file, ", ")?;
                    }
                }
                writeln!(self.out_file, "}}")?;

                Ok(())
            }
            CheckedExpressionKind::ArrayIndex { array, index } => {
                writeln!(self.out_file, "SAFE_ACCESS_ARRAY(")?;
                self.emit_expression(&array)?;
                writeln!(self.out_file, ", ")?;
                self.emit_expression(&index)?;
                writeln!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpressionKind::RecordLiteral { arguments } => {
                writeln!(
                    self.out_file,
                    "({}){{",
                    Self::get_c_type(&expression.type_kind)
                )?;
                for (i, arg) in arguments.iter().enumerate() {
                    self.emit_expression(arg)?;

                    if i < arguments.len() - 1 {
                        write!(self.out_file, ", ")?;
                    }
                }
                writeln!(self.out_file, "}}")?;
                Ok(())
            }
            CheckedExpressionKind::Accessor { accessee, member } => {
                self.emit_expression(&accessee)?;
                write!(self.out_file, ".{}", member)?;
                Ok(())
            }
        }
    }

    fn get_c_type(type_kind: &TypeKind) -> String {
        return match type_kind {
            TypeKind::Unit => "void".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::Record(name, _) => name.to_string(),
            TypeKind::U8 => "unsigned char".to_string(),
            TypeKind::U16 => "unsigned short".to_string(),
            TypeKind::U32 => "unsigned long".to_string(),
            TypeKind::U64 => "unsigned long long".to_string(),
            TypeKind::I8 => "char".to_string(),
            TypeKind::I16 => "short".to_string(),
            TypeKind::I32 => "long".to_string(),
            TypeKind::I64 => "long long".to_string(),
            TypeKind::F32 => "float".to_string(),
            TypeKind::F64 => "double".to_string(),
            TypeKind::Array(el_count, el_type) => {
                format!("{} [{}]", Self::get_c_type(el_type), el_count)
            }
        };
    }

    fn get_print_format_for_type(type_kind: &TypeKind) -> String {
        return match type_kind {
            TypeKind::Unit => panic!("can't format void!"),
            TypeKind::Bool => "%d".to_string(),
            TypeKind::Array(_, _) => todo!(),
            TypeKind::Record(_, _) => todo!(),
            TypeKind::U8 => "%hhu".to_string(),
            TypeKind::U16 => "%hu".to_string(),
            TypeKind::U32 => "%lu".to_string(),
            TypeKind::U64 => "%llu".to_string(),
            TypeKind::I8 => "%hhi".to_string(),
            TypeKind::I16 => "%hi".to_string(),
            TypeKind::I32 => "%li".to_string(),
            TypeKind::I64 => "%lli".to_string(),
            TypeKind::F32 => "%.2f".to_string(),
            TypeKind::F64 => "%.6lf".to_string(),
        };
    }
}
