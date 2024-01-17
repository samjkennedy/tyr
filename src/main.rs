use std::fs::File;
use std::path::Path;
use std::process::Command;

use emitter::CEmitter;
use lexer::{lex_file, LexError};
use parser::{ParseError, Parser, Statement};
use type_checker::{TypeCheckError, TypeChecker};

pub mod emitter;
pub mod lexer;
pub mod parser;
pub mod rewriter;
pub mod type_checker;

fn main() {
    let file_name = "snippets/with.tyr";

    let tokens = match lex_file(file_name.to_string()) {
        Ok(tokens) => tokens,
        Err(e) => {
            match e {
                LexError::IoError(err) => eprintln!("{}", err),
                LexError::InvalidCharacter(c, loc) => eprintln!(
                    "Invalid character '{}' at {}:{}:{}",
                    c,
                    loc.file,
                    loc.row,
                    loc.col + 1
                ),
            }
            return;
        }
    };

    let mut parser = Parser::new();
    let statements: Vec<Statement> = match parser.parse_statements(tokens) {
        Ok(statements) => statements,
        Err(e) => {
            match e {
                ParseError::UnexpectedToken(tok) => {
                    eprintln!(
                        "Unexpected token \"{}\" ({:?}) at {}:{}:{}",
                        tok.text,
                        tok.kind,
                        tok.loc.file,
                        tok.loc.row,
                        tok.loc.col + 1
                    )
                }
                ParseError::UnexpectedEOF => eprintln!("Unexpected EOF"),
                ParseError::TokenMismatch { expected, actual } => {
                    eprintln!(
                        "Expected token {:?} but got \"{}\" ({:?}) at {}:{}:{}",
                        expected,
                        actual.text,
                        actual.kind,
                        actual.loc.file,
                        actual.loc.row,
                        actual.loc.col + 1
                    )
                }
                ParseError::ExpectedSemicolon(loc) => {
                    eprintln!(
                        "Expected semicolon at {}:{}:{}",
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
            }
            return;
        }
    };

    let mut type_checker = TypeChecker::new();

    let checked_statements = match type_checker.type_check_statements(statements) {
        Ok(checked_statements) => checked_statements,
        Err(e) => {
            match e {
                TypeCheckError::TypeMismatch {
                    expected,
                    actual,
                    loc,
                } => {
                    eprintln!(
                        "Type mismatch, expected {:?} but got {:?} at {}:{}:{}",
                        expected,
                        actual,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
                TypeCheckError::BinaryOpNotImplementedForTypes {
                    left,
                    op,
                    right,
                    loc,
                } => {
                    eprintln!("Binary operation `{:?}` is not implemented for types `{:?} and {:?}` at {}:{}:{}", op, left, right, loc.file, loc.row, loc.col + 1)
                }
                TypeCheckError::VariableAlreadyDeclared { variable, loc } => {
                    eprintln!(
                        "Variable `{:?}` was already declared at {}:{}:{} at {}:{}:{}",
                        variable.name,
                        variable.declaration_loc.file,
                        variable.declaration_loc.row,
                        variable.declaration_loc.col + 1,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::NoSuchVariableDeclaredInScope { name, loc } => {
                    eprintln!(
                        "No such variable `{}` declared in scope at {}:{}:{}",
                        name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::NoSuchTypeDeclaredInScope { name, loc } => {
                    eprintln!(
                        "No such type `{}` declared in scope at {}:{}:{}",
                        name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::NoSuchFunctionDeclaredInScope { name, loc } => {
                    eprintln!(
                        "No such function `{}()` declared in scope at {}:{}:{}",
                        name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::FunctionAlreadyDeclared { function, loc } => {
                    eprintln!(
                        "Function `{}()` is already declared in scope at {}:{}:{}",
                        function.name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::NotAllCodePathsReturnValue { name, loc } => {
                    eprintln!(
                        "Function \"{}\" - Not all code paths return a value at {}:{}:{}",
                        name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::CannotIndexType { type_kind, loc } => {
                    eprintln!(
                        "Type `{:?}`\"` cannot be indexed at {}:{}:{}",
                        type_kind,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::TypeAlreadyDeclared { record_name, loc } => {
                    eprintln!(
                        "Type `{}` is already declared in scope at {}:{}:{}",
                        record_name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    );
                }
                TypeCheckError::MissingArgForRecord {
                    name,
                    type_kind,
                    record_name,
                    loc,
                } => {
                    eprintln!(
                        "Missing argument {}: {:?} for record {} at {}:{}:{}",
                        name,
                        type_kind,
                        record_name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
                TypeCheckError::UnexpectedArgForRecord {
                    type_kind,
                    record_name,
                    loc,
                } => {
                    eprintln!(
                        "Unexpected argument of type {:?} for record {} at {}:{}:{}",
                        type_kind,
                        record_name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
                TypeCheckError::CannotAccessType { type_kind, loc } => {
                    eprintln!(
                        "Cannot call any accessors for type {:?} at {}:{}:{}",
                        type_kind,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
                TypeCheckError::NoSuchMember {
                    member_name,
                    name,
                    loc,
                } => {
                    eprintln!(
                        "No such member {} on record {} at {}:{}:{}",
                        member_name,
                        name,
                        loc.file,
                        loc.row,
                        loc.col + 1
                    )
                }
                TypeCheckError::WithCalledOnNonRecordType(type_kind, loc) => eprintln!(
                    "Cannot use `with` on non-record type `{:?}` at {}:{}:{}",
                    type_kind,
                    loc.file,
                    loc.row,
                    loc.col + 1
                ),
            }
            return;
        }
    };

    let rewritten_statements = match rewriter::rewrite_statements(checked_statements) {
        Ok(rewritten_statements) => rewritten_statements,
        Err(_) => todo!(),
    };

    // for statement in &checked_statements {
    //     println!("{:#?}", statement)
    // }

    let mut c_emitter =
        CEmitter::new(File::create(Path::new("out.c")).expect("couldn't open c file"));

    c_emitter
        .emit_program(rewritten_statements)
        .expect("emit failed");

    Command::new("gcc")
        .arg("-o")
        .arg(
            Path::new("out")
                .with_extension("")
                .file_name()
                .expect("todo"),
        )
        .arg("out.c")
        .output()
        .expect("couldn't make executable");
    //fs::remove_file("out.c").expect(&format!("Couldn't remove intermediate c file"));
}
