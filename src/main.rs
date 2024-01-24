use std::path::Path;
use std::process::Command;
use std::{env, fs::File};

use emitter::CEmitter;
use lexer::{lex_file, LexError};
use parser::{ParseError, Parser, Statement};
use type_checker::TypeChecker;

use crate::diagnostics::format_typecheck_error;

pub mod emitter;
pub mod lexer;
pub mod parser;
pub mod rewriter;
pub mod type_checker;

mod diagnostics;

fn main() {
    // Get the input file name from command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: tyr_compiler <file.tyr>");
        std::process::exit(1);
    }
    let file_name = &args[1];

    // Check that the file has a ".tyr" extension
    if !file_name.ends_with(".tyr") {
        eprintln!("Error: Input file must have a '.tyr' extension.");
        std::process::exit(1);
    }
    let file_path = Path::new(&file_name);
    let module_name = file_path.file_stem().unwrap().to_str().unwrap().to_string();

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
                LexError::UnterminatedCharLiteral(loc) => eprintln!(
                    "Unterminated character literal at {}:{}:{}",
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
                ParseError::CannotStaticallyAccess(_expression) => todo!(),
            }
            return;
        }
    };

    let mut type_checker = TypeChecker::new(module_name);

    let mut module = match type_checker.type_check_statements(statements) {
        Ok(module) => module,
        Err(e) => {
            eprintln!("{}", format_typecheck_error(&e));
            return;
        }
    };

    module.statements = match rewriter::rewrite_statements(module.statements) {
        Ok(rewritten_statements) => rewritten_statements,
        Err(_) => todo!(),
    };

    // for statement in &checked_statements {
    //     println!("{:#?}", statement)
    // }

    let base_name = Path::new(file_name)
        .file_stem()
        .expect("Couldn't get file stem")
        .to_string_lossy();
    let c_file_name = format!("{}.c", base_name);
    let exe_file_name = format!("{}", base_name);

    let mut c_emitter = CEmitter::new(File::create(&c_file_name).expect("Couldn't open C file"));

    c_emitter.emit_module(module).expect("Emit failed");

    // Compile the C code
    Command::new("gcc")
        .arg("-o")
        .arg(&exe_file_name)
        .arg(&c_file_name)
        .output()
        .expect("Couldn't make executable");

    // Remove the intermediate C file
    //fs::remove_file(&c_file_name).expect("Couldn't remove intermediate C file");
}
