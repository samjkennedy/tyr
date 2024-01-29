use std::io::{BufRead, BufReader};
use std::{fmt, fs};

use crate::lexer::{self, LexError, Loc};

use crate::parser::ParseError;
use crate::type_checker::TypeCheckError;

pub fn read_file_contents(file_path: &str) -> Result<String, std::io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = BufReader::new(file);
    Ok(reader.lines().collect::<Result<Vec<_>, _>>()?.join("\n"))
}

pub fn format_location(file_name: &str, loc: &Loc) -> String {
    format!("{}:{}:{}", file_name, loc.row, loc.col + 1,)
}

pub fn format_error_line(loc: &Loc, error_line: &str) -> String {
    format!("{:>3} |{}", loc.row, error_line) //TODO: dynamically adjust this spacing based on line number
}

pub fn format_highlight(loc: &Loc) -> String {
    format!(
        "{}\x1b[91m{}\x1b[0m",
        " ".repeat(loc.col),
        "~".repeat(loc.len)
    )
}

pub enum DiagnosticKind {
    Lex(LexError),
    Parse(ParseError),
    TypeCheck(TypeCheckError),
}

pub enum LogLevel {
    Error,
    Warn,
    Info,
}
pub struct Diagnostic {
    level: LogLevel,
    kind: DiagnosticKind,
    hint: Option<String>,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Error => write!(f, "\x1b[91merror:\x1b[0m"),
            LogLevel::Warn => write!(f, "\x1b[93mwarning:\x1b[0m"),
            LogLevel::Info => write!(f, "\x1b[94minfo:\x1b[0m"),
        }
    }
}

pub fn display_diagnostic(file_name: &str, diagnostic: Diagnostic) {
    match diagnostic.kind {
        DiagnosticKind::Lex(lex_error) => todo!(),
        DiagnosticKind::Parse(parse_error) => {
            print_parse_error(file_name, &parse_error);
        }
        DiagnosticKind::TypeCheck(type_check_error) => {
            print_typecheck_error(file_name, &type_check_error);
        }
    }
}

pub fn print_parse_error(file_name: &str, e: &ParseError) {
    let log_level = LogLevel::Error;
    match e {
        ParseError::UnexpectedToken(tok) => {
            let error_line = get_error_line(file_name, &tok.loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(&tok.loc, &error_line),
                format_highlight(&tok.loc)
            );
            eprintln!(
                "{} unexpected token \"{}\" ({:?}) at {}\n{}",
                log_level,
                tok.text,
                tok.kind,
                format_location(file_name, &tok.loc),
                highlight,
            )
        }
        ParseError::UnexpectedEOF => eprintln!("{} unexpected EOF", log_level),
        ParseError::TokenMismatch { expected, actual } => {
            let error_line = get_error_line(file_name, &actual.loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(&actual.loc, &error_line),
                format_highlight(&actual.loc)
            );
            eprintln!(
                "{} expected token {:?} but got \"{}\" ({:?}) at {}\n{}",
                log_level,
                expected,
                actual.text,
                actual.kind,
                format_location(file_name, &actual.loc),
                highlight,
            )
        }
        ParseError::ExpectedSemicolon(loc) => {
            let error_line = get_error_line(file_name, &loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(&loc, &error_line),
                format_highlight(&loc)
            );
            eprintln!(
                "{} expected semicolon at {}\n{}",
                log_level,
                format_location(file_name, &loc),
                highlight,
            )
        }
        ParseError::CannotStaticallyAccess(_expression) => todo!(),
    }
}

pub fn print_typecheck_error(file_name: &str, error: &TypeCheckError) -> String {
    let log_level = LogLevel::Error;
    match error {
        TypeCheckError::VariableAlreadyDeclared { variable, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} variable `{}` was already declared at {}\n{}",
                log_level,
                variable.name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::BinaryOpNotImplementedForTypes {
            left,
            op,
            right,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} binary operation '{:?}' is not implemented for types '{}' and '{}' at {}\n{}",
                log_level,
                op,
                left,
                right,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::UnaryOpNotImplementedForType { op, operand, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} unary operation '{:?}' is not implemented for type '{}' at {}\n{}",
                log_level,
                op,
                operand,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::TypeMismatch {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type mismatch, expected '{}' but found '{}' at {}\n{}",
                log_level,
                expected,
                actual,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchVariableDeclaredInScope { name, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such variable `{}` declared in scope at {}\n{}",
                log_level,
                name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchTypeDeclaredInScope { name, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such type `{}` declared in scope at {}\n{}",
                log_level,
                name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchFunctionDeclaredInScope { name, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "No such function `{}` declared in scope at {}\n{}",
                name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchNamespaceDeclaredInScope { namespace } => {
            let loc = &namespace.loc;
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "No such namespace `{}` declared in scope at {}\n{}",
                namespace.text,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::FunctionAlreadyDeclared { function, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} function `{}` was already declared at {}\n{}",
                log_level,
                function.name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NotAllCodePathsReturnValue { name, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} not all code paths return a value in function `{}` at {}\n{}",
                log_level,
                name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::CannotIndexType { type_kind, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` cannot be indexed {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::TypeAlreadyDeclared { type_name, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` already declared in scope {}\n{}",
                log_level,
                type_name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::MissingArgForRecord {
            name,
            type_kind,
            record_name,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} missing argument `{}: {}` for record `{}` at {}\n{}",
                log_level,
                name,
                type_kind,
                record_name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::UnexpectedArgForRecord {
            type_kind,
            record_name,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} unexpected argument of type `{}` for record `{}` at {}\n{}",
                log_level,
                type_kind,
                record_name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::CannotAccessType { type_kind, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` has no members to access at {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::CannotIterateType { type_kind, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` cannot be iterated at {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchMember {
            member_name,
            name,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such member `{}` on type `{}` at {}\n{}",
                log_level,
                member_name,
                name,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::WithCalledOnNonRecordType { type_kind, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} `with` cannot be used with non record type `{}` at {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::WrongNumberOfTypeArguments {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} wrong number of type arguments: {}, required: {}, at {}\n{}",
                log_level,
                actual,
                expected,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::ArgLengthMismatch {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} wrong number of arguments: {}, required: {}, at {}\n{}",
                log_level,
                actual,
                expected,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::CannotDeduceTypeFromContext(loc) => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} cannot deduce type from context, at {}\n{}",
                log_level,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NonNillableType(type_kind, loc) => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} `{}` is a non-nillable type, at {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::CannotPassOptionalTypeToFunction { type_kind, loc } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} cannot print optional type `{}`, at {}\n{}",
                log_level,
                type_kind,
                format_location(file_name, loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchVariant {
            variant_name,
            union_name,
            loc,
        } => {
            let error_line = get_error_line(file_name, loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such variant `{}` of type `{}` at {}\n{}",
                log_level,
                variant_name,
                union_name,
                format_location(file_name, loc),
                highlight,
            )
        }
    }
}

pub(crate) fn get_error_line(file_name: &str, loc: &lexer::Loc) -> String {
    let file_content = read_file_contents(file_name).unwrap_or_else(|_| String::new());

    let line_number = loc.row;
    let line_start = file_content
        .lines()
        .nth(line_number - 1)
        .unwrap_or_default();
    let error_line = line_start.trim_end();

    error_line.to_owned()
}
