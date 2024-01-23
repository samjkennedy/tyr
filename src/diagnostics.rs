use std::fs;
use std::io::{BufRead, BufReader};

use crate::lexer::{self, Loc};

use crate::type_checker::TypeCheckError;

pub fn read_file_contents(file_path: &str) -> Result<String, std::io::Error> {
    let file = fs::File::open(file_path)?;
    let reader = BufReader::new(file);
    Ok(reader.lines().collect::<Result<Vec<_>, _>>()?.join("\n"))
}

pub fn format_location(loc: &Loc) -> String {
    format!("{}:{}:{}", loc.file, loc.row, loc.col + 1,)
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

pub fn format_typecheck_error(error: &TypeCheckError) -> String {
    let log_level = "\x1b[91merror:\x1b[0m";
    match error {
        TypeCheckError::VariableAlreadyDeclared { variable, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} variable `{}` was already declared at {}\n{}",
                log_level,
                variable.name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::BinaryOpNotImplementedForTypes {
            left,
            op,
            right,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::UnaryOpNotImplementedForType { op, operand, loc } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::TypeMismatch {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchVariableDeclaredInScope { name, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such variable `{}` declared in scope at {}\n{}",
                log_level,
                name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchTypeDeclaredInScope { name, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} no such type `{}` declared in scope at {}\n{}",
                log_level,
                name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchFunctionDeclaredInScope { name, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "No such function `{}` declared in scope at {}\n{}",
                name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchNamespaceDeclaredInScope { namespace } => {
            let loc = &namespace.loc;
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "No such namespace `{}` declared in scope at {}\n{}",
                namespace.text,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::FunctionAlreadyDeclared { function, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} function `{}` was already declared at {}\n{}",
                log_level,
                function.name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NotAllCodePathsReturnValue { name, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} not all code paths return a value in function `{}` at {}\n{}",
                log_level,
                name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::CannotIndexType { type_kind, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` cannot be indexed {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::TypeAlreadyDeclared { type_name, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` already declared in scope {}\n{}",
                log_level,
                type_name,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::MissingArgForRecord {
            name,
            type_kind,
            record_name,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::UnexpectedArgForRecord {
            type_kind,
            record_name,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::CannotAccessType { type_kind, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` has no members to access at {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::CannotIterateType { type_kind, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} type `{}` cannot be iterated at {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NoSuchMember {
            member_name,
            name,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::WithCalledOnNonRecordType { type_kind, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} `with` cannot be used with non record type `{}` at {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::WrongNumberOfTypeArguments {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::ArgLengthMismatch {
            expected,
            actual,
            loc,
        } => {
            let error_line = get_error_line(loc);
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
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::CannotDeduceTypeFromContext(loc) => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} cannot deduce type from context, at {}\n{}",
                log_level,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::NonNillableType(type_kind, loc) => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} `{}` is a non-nillable type, at {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
        TypeCheckError::CannotPassOptionalTypeToFunction { type_kind, loc } => {
            let error_line = get_error_line(loc);
            let highlight = format!(
                "    |\n{}\n    |{}",
                format_error_line(loc, &error_line),
                format_highlight(loc)
            );
            format!(
                "{} cannot print optional type `{}`, at {}\n{}",
                log_level,
                type_kind,
                format_location(loc),
                highlight,
            )
        }
    }
}

pub(crate) fn get_error_line(loc: &lexer::Loc) -> String {
    let file_content = read_file_contents(&loc.file).unwrap_or_else(|_| String::new());

    let line_number = loc.row;
    let line_start = file_content
        .lines()
        .nth(line_number - 1)
        .unwrap_or_default();
    let error_line = line_start.trim_end();

    error_line.to_owned()
}
