use std::{
    fs::File,
    io::{self, Write},
};

use crate::{
    parser::BinaryOpKind,
    type_checker::{
        CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedStatementKind, Module,
        TypeKind,
    },
};

pub struct CEmitter {
    out_file: File,
}

impl CEmitter {
    pub fn new(out_file: File) -> CEmitter {
        CEmitter { out_file }
    }

    pub fn emit_module(&mut self, module: Module) -> io::Result<()> {
        //preamble
        self.out_file.write_all(b"#include <stdio.h>\n")?;
        self.out_file.write_all(b"#include <stdbool.h>\n")?;
        self.out_file.write_all(b"#include <stdint.h>\n")?;
        self.out_file.write_all(b"#include <stdlib.h>\n")?;
        self.out_file.write_all(
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
            } while (0)
            
        #define PRINT_IF_NOT_NULL(ptr, format, ...) \
            do { \
                if ((ptr) == NULL) { \
                    printf(\"%s\", \"nil\"); \
                } else { \
                    printf(format, __VA_ARGS__); \
                } \
            } while(0)
        
        #define PRINTLN_IF_NOT_NULL(ptr, format, ...) \
            do { \
                if ((ptr) == NULL) { \
                    printf(\"%s\\n\", \"nil\"); \
                } else { \
                    printf(format, __VA_ARGS__); \
                } \
            } while(0)
        #define ACCESS_FIELD_OR_NULL(ptr, field) \
            ((ptr) == NULL ? NULL : &((ptr)->field))
            
            #define SAFE_DEREF(x) \
            ((x) ? *(x) : (exit(1), *(x)))
            #define DA_APPEND(arr, x) \
            do { \
                if ((arr).count == (arr).capacity) { \
                    (arr).capacity = ((arr).capacity == 0) ? 1 : (arr).capacity * 2; \
                    (arr).data = realloc((arr).data, (arr).capacity * sizeof(*(arr).data)); \
                    if (!(arr).data) { \
                        perror(\"Failed to allocate memory for dynamic array\"); \
                        exit(EXIT_FAILURE); \
                    } \
                } \
                (arr).data[(arr).count++] = (x); \
            } while (0)
        
        
        #define DA_INIT(type, ...) \
        ({ \
            da_##type arr; \
            arr.data = NULL; \
            arr.count = 0; \
            arr.capacity = 0; \
            type init_values[] = {__VA_ARGS__}; \
            for (int i = 0; i < sizeof(init_values) / sizeof(init_values[0]); ++i) { \
                DA_APPEND(arr, init_values[i]); \
            } \
            arr; \
        })
        
        #define DA_CLEAR(arr) \
            ((arr).count = 0)
        
        #define DA_IS_EMPTY(arr) \
            ((arr).count == 0)

        #define DA_POP(arr) \
            ((arr).count > 0 ? (arr).data[--(arr).count] : 0)
    ",
        )?;
        //TODO: When slices are a thing, use this struct?
        // self.out_file.write(
        //     b"#
        //     typedef struct Tyr_Slice
        //     {
        //             void *data;
        //             size_t len;
        //     } Tyr_Slice;",
        // )?;
        // for type_kind in &module.types {
        //     match type_kind {
        //         TypeKind::Record(name, generic_params, _) => {
        //             if !generic_params.is_empty() {
        //                 writeln!(
        //                     self.out_file,
        //                     "typedef struct {}_{}, {}_{};",
        //                     name,
        //                     generic_params
        //                         .iter()
        //                         .map(Self::get_c_type)
        //                         .collect::<Vec<String>>()
        //                         .join("_"),
        //                     name,
        //                     generic_params
        //                         .iter()
        //                         .map(Self::get_c_type)
        //                         .collect::<Vec<String>>()
        //                         .join("_")
        //                 )?;
        //             } else {
        //                 writeln!(self.out_file, "typedef struct {} {};", name, name)?;
        //             }
        //         }
        //         _ => todo!("emit module type kind: {}", type_kind),
        //     }
        // }
        for type_kind in &module.types {
            match type_kind {
                TypeKind::Record(name, generic_params, members) => {
                    if !generic_params.is_empty() {
                        writeln!(
                            self.out_file,
                            "typedef struct {}_{} {{",
                            name,
                            generic_params
                                .iter()
                                .map(Self::get_c_type)
                                .collect::<Vec<String>>()
                                .join("_")
                        )?;
                        for member in members {
                            writeln!(
                                self.out_file,
                                "{} {};",
                                Self::get_c_type(&member.type_kind),
                                member.name
                            )?;
                        }
                        writeln!(
                            self.out_file,
                            "}} {}_{};",
                            name,
                            generic_params
                                .iter()
                                .map(Self::get_c_type)
                                .collect::<Vec<String>>()
                                .join("_")
                        )?;
                    } else {
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
                }
                TypeKind::Enum(name, variants) => {
                    //values array for printing
                    writeln!(
                        self.out_file,
                        "const char *{}_values[{}] = {{",
                        name,
                        variants.len()
                    )?;
                    for variant in variants {
                        writeln!(self.out_file, "\"{}\",", variant)?;
                    }
                    writeln!(self.out_file, "}};")?;
                    //struct typedef
                    writeln!(self.out_file, "typedef enum {} {{", name)?;
                    for variant in variants {
                        writeln!(self.out_file, "{}_{},", name, variant)?;
                    }
                    writeln!(self.out_file, "}} {};", name)?;
                }
                _ => todo!("emit module type kind: {}", type_kind),
            }
        }

        for statement in module.statements {
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
            } => match type_kind {
                TypeKind::Array(size, el_type) => {
                    write!(
                        self.out_file,
                        "{} {}[{}] = ",
                        Self::get_c_type(el_type),
                        name,
                        size
                    )?;

                    self.emit_expression(initialiser)?;
                    writeln!(self.out_file, ";")?;
                }
                TypeKind::Optional(base_type) => match **base_type {
                    TypeKind::File => {
                        write!(self.out_file, "{} {} = ", Self::get_c_type(type_kind), name)?;
                        self.emit_expression(initialiser)?;
                        writeln!(self.out_file, ";")?;
                    }
                    _ => {
                        write!(
                            self.out_file,
                            "{} optional_{} = ",
                            Self::get_c_type(base_type),
                            name
                        )?;
                        self.emit_expression(initialiser)?;
                        writeln!(self.out_file, ";",)?;
                        write!(
                            self.out_file,
                            "{} *{} = &optional_{};",
                            Self::get_c_type(base_type),
                            name,
                            name
                        )?;
                    }
                },
                _ => {
                    write!(self.out_file, "{} {} = ", Self::get_c_type(type_kind), name)?;

                    self.emit_expression(initialiser)?;
                    writeln!(self.out_file, ";")?;
                }
            },
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
                        "{}",
                        Self::get_c_type_function_arg(&arg.type_kind, arg.name.clone()),
                    )?;
                    if i < args.len() - 1 {
                        write!(self.out_file, ", ")?;
                    }
                }
                write!(self.out_file, ")")?;

                self.emit_statement(body)?;
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
            CheckedStatementKind::Record {
                name: _,
                generic_params: _,
                members: _,
            } => {
                //Don't emit records
                return Ok(());
            }
            CheckedStatementKind::Break => writeln!(self.out_file, "break;")?,
            CheckedStatementKind::Continue => writeln!(self.out_file, "continue;")?,
            CheckedStatementKind::Enum { name, variants } => (), //No need to emit enums again
            CheckedStatementKind::MatchCases { cases } => {
                for case in cases {
                    match &case.kind {
                        CheckedExpressionKind::MatchCase { pattern, result } => {
                            write!(self.out_file, "     case ")?;
                            self.emit_expression(pattern)?;
                            writeln!(self.out_file, ":")?;
                            writeln!(self.out_file, "{{")?;
                            self.emit_expression(result)?;
                            writeln!(self.out_file, ";}};")?;
                            writeln!(self.out_file, "break;")?;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            CheckedStatementKind::Match { expression, cases } => {
                write!(self.out_file, "switch (")?;
                self.emit_expression(expression)?;
                writeln!(self.out_file, ") {{")?;
                self.emit_statement(cases)?;
                writeln!(self.out_file, "}}")?;
            }
            CheckedStatementKind::NoOp => {}
            CheckedStatementKind::ForIn {
                iterator: _,
                iterable: _,
                body: _,
            } => unreachable!("Should have been rewritten to a while loop"),
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
            CheckedExpressionKind::CharLiteral { value } => write!(self.out_file, "'{}'", value),
            CheckedExpressionKind::F32Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::F64Literal { value } => write!(self.out_file, "{}", value),
            CheckedExpressionKind::StringLiteral { value } => {
                write!(self.out_file, "\"{}\"", value)
            }
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
            CheckedExpressionKind::Unary { op, operand } => {
                match op {
                    crate::parser::UnaryOpKind::Negation => write!(self.out_file, " -")?,
                    crate::parser::UnaryOpKind::Not => write!(self.out_file, " !")?,
                    crate::parser::UnaryOpKind::Identity => write!(self.out_file, " +")?,
                }
                write!(self.out_file, "(")?;
                self.emit_expression(operand)?;
                write!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpressionKind::Parenthesised { expression } => {
                write!(self.out_file, "(")?;
                self.emit_expression(expression)?;
                write!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpressionKind::Variable { variable } => {
                write!(self.out_file, "{}", variable.name)
            }
            CheckedExpressionKind::Assignment { lhs, rhs } => {
                // if let CheckedExpressionKind::ArrayIndex { array, index } = &lhs.kind {
                //     writeln!(self.out_file, "SAFE_SET_ARRAY(")?;
                //     self.emit_expression(&array)?;
                //     writeln!(self.out_file, ", ")?;
                //     self.emit_expression(&index)?;
                //     writeln!(self.out_file, ", ")?;
                //     self.emit_expression(&rhs)?;
                //     writeln!(self.out_file, ")")?;
                //     return Ok(());
                // }

                self.emit_expression(lhs)?;
                write!(self.out_file, " = ")?;
                self.emit_expression(rhs)?;
                Ok(())
            }
            CheckedExpressionKind::FunctionCall { name, args } => {
                if name == "print" {
                    let arg = &args[0];
                    write!(self.out_file, "printf(")?;
                    write!(
                        self.out_file,
                        "\"{}\", ",
                        Self::get_print_format_for_type(&arg.type_kind)
                    )?;
                    if let TypeKind::Enum(name, _) = &arg.type_kind {
                        write!(self.out_file, "{}_values[", name)?;
                        self.emit_expression(arg)?;
                        write!(self.out_file, "]")?;
                    } else {
                        self.emit_expression(arg)?;
                    }
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "println" {
                    let arg = &args[0];
                    write!(self.out_file, "printf(")?;
                    write!(
                        self.out_file,
                        "\"{}\\n\", ",
                        Self::get_print_format_for_type(&arg.type_kind)
                    )?;
                    if let TypeKind::Enum(name, _) = &arg.type_kind {
                        write!(self.out_file, "{}_values[", name)?;
                        self.emit_expression(arg)?;
                        write!(self.out_file, "]")?;
                    } else {
                        self.emit_expression(arg)?;
                    }
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                //Dynamic arrays
                if name == "append" {
                    write!(self.out_file, "DA_APPEND(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ", (")?;
                    self.emit_expression(&args[1])?;
                    write!(self.out_file, "))")?;
                    return Ok(());
                }
                if name == "is_empty" {
                    write!(self.out_file, "DA_IS_EMPTY(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "clear" {
                    write!(self.out_file, "DA_CLEAR(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "pop" {
                    write!(self.out_file, "DA_POP(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                //Files
                if name == "open_file" {
                    write!(self.out_file, "fopen(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ", ")?;
                    self.emit_expression(&args[1])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "read_char" {
                    write!(self.out_file, "fgetc(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "write" {
                    write!(self.out_file, "fputs(")?;
                    self.emit_expression(&args[1])?;
                    write!(self.out_file, ", ")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                if name == "eof" {
                    write!(self.out_file, "ungetc(getc(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, "), ")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ") == -1")?;
                    return Ok(());
                }
                if name == "close" {
                    write!(self.out_file, "fclose(")?;
                    self.emit_expression(&args[0])?;
                    write!(self.out_file, ")")?;
                    return Ok(());
                }
                write!(self.out_file, "{}(", name)?;
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
                match &expression.type_kind {
                    TypeKind::Array(_size, _el_type) => {
                        write!(self.out_file, "{{")?;
                        for (i, element) in elements.iter().enumerate() {
                            self.emit_expression(element)?;

                            if i < elements.len() - 1 {
                                write!(self.out_file, ", ")?;
                            }
                        }
                        writeln!(self.out_file, "}}")?;
                    }
                    TypeKind::DynamicArray(el_type) => {
                        if elements.len() > 0 {
                            todo!("default dynamic array initializers");
                        }
                        write!(self.out_file, "(da_{}){{NULL, 0, 0}}", el_type)?;
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }
            CheckedExpressionKind::ArrayIndex { array, index } => {
                //TODO: This didn't work for arrays passed to functions, rethink once arrays are wide pointers
                // writeln!(self.out_file, "SAFE_ACCESS_ARRAY(")?;
                // self.emit_expression(&array)?;
                // writeln!(self.out_file, ", ")?;
                // self.emit_expression(&index)?;
                // writeln!(self.out_file, ")")?;
                if let TypeKind::DynamicArray(_) = array.type_kind {
                    self.emit_expression(array)?;
                    writeln!(self.out_file, ".data[")?;
                    self.emit_expression(index)?;
                    writeln!(self.out_file, "]")?;
                    return Ok(());
                }
                self.emit_expression(array)?;
                writeln!(self.out_file, "[")?;
                self.emit_expression(index)?;
                writeln!(self.out_file, "]")?;
                Ok(())
            }
            CheckedExpressionKind::RecordLiteral { arguments } => {
                match &expression.type_kind {
                    TypeKind::Optional(base_type) => {
                        write!(self.out_file, "&({}){{", Self::get_c_type(base_type))?;
                    }
                    _ => {
                        write!(
                            self.out_file,
                            "({}){{",
                            Self::get_c_type(&expression.type_kind)
                        )?;
                    }
                }

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
                self.emit_expression(accessee)?;
                write!(self.out_file, ".{}", member)?;
                Ok(())
            }
            CheckedExpressionKind::SafeAccessor { accessee, member } => {
                write!(self.out_file, "ACCESS_FIELD_OR_NULL(")?;
                self.emit_expression(accessee)?;
                write!(self.out_file, ", {})", member)?;
                Ok(())
            }
            CheckedExpressionKind::StaticAccessor { name, member } => {
                write!(self.out_file, "{}_{}", name, member.name) //TODO member should be an expression we handle differently
            }
            CheckedExpressionKind::MatchCase {
                pattern: _,
                result: _,
            } => todo!(),
            CheckedExpressionKind::Nil => write!(self.out_file, " NULL "),
            CheckedExpressionKind::ForceUnwrap { expression } => {
                if let TypeKind::Optional(_) = expression.type_kind {
                    self.emit_expression(expression)?;
                    return Ok(());
                }
                //TODO: use a safe macro
                write!(self.out_file, "SAFE_DEREF(")?;
                self.emit_expression(expression)?;
                write!(self.out_file, ")")?;
                Ok(())
            }
            CheckedExpressionKind::NilCoalesce { optional, default } => {
                //TODO: this evaluates the optional expression twice, store it as a temporary variable first
                self.emit_expression(optional)?;
                write!(self.out_file, " == NULL ? ")?;
                self.emit_expression(default)?;
                write!(self.out_file, " : ")?;
                self.emit_expression(optional)?;
                Ok(())
            }
            CheckedExpressionKind::Range { lower: _, upper: _ } => todo!(),
            CheckedExpressionKind::DefaultArrayInitializer { value } => {
                //{ [0 ... 8] = 10 }
                if let TypeKind::Array(size, _element_type) = &expression.type_kind {
                    write!(self.out_file, "{{ [0 ... {}] = ", size - 1)?;
                    self.emit_expression(value)?;
                    write!(self.out_file, "}}")?;
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            CheckedExpressionKind::Cast {
                expression,
                type_kind,
            } => {
                write!(self.out_file, "({}) ", Self::get_c_type(type_kind))?;
                self.emit_expression(expression)?;
                Ok(())
            }
        }
    }

    fn get_c_type(type_kind: &TypeKind) -> String {
        match type_kind {
            TypeKind::Unit => "void".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::Record(name, generic_params, _) => {
                if generic_params.is_empty() {
                    name.to_string()
                } else {
                    format!(
                        "{}_{}",
                        name,
                        generic_params
                            .iter()
                            .map(Self::get_c_type)
                            .collect::<Vec<String>>()
                            .join("_")
                    )
                }
            }
            TypeKind::Enum(name, _) => name.to_string(),
            TypeKind::U8 => "unsigned char".to_string(),
            TypeKind::U16 => "unsigned short".to_string(),
            TypeKind::U32 => "unsigned long".to_string(),
            TypeKind::U64 => "unsigned long long".to_string(),
            TypeKind::I8 | TypeKind::Char => "char".to_string(),
            TypeKind::I16 => "short".to_string(),
            TypeKind::I32 => "long".to_string(),
            TypeKind::I64 => "long long".to_string(),
            TypeKind::F32 => "float".to_string(),
            TypeKind::F64 => "double".to_string(),
            TypeKind::Array(el_count, el_type) => {
                format!("{} [{}]", Self::get_c_type(el_type), el_count)
            }
            TypeKind::Slice(el_type) => {
                format!("{}* ", Self::get_c_type(el_type))
            }
            TypeKind::String => "char*".to_string(),
            TypeKind::Range(_) => todo!(),
            TypeKind::GenericParameter(name) => name.to_string(),
            TypeKind::File => "FILE *".to_string(),
            TypeKind::Optional(base_type) => match **base_type {
                TypeKind::File => format!("{} ", Self::get_c_type(base_type)),
                _ => format!("{} *", Self::get_c_type(base_type)),
            },
            TypeKind::DynamicArray(el_type) => {
                format!("da_{}", el_type)
            }
            TypeKind::Pointer(base_type) => format!("{} *", Self::get_c_type(base_type)),
        }
    }

    fn get_c_type_function_arg(type_kind: &TypeKind, param_name: String) -> String {
        match type_kind {
            TypeKind::Unit => format!("void {}", param_name),
            TypeKind::Bool => format!("bool {}", param_name),
            TypeKind::Record(name, _, _) => format!("{} {}", name, param_name),
            TypeKind::Enum(name, _) => format!("{} {}", name, param_name),
            TypeKind::U8 => format!("unsigned char {}", param_name),
            TypeKind::U16 => format!("unsigned short {}", param_name),
            TypeKind::U32 => format!("unsigned long {}", param_name),
            TypeKind::U64 => format!("unsigned long long {}", param_name),
            TypeKind::I8 | TypeKind::Char => format!("char {}", param_name),
            TypeKind::I16 => format!("short {}", param_name),
            TypeKind::I32 => format!("long {}", param_name),
            TypeKind::I64 => format!("long long {}", param_name),
            TypeKind::F32 => format!("float {}", param_name),
            TypeKind::F64 => format!("double {}", param_name),
            TypeKind::Array(size, el_type) => {
                format!("{} {}[{}]", Self::get_c_type(el_type), param_name, size)
                //TODO: does this work for multidim arrays?
            }
            TypeKind::Slice(el_type) => {
                //TODO: should be a wide pointer really
                format!("{} *{}", Self::get_c_type(el_type), param_name)
            }
            TypeKind::String => format!("char* {}", param_name),
            TypeKind::Range(_) => todo!(),
            TypeKind::GenericParameter(_) => todo!(),
            TypeKind::File => format!("FILE *{}", param_name),
            TypeKind::Optional(base_type) => {
                format!("{} {}", Self::get_c_type(base_type), param_name)
            }
            TypeKind::DynamicArray(el_type) => format!("da_{} {}", el_type, param_name),
            TypeKind::Pointer(base_type) => {
                format!("{} *{}", Self::get_c_type(base_type), param_name)
            }
        }
    }

    fn get_print_format_for_type(type_kind: &TypeKind) -> String {
        match type_kind {
            TypeKind::Unit => panic!("can't format void!"),
            TypeKind::Bool => "%d".to_string(),
            TypeKind::Array(_, _) => todo!(),
            TypeKind::Slice(_) => todo!(),
            TypeKind::Record(_, _, _) => todo!(),
            TypeKind::Enum(_, _) => "%s".to_string(),
            TypeKind::U8 => "%hhu".to_string(),
            TypeKind::U16 => "%hu".to_string(),
            TypeKind::U32 => "%lu".to_string(),
            TypeKind::U64 => "%llu".to_string(),
            TypeKind::I8 => "%hhi".to_string(),
            TypeKind::Char => "%c".to_string(),
            TypeKind::I16 => "%hi".to_string(),
            TypeKind::I32 => "%li".to_string(),
            TypeKind::I64 => "%lli".to_string(),
            TypeKind::F32 => "%.2f".to_string(),
            TypeKind::F64 => "%.6lf".to_string(),
            TypeKind::String => "%s".to_string(),
            TypeKind::Range(_) => todo!(),
            TypeKind::GenericParameter(_) => todo!(),
            TypeKind::File => "%zu".to_string(),
            TypeKind::Optional(base_type) => Self::get_print_format_for_type(base_type),
            TypeKind::DynamicArray(_) => todo!(),
            TypeKind::Pointer(_) => "%zu".to_string(),
        }
    }
}
