use core::panic;
use std::{collections::HashMap, fmt};

use crate::{
    lexer::{Loc, Token, TokenKind},
    parser::{
        self, BinaryOp, BinaryOpKind, Expression, ExpressionKind, Statement, StatementKind,
        UnaryOp, UnaryOpKind,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Unit,
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
    Array(usize, Box<TypeKind>),
    Record(String, Vec<CheckedVariable>),
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Unit => write!(f, "unit"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Array(size, inner_type) => {
                write!(f, "[{}]", size)?;
                write!(f, "{}", inner_type)
            }
            TypeKind::Record(name, fields) => {
                write!(f, "{} {{ ", name)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.type_kind)?;
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    VariableAlreadyDeclared {
        variable: CheckedVariable,
        loc: Loc,
    },
    BinaryOpNotImplementedForTypes {
        left: TypeKind,
        op: BinaryOpKind,
        right: TypeKind,
        loc: Loc,
    },
    UnaryOpNotImplementedForType {
        op: parser::UnaryOpKind,
        operand: TypeKind,
        loc: Loc,
    },
    TypeMismatch {
        expected: TypeKind,
        actual: TypeKind,
        loc: Loc,
    },
    NoSuchVariableDeclaredInScope {
        name: String,
        loc: Loc,
    },
    NoSuchTypeDeclaredInScope {
        name: String,
        loc: Loc,
    },
    NoSuchFunctionDeclaredInScope {
        name: String,
        loc: Loc,
    },
    FunctionAlreadyDeclared {
        function: CheckedFunction,
        loc: Loc,
    },
    NotAllCodePathsReturnValue {
        name: String,
        loc: Loc,
    },
    CannotIndexType {
        type_kind: TypeKind,
        loc: Loc,
    },
    TypeAlreadyDeclared {
        record_name: String,
        loc: Loc,
    },
    MissingArgForRecord {
        name: String,
        type_kind: TypeKind,
        record_name: String,
        loc: Loc,
    },
    UnexpectedArgForRecord {
        type_kind: TypeKind,
        record_name: String,
        loc: Loc,
    },
    CannotAccessType {
        type_kind: TypeKind,
        loc: Loc,
    },
    NoSuchMember {
        member_name: String,
        name: String,
        loc: Loc,
    },
    WithCalledOnNonRecordType {
        type_kind: TypeKind,
        loc: Loc,
    },
}

#[derive(Debug, Clone)]
pub enum CheckedStatementKind {
    Block {
        statements: Vec<CheckedStatement>,
    },
    Expression {
        expression: CheckedExpression,
    },
    VariableDeclaration {
        name: String,
        type_kind: TypeKind,
        initialiser: CheckedExpression,
    },
    FunctionDeclaration {
        name: String,
        args: Vec<CheckedVariable>,
        return_type: Option<TypeKind>,
        body: Box<CheckedStatement>,
    },
    If {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
        else_body: Option<Box<CheckedStatement>>,
    },
    While {
        condition: CheckedExpression,
        body: Box<CheckedStatement>,
    },
    Return {
        return_value: Option<CheckedExpression>,
    },
    Record {
        name: String,
        members: Vec<CheckedVariable>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct CheckedStatement {
    pub kind: CheckedStatementKind,
}

#[derive(Debug, Clone)]
pub enum CheckedExpressionKind {
    BoolLiteral {
        value: bool,
    },
    U8Literal {
        value: u8,
    },
    U16Literal {
        value: u16,
    },
    U32Literal {
        value: u32,
    },
    U64Literal {
        value: u64,
    },
    I8Literal {
        value: i8,
    },
    I16Literal {
        value: i16,
    },
    I32Literal {
        value: i32,
    },
    I64Literal {
        value: i64,
    },
    F32Literal {
        value: f32,
    },
    F64Literal {
        value: f64,
    },
    ArrayLiteral {
        elements: Vec<CheckedExpression>,
    },
    RecordLiteral {
        arguments: Vec<CheckedExpression>,
    },
    Unary {
        op: UnaryOpKind,
        operand: Box<CheckedExpression>,
    },
    Binary {
        left: Box<CheckedExpression>,
        op: BinaryOpKind,
        right: Box<CheckedExpression>,
    },
    Parenthesised {
        expression: Box<CheckedExpression>,
    },
    Variable {
        name: String,
    },
    Assignment {
        lhs: Box<CheckedExpression>,
        rhs: Box<CheckedExpression>,
    },
    FunctionCall {
        name: String,
        args: Vec<CheckedExpression>,
    },
    ArrayIndex {
        array: Box<CheckedExpression>,
        index: Box<CheckedExpression>,
    },
    Accessor {
        accessee: Box<CheckedExpression>,
        member: String,
    },
    StringLiteral {
        value: String,
    },
}

#[derive(Debug, Clone)]
pub struct CheckedExpression {
    pub kind: CheckedExpressionKind,
    pub type_kind: TypeKind,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedVariable {
    pub name: String,
    pub type_kind: TypeKind,
    pub declaration_loc: Loc,
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
    pub name: String,
    pub args: Vec<TypeKind>,
    pub return_type: Option<TypeKind>,
    declaration_loc: Loc,
}

#[derive(Debug, Clone)]
struct Scope {
    parent: Option<Box<Scope>>,
    variables: HashMap<String, CheckedVariable>,
    functions: HashMap<String, CheckedFunction>,
    lookup_overrides: HashMap<String, (String, CheckedVariable)>,
    records: HashMap<String, TypeKind>,
    return_context: TypeKind,
    assign_context: TypeKind,
}

impl Scope {
    fn new_global_scope() -> Scope {
        return Scope {
            parent: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
            lookup_overrides: HashMap::new(),
            records: HashMap::new(),
            return_context: TypeKind::Unit,
            assign_context: TypeKind::Unit,
        };
    }

    fn new_inner_scope(parent: Scope) -> Scope {
        return Scope {
            return_context: parent.return_context.clone(),
            assign_context: parent.assign_context.clone(),
            parent: Some(Box::new(parent)),
            variables: HashMap::new(),
            functions: HashMap::new(),
            lookup_overrides: HashMap::new(),
            records: HashMap::new(),
        };
    }

    fn try_get_variable(
        &self,
        name: &String,
        loc: &Loc,
    ) -> Result<&CheckedVariable, TypeCheckError> {
        return match self.variables.get(name) {
            Some(variable) => Ok(variable),
            None => {
                return match &self.parent {
                    Some(parent) => parent.try_get_variable(name, loc),
                    None => Err(TypeCheckError::NoSuchVariableDeclaredInScope {
                        name: name.clone(),
                        loc: loc.clone(),
                    }),
                };
            }
        };
    }

    fn try_get_lookup_override(
        &self,
        name: &String,
        loc: &Loc,
    ) -> Result<(String, CheckedVariable), TypeCheckError> {
        return match self.lookup_overrides.get(name) {
            Some(lookup_override) => Ok(lookup_override.clone()),
            None => {
                return match &self.parent {
                    Some(parent) => parent.try_get_lookup_override(name, loc),
                    None => Err(TypeCheckError::NoSuchVariableDeclaredInScope {
                        name: name.clone(),
                        loc: loc.clone(),
                    }),
                };
            }
        };
    }

    fn try_declare_variable(
        &mut self,
        variable: CheckedVariable,
        loc: Loc,
    ) -> Result<(), TypeCheckError> {
        let name = variable.name.clone();
        if let Some(variable) = self.variables.get(&name) {
            return Err(TypeCheckError::VariableAlreadyDeclared {
                variable: variable.clone(),
                loc: loc.clone(),
            });
        }
        self.variables.insert(name, variable);
        Ok(())
    }

    fn try_get_function(
        &self,
        name: &String,
        loc: &Loc,
    ) -> Result<&CheckedFunction, TypeCheckError> {
        return match self.functions.get(name) {
            Some(functions) => Ok(functions),
            None => {
                return match &self.parent {
                    Some(parent) => parent.try_get_function(name, loc),
                    None => Err(TypeCheckError::NoSuchFunctionDeclaredInScope {
                        name: name.clone(),
                        loc: loc.clone(),
                    }),
                }
            }
        };
    }

    fn try_declare_function(&mut self, function: CheckedFunction) -> Result<(), TypeCheckError> {
        let name = function.name.clone();
        if let Some(function) = self.functions.get(&name) {
            return Err(TypeCheckError::FunctionAlreadyDeclared {
                function: function.clone(),
                loc: function.declaration_loc.clone(),
            });
        }
        self.functions.insert(name, function);
        Ok(())
    }

    fn try_declare_type(&mut self, record: TypeKind, loc: Loc) -> Result<(), TypeCheckError> {
        if let TypeKind::Record(name, ..) = &record {
            if self.records.contains_key(name) {
                return Err(TypeCheckError::TypeAlreadyDeclared {
                    record_name: name.clone(),
                    loc: loc.clone(),
                });
            }
            self.records.insert(name.clone(), record);
            return Ok(());
        }
        panic!("Cannot declare non-record types")
    }

    fn try_get_type(&self, name: &String, loc: Loc) -> Result<TypeKind, TypeCheckError> {
        //TODO: once custom types are a thing, this needs to be more complex
        return match name.as_str() {
            "bool" => Ok(TypeKind::Bool),
            "u8" => Ok(TypeKind::U8),
            "u16" => Ok(TypeKind::U16),
            "u32" => Ok(TypeKind::U32),
            "u64" => Ok(TypeKind::U64),
            "i8" => Ok(TypeKind::I8),
            "i16" => Ok(TypeKind::I16),
            "i32" => Ok(TypeKind::I32),
            "i64" => Ok(TypeKind::I64),
            "f32" => Ok(TypeKind::F32),
            "f64" => Ok(TypeKind::F64),
            "string" => Ok(TypeKind::String),
            _ => {
                return match self.records.get(name) {
                    Some(type_kind) => Ok(type_kind.clone()),
                    None => match &self.parent {
                        Some(parent) => parent.try_get_type(name, loc),
                        None => Err(TypeCheckError::NoSuchFunctionDeclaredInScope {
                            name: name.clone(),
                            loc: loc.clone(),
                        }),
                    },
                }
            }
        };
    }
}

pub struct TypeChecker {
    scope: Scope,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        return TypeChecker {
            scope: Scope::new_global_scope(),
        };
    }

    pub fn type_check_statements(
        &mut self,
        statements: Vec<Statement>,
    ) -> Result<Vec<CheckedStatement>, TypeCheckError> {
        let mut checked_statements: Vec<CheckedStatement> = Vec::new();

        for statement in statements {
            let checked_statement = self.type_check_statement(&statement)?;

            checked_statements.push(checked_statement);
        }

        Ok(checked_statements)
    }

    fn type_check_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<CheckedStatement, TypeCheckError> {
        match &statement.kind {
            StatementKind::Block { statements } => {
                //todo: too much cloning here
                let outer_scope = self.scope.clone();
                self.scope = Scope::new_inner_scope(outer_scope.clone());

                let mut checked_statements: Vec<CheckedStatement> = vec![];
                for statement in statements {
                    checked_statements.push(self.type_check_statement(&statement)?);
                }

                self.scope = outer_scope;

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Block {
                        statements: checked_statements,
                    },
                })
            }
            StatementKind::VariableDeclaration {
                var_keyword: _,
                identifier,
                type_annotation,
                equals: _,
                initialiser,
                semicolon: _,
            } => {
                let checked_initialiser = if let Some(type_annotation) = type_annotation {
                    if let ExpressionKind::TypeAnnotation {
                        colon: _,
                        identifier,
                    } = &type_annotation.kind
                    {
                        let type_kind = self
                            .scope
                            .try_get_type(&identifier.text, identifier.loc.clone())?;

                        self.scope.assign_context = type_kind.clone();

                        let checked_initialiser = self.type_check_expression(initialiser)?;

                        if type_kind != checked_initialiser.type_kind {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: type_kind,
                                actual: checked_initialiser.type_kind,
                                loc: initialiser.loc.clone(),
                            });
                        }
                        checked_initialiser
                    } else {
                        unreachable!()
                    }
                } else {
                    self.type_check_expression(initialiser)?
                };

                self.scope.try_declare_variable(
                    CheckedVariable {
                        name: identifier.text.clone(),
                        type_kind: checked_initialiser.type_kind.clone(),
                        declaration_loc: identifier.loc.clone(),
                    },
                    identifier.loc.clone(),
                )?;

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::VariableDeclaration {
                        name: identifier.text.clone(),
                        type_kind: checked_initialiser.type_kind.clone(), //TODO: use the annotation if present
                        initialiser: checked_initialiser,
                    },
                })
            }
            StatementKind::Expression { expression, .. } => Ok(CheckedStatement {
                kind: CheckedStatementKind::Expression {
                    expression: self.type_check_expression(expression)?,
                },
            }),
            StatementKind::FunctionDeclaration {
                function_keyword,
                identifier,
                open_paren: _,
                args,
                close_paren: _,
                return_annotation,
                body,
            } => self.type_check_function_definition(
                identifier,
                return_annotation,
                args,
                function_keyword,
                body,
            ),
            StatementKind::If {
                if_keyword: _,
                condition,
                body,
                else_body,
            } => {
                let checked_condition = self.type_check_expression(condition)?;

                Self::expect_type(
                    TypeKind::Bool,
                    checked_condition.type_kind.clone(),
                    condition.loc.clone(),
                )?;

                let checked_body = self.type_check_statement(body)?;
                let checked_else_body = match else_body {
                    Some(else_body) => Some(self.type_check_statement(else_body)?),
                    None => None,
                };

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::If {
                        condition: checked_condition,
                        body: Box::new(checked_body),
                        else_body: checked_else_body.map(|e| Box::new(e)),
                    },
                })
            }
            StatementKind::While {
                while_keyword: _,
                condition,
                body,
            } => {
                let checked_condition = self.type_check_expression(condition)?;

                Self::expect_type(
                    TypeKind::Bool,
                    checked_condition.type_kind.clone(),
                    condition.loc.clone(),
                )?;

                let checked_body = self.type_check_statement(body)?;

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::While {
                        condition: checked_condition,
                        body: Box::new(checked_body),
                    },
                })
            }
            StatementKind::Return {
                return_keyword: _,
                return_value,
            } => {
                if let Some(return_value) = return_value {
                    let checked_return_value = self.type_check_expression(return_value)?;

                    Self::expect_type(
                        self.scope.return_context.clone(),
                        checked_return_value.type_kind.clone(),
                        return_value.loc.clone(),
                    )?;
                    return Ok(CheckedStatement {
                        kind: CheckedStatementKind::Return {
                            return_value: Some(checked_return_value),
                        },
                    });
                }
                return Ok(CheckedStatement {
                    kind: CheckedStatementKind::Return { return_value: None },
                });
            }
            StatementKind::Record {
                record_keyword,
                identifier,
                members,
            } => {
                let mut checked_members: Vec<CheckedVariable> = Vec::new();

                for (member_identifier, type_annotation) in members {
                    if let ExpressionKind::TypeAnnotation {
                        colon: _,
                        identifier: type_identifier,
                    } = &type_annotation.kind
                    {
                        let type_kind = self
                            .scope
                            .try_get_type(&type_identifier.text, type_identifier.loc.clone())?;

                        checked_members.push(CheckedVariable {
                            name: member_identifier.text.clone(),
                            type_kind,
                            declaration_loc: member_identifier.loc.clone(),
                        })
                    } else {
                        panic!("non-type annotation expression made it into here")
                    }
                }

                let record = TypeKind::Record(identifier.text.clone(), checked_members.clone());

                self.scope
                    .try_declare_type(record, record_keyword.loc.clone())?;

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Record {
                        name: identifier.text.clone(),
                        members: checked_members,
                    },
                })
            }
            StatementKind::Break => Ok(CheckedStatement {
                kind: CheckedStatementKind::Break,
            }),
            StatementKind::Continue => Ok(CheckedStatement {
                kind: CheckedStatementKind::Continue,
            }),
        }
    }

    fn type_check_function_definition(
        &mut self,
        identifier: &Token,
        return_annotation: &Option<Expression>,
        args: &Vec<Expression>,
        function_keyword: &Token,
        body: &Box<Statement>,
    ) -> Result<CheckedStatement, TypeCheckError> {
        let name = &identifier.text;
        let return_type: Option<TypeKind> = match return_annotation {
            Some(type_annotation) => {
                if let ExpressionKind::TypeAnnotation {
                    colon: _,
                    identifier,
                } = &type_annotation.kind
                {
                    let type_kind = self
                        .scope
                        .try_get_type(&identifier.text, identifier.loc.clone())?;
                    Some(type_kind)
                } else {
                    unreachable!()
                }
            }
            None => None,
        };
        let mut checked_args: Vec<TypeKind> = Vec::new();
        for arg in args {
            if let ExpressionKind::FunctionParameter {
                modifiers: _,
                identifier: _,
                type_annotation,
            } = &arg.kind
            {
                if let ExpressionKind::TypeAnnotation {
                    colon: _,
                    identifier,
                } = &type_annotation.kind
                {
                    checked_args.push(
                        self.scope
                            .try_get_type(&identifier.text, identifier.loc.clone())?,
                    )
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        let checked_function = CheckedFunction {
            name: name.clone(),
            args: checked_args,
            return_type: return_type.clone(),
            declaration_loc: function_keyword.loc.clone(),
        };
        self.scope.try_declare_function(checked_function)?;
        //todo: too much cloning here
        let outer_scope = self.scope.clone();
        self.scope = Scope::new_inner_scope(outer_scope.clone());
        self.scope.return_context = match &return_type {
            Some(return_type) => return_type.clone(),
            None => TypeKind::Unit,
        };
        let checked_args: Vec<CheckedVariable> = self.type_check_function_parameters(args)?;

        let checked_body = self.type_check_statement(body)?;
        if return_type.is_some() {
            if !Self::all_paths_return_value(&checked_body) {
                return Err(TypeCheckError::NotAllCodePathsReturnValue {
                    name: name.clone(),
                    loc: identifier.loc.clone(),
                });
            }
        }
        self.scope = outer_scope;
        Ok(CheckedStatement {
            kind: CheckedStatementKind::FunctionDeclaration {
                name: name.clone(),
                args: checked_args,
                return_type,
                body: Box::new(checked_body),
            },
        })
    }

    fn type_check_function_parameters(
        &mut self,
        args: &Vec<Expression>,
    ) -> Result<Vec<CheckedVariable>, TypeCheckError> {
        let mut checked_args: Vec<CheckedVariable> = Vec::new();
        for arg in args {
            if let ExpressionKind::FunctionParameter {
                modifiers,
                identifier: arg_identifier,
                type_annotation,
            } = &arg.kind
            {
                let name = arg_identifier.text.to_owned();
                if let ExpressionKind::TypeAnnotation {
                    colon: _,
                    identifier,
                } = &type_annotation.kind
                {
                    let type_kind = self
                        .scope
                        .try_get_type(&identifier.text, identifier.loc.clone())?;
                    let arg = CheckedVariable {
                        name,
                        type_kind: type_kind.clone(),
                        declaration_loc: identifier.loc.clone(),
                    };

                    for modifier in modifiers {
                        match modifier.kind {
                            TokenKind::WithKeyword => {
                                if let TypeKind::Record(_, members) = &type_kind {
                                    for member in members {
                                        //TODO: This should use a function try_declare_lookup_override() or something, so that there can be an ordering
                                        self.scope.lookup_overrides.insert(
                                            member.name.clone(),
                                            (arg.name.clone(), member.clone()),
                                        );
                                    }
                                } else {
                                    return Err(TypeCheckError::WithCalledOnNonRecordType {
                                        type_kind,
                                        loc: modifier.loc.clone(),
                                    });
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.scope
                        .try_declare_variable(arg.clone(), arg_identifier.loc.clone())?;

                    checked_args.push(arg);
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        Ok(checked_args)
    }

    fn expect_type(expected: TypeKind, actual: TypeKind, loc: Loc) -> Result<(), TypeCheckError> {
        if expected != actual {
            return Err(TypeCheckError::TypeMismatch {
                expected,
                actual,
                loc,
            });
        }
        Ok(())
    }

    fn type_check_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<CheckedExpression, TypeCheckError> {
        match &expression.kind {
            ExpressionKind::BoolLiteral { token } => Ok(CheckedExpression {
                kind: CheckedExpressionKind::BoolLiteral {
                    value: match token.text.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => unreachable!(),
                    },
                },
                type_kind: TypeKind::Bool,
                loc: expression.loc.clone(),
            }),
            ExpressionKind::IntLiteral { token } => {
                let (type_kind, checked_expression_kind) = match self.scope.assign_context {
                    TypeKind::Unit | TypeKind::U32 => (
                        TypeKind::U32,
                        CheckedExpressionKind::U32Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::U8 => (
                        TypeKind::U8,
                        CheckedExpressionKind::U8Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::U16 => (
                        TypeKind::U16,
                        CheckedExpressionKind::U16Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::U64 => (
                        TypeKind::U64,
                        CheckedExpressionKind::U64Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::I8 => (
                        TypeKind::I8,
                        CheckedExpressionKind::I8Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::I16 => (
                        TypeKind::I16,
                        CheckedExpressionKind::I16Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::I32 => (
                        TypeKind::I32,
                        CheckedExpressionKind::I32Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::I64 => (
                        TypeKind::I64,
                        CheckedExpressionKind::I64Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::F32 => (
                        TypeKind::F32,
                        CheckedExpressionKind::F32Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::F64 => (
                        TypeKind::F64,
                        CheckedExpressionKind::F64Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    _ => {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: self.scope.assign_context.clone(),
                            actual: TypeKind::U32,
                            loc: expression.loc.clone(),
                        })
                    }
                };
                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: expression.loc.clone(),
                })
            }
            ExpressionKind::RealLiteral { token } => {
                let (type_kind, checked_expression_kind) = match self.scope.assign_context {
                    TypeKind::Unit | TypeKind::F32 | TypeKind::I32 => (
                        TypeKind::F32,
                        CheckedExpressionKind::F32Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    TypeKind::F64 | TypeKind::I64 => (
                        TypeKind::F64,
                        CheckedExpressionKind::F64Literal {
                            value: token.text.parse().expect("should not fail"),
                        },
                    ),
                    _ => unreachable!("{:?}", self.scope.assign_context),
                };
                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: expression.loc.clone(),
                })
            }
            ExpressionKind::StringLiteral { token } => Ok(CheckedExpression {
                kind: CheckedExpressionKind::StringLiteral {
                    value: token.text.to_owned(),
                },
                type_kind: TypeKind::String,
                loc: expression.loc.clone(),
            }),
            ExpressionKind::Unary { op, operand } => {
                let checked_operand = self.type_check_expression(&operand)?;

                let type_kind = Self::check_unary_expression(&op, &checked_operand.type_kind)?;

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Unary {
                        op: op.kind.clone(),
                        operand: Box::new(checked_operand),
                    },
                    type_kind,
                    loc: expression.loc.clone(),
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let checked_left = self.type_check_expression(&left)?;
                let checked_right = self.type_check_expression(&right)?;

                let type_kind = Self::check_binary_expression(
                    &checked_left.type_kind,
                    &op,
                    &checked_right.type_kind,
                )?;

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Binary {
                        left: Box::new(checked_left),
                        op: op.kind.clone(),
                        right: Box::new(checked_right),
                    },
                    type_kind,
                    loc: expression.loc.clone(),
                })
            }
            ExpressionKind::Parenthesised { expression } => {
                let checked_inner = self.type_check_expression(&expression)?;
                let checked_type = checked_inner.type_kind.clone();

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Parenthesised {
                        expression: Box::new(checked_inner),
                    },
                    type_kind: checked_type,
                    loc: expression.loc.clone(),
                })
            }
            ExpressionKind::Assignment {
                lhs,
                equals: _,
                rhs,
            } => {
                let checked_lhs = self.type_check_expression(lhs)?;
                let checked_rhs = self.type_check_expression(rhs)?;

                if !self.is_assignable_from(&checked_rhs.type_kind, &checked_lhs.type_kind) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: checked_lhs.type_kind,
                        actual: checked_rhs.type_kind,
                        loc: rhs.loc.clone(),
                    });
                }

                return Ok(CheckedExpression {
                    type_kind: checked_lhs.type_kind.clone(),
                    kind: CheckedExpressionKind::Assignment {
                        lhs: Box::new(checked_lhs),
                        rhs: Box::new(checked_rhs),
                    },
                    loc: expression.loc.clone(),
                });
            }
            ExpressionKind::Variable { identifier } => {
                return match self
                    .scope
                    .try_get_variable(&identifier.text, &identifier.loc)
                {
                    Ok(variable) => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::Variable {
                            name: variable.name.clone(),
                        },
                        type_kind: variable.type_kind.clone(),
                        loc: expression.loc.clone(),
                    }),
                    Err(_) => match self
                        .scope
                        .try_get_lookup_override(&identifier.text, &expression.loc.clone())
                    {
                        Ok((record_name, member)) => Ok(CheckedExpression {
                            kind: CheckedExpressionKind::Accessor {
                                accessee: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::Variable {
                                        name: record_name.clone(),
                                    },
                                    type_kind: member.type_kind.clone(),
                                    loc: expression.loc.clone(),
                                }),
                                member: member.name.clone(),
                            },
                            type_kind: member.type_kind.clone(),
                            loc: expression.loc.clone(),
                        }),
                        Err(e) => Err(e),
                    },
                }
            }
            ExpressionKind::TypeAnnotation { .. } => {
                unreachable!("Should be handled in another function")
            }
            ExpressionKind::FunctionCall { callee, args } => {
                let loc = callee.loc.clone();
                let mut args: Vec<Expression> = args.to_owned();
                let name = match &callee.kind {
                    ExpressionKind::Variable { identifier } => &identifier.text,
                    ExpressionKind::Accessor {
                        accessee,
                        dot: _,
                        member_identifier,
                    } => {
                        args.insert(0, *accessee.clone());

                        &member_identifier.text
                    }
                    _ => todo!("{:?}", callee.kind),
                };

                //TMP
                if name == "print" {
                    return Ok(CheckedExpression {
                        kind: CheckedExpressionKind::FunctionCall {
                            name: "printf".to_owned(),
                            args: vec![self.type_check_expression(&args[0])?],
                        },
                        type_kind: TypeKind::Unit,
                        loc,
                    });
                }
                //ENDTMP

                let checked_function = self.scope.try_get_function(&name, &callee.loc)?.clone();

                let mut checked_args: Vec<CheckedExpression> = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let checked_arg = self.type_check_expression(arg)?;

                    Self::expect_type(
                        checked_function.args.get(i).unwrap().clone(),
                        checked_arg.type_kind.clone(),
                        arg.loc.clone(),
                    )?;

                    checked_args.push(checked_arg);
                }

                return Ok(CheckedExpression {
                    kind: CheckedExpressionKind::FunctionCall {
                        name: name.clone(),
                        args: checked_args,
                    },
                    type_kind: checked_function
                        .return_type
                        .clone()
                        .unwrap_or(TypeKind::Unit),
                    loc,
                });
            }
            ExpressionKind::ArrayLiteral {
                open_square,
                elements,
                close_square: _,
            } => {
                let mut element_type: Option<TypeKind> = None;

                let mut checked_elements: Vec<CheckedExpression> = Vec::new();
                for element in elements {
                    let checked_element = self.type_check_expression(element)?;

                    match &element_type {
                        Some(el) => Self::expect_type(
                            el.clone(),
                            checked_element.type_kind.clone(),
                            element.loc.clone(),
                        )?,
                        None => {
                            element_type = Some(checked_element.type_kind.clone());
                        }
                    }
                    checked_elements.push(checked_element);
                }

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::ArrayLiteral {
                        elements: checked_elements,
                    },
                    type_kind: TypeKind::Array(
                        elements.len(),
                        Box::new(element_type.expect("TODO: type context")),
                    ),
                    loc: open_square.loc.clone(),
                })
            }
            ExpressionKind::ArrayIndex { array, index } => {
                let checked_array = self.type_check_expression(&array)?;

                if let TypeKind::Array(_, el_type) = &checked_array.type_kind.clone() {
                    let checked_index = self.type_check_expression(index)?;

                    if !Self::is_integer_type(&checked_index.type_kind) {
                        return Err(TypeCheckError::TypeMismatch {
                            expected: TypeKind::U32,
                            actual: checked_index.type_kind,
                            loc: index.loc.clone(),
                        });
                    }

                    return Ok(CheckedExpression {
                        kind: CheckedExpressionKind::ArrayIndex {
                            array: Box::new(checked_array),
                            index: Box::new(checked_index),
                        },
                        type_kind: *el_type.clone(),
                        loc: index.loc.clone(),
                    });
                }
                Err(TypeCheckError::CannotIndexType {
                    type_kind: checked_array.type_kind,
                    loc: array.loc.clone(),
                })
            }
            ExpressionKind::RecordLiteral {
                record_name,
                open_curly,
                args,
                close_curly,
            } => {
                let record_type = self
                    .scope
                    .try_get_type(record_name, open_curly.loc.clone())?;
                if let TypeKind::Record(name, members) = &record_type {
                    let n_members = members.len();

                    let mut checked_args: Vec<CheckedExpression> = Vec::new();
                    for (i, member) in members.into_iter().enumerate() {
                        match args.get(i) {
                            Some(arg) => {
                                let checked_arg = self.type_check_expression(arg)?;
                                Self::expect_type(
                                    member.type_kind.clone(),
                                    checked_arg.type_kind.clone(),
                                    arg.loc.clone(),
                                )?;
                                checked_args.push(checked_arg);
                            }
                            None => {
                                return Err(TypeCheckError::MissingArgForRecord {
                                    record_name: name.clone(),
                                    name: member.name.clone(),
                                    type_kind: member.type_kind.clone(),
                                    loc: close_curly.loc.clone(),
                                })
                            }
                        }
                    }
                    if args.len() > n_members {
                        let checked_arg = self.type_check_expression(&args[n_members])?;
                        return Err(TypeCheckError::UnexpectedArgForRecord {
                            type_kind: checked_arg.type_kind,
                            record_name: record_name.clone(),
                            loc: args[n_members].loc.clone(),
                        });
                    }
                    Ok(CheckedExpression {
                        kind: CheckedExpressionKind::RecordLiteral {
                            arguments: checked_args,
                        },
                        type_kind: record_type,
                        loc: open_curly.loc.clone(),
                    })
                } else {
                    //TODO: maybe better error
                    Err(TypeCheckError::NoSuchTypeDeclaredInScope {
                        name: record_name.clone(),
                        loc: open_curly.loc.clone(),
                    })
                }
            }
            ExpressionKind::Accessor {
                accessee,
                dot,
                member_identifier,
            } => {
                let checked_accessee = self.type_check_expression(&accessee)?;
                if let TypeKind::Record(name, members) = &checked_accessee.type_kind.clone() {
                    let member_name = &member_identifier.text;

                    if let Some(member) = members.iter().find(|m| &m.name == member_name) {
                        return Ok(CheckedExpression {
                            kind: CheckedExpressionKind::Accessor {
                                accessee: Box::new(checked_accessee),
                                member: member.name.to_string(),
                            },
                            type_kind: member.type_kind.clone(),
                            loc: dot.loc.clone(),
                        });
                    } else {
                        return Err(TypeCheckError::NoSuchMember {
                            member_name: member_name.to_string(),
                            name: name.to_string(),
                            loc: member_identifier.loc.clone(),
                        });
                    }
                }
                Err(TypeCheckError::CannotAccessType {
                    type_kind: checked_accessee.type_kind,
                    loc: accessee.loc.clone(),
                })
            }
            ExpressionKind::FunctionParameter { .. } => unreachable!(),
        }
    }

    fn is_integer_type(kind: &TypeKind) -> bool {
        match kind {
            TypeKind::Record(_, _)
            | TypeKind::Array(_, _)
            | TypeKind::Unit
            | TypeKind::Bool
            | TypeKind::F32
            | TypeKind::F64
            | TypeKind::String => false,
            TypeKind::U8
            | TypeKind::U16
            | TypeKind::U32
            | TypeKind::U64
            | TypeKind::I8
            | TypeKind::I16
            | TypeKind::I32
            | TypeKind::I64 => true,
        }
    }

    fn is_number_type(kind: &TypeKind) -> bool {
        match kind {
            TypeKind::Record(_, _)
            | TypeKind::Array(_, _)
            | TypeKind::Unit
            | TypeKind::Bool
            | TypeKind::String => false,
            TypeKind::U8
            | TypeKind::U16
            | TypeKind::U32
            | TypeKind::U64
            | TypeKind::I8
            | TypeKind::I16
            | TypeKind::I32
            | TypeKind::I64
            | TypeKind::F32
            | TypeKind::F64 => true,
        }
    }

    fn check_unary_expression(
        op: &UnaryOp,
        operand: &TypeKind,
    ) -> Result<TypeKind, TypeCheckError> {
        match op.kind {
            parser::UnaryOpKind::Negation | parser::UnaryOpKind::Identity => {
                if Self::is_number_type(&operand) {
                    return Ok(operand.clone()); //todo pick right width
                }
                return Err(TypeCheckError::UnaryOpNotImplementedForType {
                    op: op.kind.clone(),
                    operand: operand.clone(),
                    loc: op.loc.clone(),
                });
            }
            parser::UnaryOpKind::Not => {
                if operand == &TypeKind::Bool {
                    return Ok(TypeKind::Bool);
                }
                return Err(TypeCheckError::UnaryOpNotImplementedForType {
                    op: op.kind.clone(),
                    operand: operand.clone(),
                    loc: op.loc.clone(),
                });
            }
        }
    }

    fn check_binary_expression(
        left: &TypeKind,
        op: &BinaryOp,
        right: &TypeKind,
    ) -> Result<TypeKind, TypeCheckError> {
        match op.kind {
            BinaryOpKind::Add
            | BinaryOpKind::Sub
            | BinaryOpKind::Mul
            | BinaryOpKind::Div
            | BinaryOpKind::Rem => {
                if Self::is_number_type(&left) && Self::is_number_type(&right) {
                    return Ok(left.clone()); //todo pick right width
                }
                return Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                });
            }
            BinaryOpKind::Lt | BinaryOpKind::LtEq | BinaryOpKind::Gt | BinaryOpKind::GtEq => {
                if Self::is_number_type(&left) && Self::is_number_type(&right) {
                    return Ok(TypeKind::Bool);
                }
                return Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                });
            }
            BinaryOpKind::And | BinaryOpKind::Or | BinaryOpKind::Xor => {
                if left == &TypeKind::Bool && right == &TypeKind::Bool {
                    return Ok(TypeKind::Bool);
                }
                return Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                });
            }
            BinaryOpKind::Eq | BinaryOpKind::NEq => {
                if Self::is_number_type(&left) && Self::is_number_type(&right) {
                    return Ok(TypeKind::Bool);
                }
                if left == right {
                    return Ok(TypeKind::Bool);
                }
                return Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                });
            }
        }
    }

    fn all_paths_return_value(statement: &CheckedStatement) -> bool {
        match &statement.kind {
            CheckedStatementKind::Block { statements } => {
                if statements.len() == 0 {
                    return false;
                }
                for s in statements {
                    if Self::all_paths_return_value(s) {
                        return true;
                    }
                }
                //TODO warn about unreachable code
                return false;
            }
            CheckedStatementKind::If {
                condition: _,
                body,
                else_body,
            } => {
                return match else_body {
                    Some(else_body) => {
                        Self::all_paths_return_value(body)
                            && Self::all_paths_return_value(&else_body)
                    }
                    None => false,
                };
            }
            CheckedStatementKind::Return { .. } => true,
            CheckedStatementKind::Expression { .. }
            | CheckedStatementKind::VariableDeclaration { .. }
            | CheckedStatementKind::FunctionDeclaration { .. }
            | CheckedStatementKind::While { .. }
            | CheckedStatementKind::Record { .. }
            | CheckedStatementKind::Break
            | CheckedStatementKind::Continue => false,
        }
    }

    fn is_assignable_from(&self, from: &TypeKind, to: &TypeKind) -> bool {
        if from == to {
            return true;
        }
        //TODO more complex coersion
        return false;
    }
}
