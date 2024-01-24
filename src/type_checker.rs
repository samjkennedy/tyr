use core::{fmt, panic};
use std::{collections::HashMap, path::PathBuf};

use crate::{
    lexer::{lex_file, span_locs, Loc, Token, TokenKind},
    parser::{
        self, BinaryOp, BinaryOpKind, Expression, ExpressionKind, Location, Parser, Statement,
        StatementKind, TypeExpressionKind, UnaryOp, UnaryOpKind,
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
    Char,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
    Array(usize, Box<TypeKind>),
    Slice(Box<TypeKind>),
    Record(String, Vec<TypeKind>, Vec<CheckedVariable>),
    Range(Box<TypeKind>),
    Enum(String, Vec<String>),
    GenericParameter(String),
    Optional(Box<TypeKind>),
    //TMP
    File,
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
            TypeKind::Char => write!(f, "char"),
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
            TypeKind::Slice(inner_type) => {
                write!(f, "[]{}", inner_type)
            }
            TypeKind::Record(name, generic_parameters, _fields) => {
                if generic_parameters.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name,
                        generic_parameters
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            TypeKind::Range(inner_type) => {
                write!(f, "range<{}>", inner_type)
            }
            TypeKind::Enum(name, _) => write!(f, "{}", name),
            TypeKind::GenericParameter(name) => write!(f, "{}", name),
            TypeKind::File => write!(f, "File"),
            TypeKind::Optional(base_type) => write!(f, "?{}", base_type),
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
        type_name: String,
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
    WrongNumberOfTypeArguments {
        expected: usize,
        actual: usize,
        loc: Loc,
    },
    ArgLengthMismatch {
        expected: usize,
        actual: usize,
        loc: Loc,
    },
    CannotDeduceTypeFromContext(Loc),
    NonNillableType(TypeKind, Loc),
    CannotPassOptionalTypeToFunction {
        type_kind: TypeKind,
        loc: Loc,
    },
    NoSuchNamespaceDeclaredInScope {
        namespace: Token,
    },
    CannotIterateType {
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
        generic_params: Vec<TypeKind>,
        members: Vec<CheckedVariable>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
    },
    Break,
    Continue,
    MatchCases {
        cases: Vec<CheckedExpression>,
    },
    Match {
        expression: CheckedExpression,
        cases: Box<CheckedStatement>,
    },
    NoOp,
    ForIn {
        iterator: CheckedVariable,
        iterable: CheckedExpression,
        body: Box<CheckedStatement>,
    },
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
        variable: CheckedVariable,
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
    SafeAccessor {
        accessee: Box<CheckedExpression>,
        member: String,
    },
    NilCoalesce {
        optional: Box<CheckedExpression>,
        default: Box<CheckedExpression>,
    },
    StringLiteral {
        value: String,
    },
    StaticAccessor {
        name: String,
        member: CheckedVariable,
    },
    MatchCase {
        pattern: Box<CheckedExpression>,
        result: Box<CheckedExpression>,
    },
    Nil,
    ForceUnwrap {
        expression: Box<CheckedExpression>,
    },
    Range {
        lower: Box<CheckedExpression>,
        upper: Box<CheckedExpression>,
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
    modules: HashMap<String, Scope>,
    variables: HashMap<String, CheckedVariable>,
    functions: HashMap<String, CheckedFunction>,
    lookup_overrides: HashMap<String, (String, CheckedVariable)>,
    types: HashMap<String, TypeKind>,
    generic_erasures: HashMap<String, TypeKind>,
    return_context: TypeKind,
    assign_context: Vec<TypeKind>,
}

impl Scope {
    fn new_global_scope() -> Scope {
        let mut scope = Scope {
            parent: None,
            modules: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            lookup_overrides: HashMap::new(),
            types: HashMap::new(),
            generic_erasures: HashMap::new(),
            return_context: TypeKind::Unit,
            assign_context: vec![],
        };

        // TODO: package this into a module that must be imported
        scope.types.insert("File".to_owned(), TypeKind::File);
        // let mode_variants = vec!["Read".to_string(), "Write".to_string()];
        // let mode_enum = TypeKind::Enum("Mode".to_string(), mode_variants.clone());

        // //This one should really be defined in tyr code
        // scope.types.insert("Mode".to_string(), mode_enum.clone());

        // let mut module = Scope {
        //     parent: None,
        //     modules: HashMap::new(),
        //     variables: HashMap::new(),
        //     functions: HashMap::new(),
        //     lookup_overrides: HashMap::new(),
        //     types: HashMap::new(),
        //     generic_erasures: HashMap::new(),
        //     return_context: TypeKind::Unit,
        //     assign_context: vec![TypeKind::Unit],
        // };
        // for variant in mode_variants {
        //     module
        //         .try_declare_variable(
        //             CheckedVariable {
        //                 name: variant.to_string(),
        //                 type_kind: mode_enum.clone(),
        //                 declaration_loc: Loc {
        //                     file: "".to_owned(),
        //                     row: 0,
        //                     col: 0,
        //                     len: 0,
        //                 },
        //             },
        //             Loc {
        //                 file: "".to_owned(),
        //                 row: 0,
        //                 col: 0,
        //                 len: 0,
        //             },
        //         )
        //         .unwrap();
        // }

        // scope.modules.insert("Mode".to_string(), module);

        scope.functions.insert(
            "open_file".to_owned(),
            CheckedFunction {
                name: "open_file".to_owned(),
                args: vec![TypeKind::String, TypeKind::String],
                return_type: Some(TypeKind::Optional(Box::new(TypeKind::File))),
                declaration_loc: Loc {
                    file: "".to_owned(),
                    row: 0,
                    col: 0,
                    len: 0,
                },
            },
        );
        scope.functions.insert(
            "read_char".to_owned(),
            CheckedFunction {
                name: "read_char".to_owned(),
                args: vec![TypeKind::File],
                return_type: Some(TypeKind::Char),
                declaration_loc: Loc {
                    file: "".to_owned(),
                    row: 0,
                    col: 0,
                    len: 0,
                },
            },
        );
        scope.functions.insert(
            "write".to_owned(),
            CheckedFunction {
                name: "write".to_owned(),
                args: vec![TypeKind::File, TypeKind::String],
                return_type: None,
                declaration_loc: Loc {
                    file: "".to_owned(),
                    row: 0,
                    col: 0,
                    len: 0,
                },
            },
        );
        scope.functions.insert(
            "eof".to_owned(),
            CheckedFunction {
                name: "eof".to_owned(),
                args: vec![TypeKind::File],
                return_type: Some(TypeKind::Bool),
                declaration_loc: Loc {
                    file: "".to_owned(),
                    row: 0,
                    col: 0,
                    len: 0,
                },
            },
        );
        scope.functions.insert(
            "close".to_owned(),
            CheckedFunction {
                name: "close".to_owned(),
                args: vec![TypeKind::File],
                return_type: None,
                declaration_loc: Loc {
                    file: "".to_owned(),
                    row: 0,
                    col: 0,
                    len: 0,
                },
            },
        );

        scope
    }

    fn new_inner_scope(parent: Scope) -> Scope {
        Scope {
            return_context: parent.return_context.clone(),
            assign_context: parent.assign_context.clone(),
            parent: Some(Box::new(parent)),
            modules: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            lookup_overrides: HashMap::new(),
            types: HashMap::new(),
            generic_erasures: HashMap::new(),
        }
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

    fn try_get_generic_erasure(&self, name: &String) -> Option<TypeKind> {
        return match self.generic_erasures.get(name) {
            Some(concrete_type) => Some(concrete_type.clone()),
            None => {
                return match &self.parent {
                    Some(parent) => parent.try_get_generic_erasure(name),
                    None => None,
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

    fn try_declare_type(&mut self, type_kind: TypeKind, loc: Loc) -> Result<(), TypeCheckError> {
        match &type_kind {
            TypeKind::Record(name, ..) => {
                if self.types.contains_key(name) {
                    return Err(TypeCheckError::TypeAlreadyDeclared {
                        type_name: name.clone(),
                        loc: loc.clone(),
                    });
                }
                self.types.insert(name.clone(), type_kind);
                Ok(())
            }
            TypeKind::GenericParameter(name) => {
                if self.types.contains_key(name) {
                    return Err(TypeCheckError::TypeAlreadyDeclared {
                        type_name: name.clone(),
                        loc: loc.clone(),
                    });
                }
                self.types.insert(name.clone(), type_kind);
                Ok(())
            }
            TypeKind::Enum(name, variants) => {
                if self.types.contains_key(name) {
                    return Err(TypeCheckError::TypeAlreadyDeclared {
                        type_name: name.clone(),
                        loc: loc.clone(),
                    });
                }
                if self.modules.contains_key(name) {
                    return Err(TypeCheckError::TypeAlreadyDeclared {
                        type_name: name.clone(),
                        loc: loc.clone(),
                    });
                }
                self.types.insert(name.clone(), type_kind.clone());

                let mut module = Scope::new_global_scope();
                module.types.insert(name.clone(), type_kind.clone());
                for variant in variants {
                    module.try_declare_variable(
                        CheckedVariable {
                            name: variant.to_string(),
                            type_kind: type_kind.clone(),
                            declaration_loc: loc.clone(),
                        },
                        loc.clone(),
                    )?;
                }

                self.modules.insert(name.clone(), module);
                Ok(())
            }
            _ => panic!("Cannot declare {} types", type_kind),
        }
    }

    fn try_get_module_member(
        &self,
        module_name: &String,
        member_name: &String,
        loc: &Loc,
    ) -> Result<&CheckedVariable, TypeCheckError> {
        match self.modules.get(module_name) {
            Some(module) => {
                return module.try_get_variable(member_name, loc);
            }
            None => {
                return match &self.parent {
                    Some(parent) => parent.try_get_module_member(module_name, member_name, loc),
                    None => Err(TypeCheckError::NoSuchVariableDeclaredInScope {
                        //TODO: maybe a better error
                        name: member_name.clone(),
                        loc: loc.clone(),
                    }),
                };
            }
        };
    }

    fn try_get_type(
        &self,
        type_expression_kind: &TypeExpressionKind,
    ) -> Result<TypeKind, TypeCheckError> {
        match type_expression_kind {
            TypeExpressionKind::Basic { identifier } => {
                let name = &identifier.text;
                return match name.as_str() {
                    "bool" => Ok(TypeKind::Bool),
                    "u8" => Ok(TypeKind::U8),
                    "u16" => Ok(TypeKind::U16),
                    "u32" => Ok(TypeKind::U32),
                    "u64" => Ok(TypeKind::U64),
                    "i8" => Ok(TypeKind::I8),
                    "char" => Ok(TypeKind::Char),
                    "i16" => Ok(TypeKind::I16),
                    "i32" => Ok(TypeKind::I32),
                    "i64" => Ok(TypeKind::I64),
                    "f32" => Ok(TypeKind::F32),
                    "f64" => Ok(TypeKind::F64),
                    "string" => Ok(TypeKind::String),
                    _ => {
                        return match self.types.get(name) {
                            Some(type_kind) => Ok(type_kind.clone()),
                            None => match &self.parent {
                                Some(parent) => parent.try_get_type(type_expression_kind),
                                None => Err(TypeCheckError::NoSuchTypeDeclaredInScope {
                                    name: name.clone(),
                                    loc: identifier.loc.clone(),
                                }),
                            },
                        }
                    }
                };
            }
            TypeExpressionKind::Array {
                open_square: _,
                length,
                close_square: _,
                element_type,
            } => {
                let element_type_kind = self.try_get_type(element_type)?;

                Ok(TypeKind::Array(*length, Box::new(element_type_kind)))
            }
            TypeExpressionKind::Slice {
                open_square: _,
                close_square: _,
                element_type,
            } => {
                let element_type_kind = self.try_get_type(element_type)?;
                Ok(TypeKind::Slice(Box::new(element_type_kind)))
            }
            TypeExpressionKind::Generic {
                generic_type,
                open_angle,
                generic_parameter_types,
                close_angle,
            } => {
                if let TypeKind::Record(name, generic_params, members) =
                    self.try_get_type(generic_type)?
                {
                    let mut erased_generics = HashMap::new();
                    if generic_parameter_types.len() != generic_params.len() {
                        return Err(TypeCheckError::WrongNumberOfTypeArguments {
                            expected: generic_params.len(),
                            actual: generic_parameter_types.len(),
                            loc: span_locs(&open_angle.loc, &close_angle.loc),
                        });
                    }
                    for (i, param_type) in generic_parameter_types.iter().enumerate() {
                        match generic_params.get(i) {
                            Some(param) => match param {
                                TypeKind::GenericParameter(generic_param_name) => {
                                    let erased_type = self.try_get_type(param_type)?;

                                    erased_generics.insert(
                                        format!("{}_{}", name, generic_param_name),
                                        erased_type,
                                    );
                                }
                                _ => todo!(), //will this ever happen?
                            },
                            None => unreachable!(),
                        }
                    }

                    let mut erased_members: Vec<CheckedVariable> = Vec::new();
                    for member in members {
                        if let TypeKind::GenericParameter(name) = member.type_kind {
                            let erased_member = CheckedVariable {
                                name: member.name,
                                type_kind: erased_generics
                                    .get(&name)
                                    .expect("should not fail")
                                    .clone(),
                                declaration_loc: member.declaration_loc,
                            };
                            erased_members.push(erased_member)
                        } else {
                            erased_members.push(member);
                        }
                    }

                    return Ok(TypeKind::Record(
                        name,
                        erased_generics.values().cloned().collect(),
                        erased_members,
                    ));
                } else {
                    todo!("other generic types, e.g. enums")
                }
            }
            TypeExpressionKind::GenericParameter { type_name, param } => {
                let name = format!("{}_{}", type_name, param.text);
                match self.types.get(&name) {
                    Some(type_kind) => Ok(type_kind.clone()),
                    None => match &self.parent {
                        Some(parent) => parent.try_get_type(type_expression_kind),
                        None => Err(TypeCheckError::NoSuchTypeDeclaredInScope {
                            name: name.clone(),
                            loc: param.loc.clone(),
                        }),
                    },
                }
            }
            TypeExpressionKind::Optional {
                question_mark: _,
                base_type,
            } => {
                let checked_base_type = self.try_get_type(base_type)?;

                Ok(TypeKind::Optional(Box::new(checked_base_type)))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub imports: HashMap<String, Module>,
    pub types: Vec<TypeKind>,
    pub statements: Vec<CheckedStatement>,
    //TODO: eventually move all global declarations here, variables, functions, all types, etc
}

impl Module {
    fn new(name: String) -> Module {
        Module {
            name,
            imports: HashMap::new(),
            types: Vec::new(),
            statements: Vec::new(),
        }
    }
}

pub struct TypeChecker {
    module: Module,
    scope: Scope,
}

impl TypeChecker {
    pub fn new(module_name: String) -> TypeChecker {
        TypeChecker {
            module: Module::new(module_name),
            scope: Scope::new_global_scope(),
        }
    }

    pub fn type_check_statements(
        &mut self,
        statements: Vec<Statement>,
    ) -> Result<Module, TypeCheckError> {
        for statement in statements {
            let checked_statement = self.type_check_statement(&statement)?;

            self.module.statements.push(checked_statement);
        }

        Ok(self.module.clone())
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
                    checked_statements.push(self.type_check_statement(statement)?);
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
                        type_expression_kind,
                    } = &type_annotation.kind
                    {
                        let type_kind = self.scope.try_get_type(type_expression_kind)?;

                        self.scope.assign_context.push(type_kind.clone());

                        let mut checked_initialiser = self.type_check_expression(initialiser)?;

                        self.scope.assign_context.pop();

                        self.expect_type(
                            type_kind.clone(),
                            checked_initialiser.type_kind.clone(),
                            initialiser.kind.get_loc().clone(),
                        )?;

                        checked_initialiser.type_kind = type_kind;

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

                self.expect_type(
                    TypeKind::Bool,
                    checked_condition.type_kind.clone(),
                    condition.kind.get_loc().clone(),
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
                        else_body: checked_else_body.map(Box::new),
                    },
                })
            }
            StatementKind::While {
                while_keyword: _,
                condition,
                body,
            } => {
                let checked_condition = self.type_check_expression(condition)?;

                self.expect_type(
                    TypeKind::Bool,
                    checked_condition.type_kind.clone(),
                    condition.kind.get_loc().clone(),
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
                    self.scope
                        .assign_context
                        .push(self.scope.return_context.clone());
                    let checked_return_value = self.type_check_expression(return_value)?;
                    self.scope.assign_context.pop();

                    self.expect_type(
                        self.scope.return_context.clone(),
                        checked_return_value.type_kind.clone(),
                        return_value.kind.get_loc().clone(),
                    )?;
                    return Ok(CheckedStatement {
                        kind: CheckedStatementKind::Return {
                            return_value: Some(checked_return_value),
                        },
                    });
                }
                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Return { return_value: None },
                })
            }
            StatementKind::Record {
                record_keyword,
                identifier,
                generic_type_parameters,
                members,
            } => {
                let mut checked_members: Vec<CheckedVariable> = Vec::new();

                let mut generic_params = Vec::new();
                for gtp in generic_type_parameters {
                    self.scope.try_declare_type(
                        TypeKind::GenericParameter(format!(
                            "{}_{}",
                            identifier.text,
                            gtp.text.clone()
                        )),
                        gtp.loc.clone(),
                    )?;
                    generic_params.push(TypeKind::GenericParameter(gtp.text.clone()));
                }

                for (member_identifier, type_annotation) in members {
                    if let ExpressionKind::TypeAnnotation {
                        colon: _,
                        type_expression_kind,
                    } = &type_annotation.kind
                    {
                        let mut type_kind = TypeKind::Unit;

                        if !generic_params.is_empty() {
                            if let TypeExpressionKind::Basic {
                                identifier: generic_identifier,
                            } = type_expression_kind
                            {
                                for gtp in generic_type_parameters {
                                    if gtp.text == generic_identifier.text {
                                        type_kind = self.scope.try_get_type(
                                            &TypeExpressionKind::GenericParameter {
                                                type_name: identifier.text.clone(),
                                                param: gtp.clone(),
                                            },
                                        )?;
                                        break;
                                    }
                                }
                                if type_kind == TypeKind::Unit {
                                    type_kind = self.scope.try_get_type(type_expression_kind)?;
                                }
                            } else {
                                type_kind = self.scope.try_get_type(type_expression_kind)?;
                            }
                        } else {
                            type_kind = self.scope.try_get_type(type_expression_kind)?;
                        }

                        checked_members.push(CheckedVariable {
                            name: member_identifier.text.clone(),
                            type_kind,
                            declaration_loc: member_identifier.loc.clone(),
                        })
                    } else {
                        panic!("non-type annotation expression made it into here")
                    }
                }

                let record = TypeKind::Record(
                    identifier.text.clone(),
                    generic_params.clone(),
                    checked_members.clone(),
                );

                self.scope
                    .try_declare_type(record.clone(), record_keyword.loc.clone())?;

                if generic_params.is_empty() {
                    self.module.types.push(record);
                }

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Record {
                        name: identifier.text.clone(),
                        generic_params,
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
            StatementKind::Enum {
                enum_keyword,
                identifier,
                variants,
            } => {
                let mut checked_variants = Vec::new();
                for variant in variants {
                    if checked_variants.contains(&variant.text) {
                        todo!("duplicate variant error");
                    }
                    checked_variants.push(variant.text.clone());
                }

                let the_enum = TypeKind::Enum(identifier.text.clone(), checked_variants.clone());

                self.scope
                    .try_declare_type(the_enum, enum_keyword.loc.clone())?;

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Enum {
                        name: identifier.text.clone(),
                        variants: checked_variants,
                    },
                })
            }
            StatementKind::MatchCases { cases } => {
                let mut checked_cases = Vec::new();
                for case in cases {
                    checked_cases.push(self.type_check_expression(case)?);
                }
                Ok(CheckedStatement {
                    kind: CheckedStatementKind::MatchCases {
                        cases: checked_cases,
                    },
                })
            }
            StatementKind::Match {
                match_keyword: _,
                expression,
                cases,
            } => {
                let checked_expression = self.type_check_expression(expression)?;
                let checked_cases =
                    self.type_check_match_cases(cases, checked_expression.type_kind.clone())?;
                Ok(CheckedStatement {
                    kind: CheckedStatementKind::Match {
                        expression: checked_expression,
                        cases: Box::new(checked_cases),
                    },
                })
            }
            StatementKind::Import {
                import_keyword: _,
                path,
            } => {
                let module_name = path.last().unwrap().text.clone();
                let mut pb: PathBuf = PathBuf::from(".");
                for path_component in path {
                    pb.push(path_component.text.clone());
                }
                pb.set_extension("tyr");

                let module = match pb.into_os_string().into_string() {
                    Ok(p) => {
                        let res = lex_file(p);
                        match res {
                            Ok(tokens) => {
                                let mut parser = Parser::new();
                                let statements: Vec<Statement> = match parser
                                    .parse_statements(tokens)
                                {
                                    Ok(statements) => statements,
                                    Err(parse_error) => {
                                        todo!("hand this to the diagnoster:\n {:?}", parse_error)
                                    }
                                };

                                let mut module_type_checker = TypeChecker::new(module_name.clone());

                                module_type_checker.type_check_statements(statements)?
                            }
                            Err(lex_error) => {
                                todo!("hand this to the diagnoster:\n {:?}", lex_error)
                            }
                        }
                    }
                    Err(_e) => todo!(),
                };
                //For realsies it would probably a good idea to build a graph of imports
                self.module.imports.insert(module_name.clone(), module);

                Ok(CheckedStatement {
                    kind: CheckedStatementKind::NoOp,
                })
            }
            StatementKind::ForIn {
                for_keyword: _,
                iterator,
                in_keyword: _is_digit,
                iterable,
                body,
            } => {
                let checked_iterable = self.type_check_expression(iterable)?;

                //todo: too much cloning here
                let outer_scope = self.scope.clone();
                self.scope = Scope::new_inner_scope(outer_scope.clone());

                match &checked_iterable.type_kind {
                    TypeKind::Range(range_type) => {
                        let checked_iterator = CheckedVariable {
                            name: iterator.text.clone(),
                            type_kind: *range_type.clone(),
                            declaration_loc: iterator.loc.clone(),
                        };
                        self.scope
                            .try_declare_variable(checked_iterator.clone(), iterator.loc.clone())?;

                        let checked_body = self.type_check_statement(body)?;

                        Ok(CheckedStatement {
                            kind: CheckedStatementKind::ForIn {
                                iterator: checked_iterator,
                                iterable: checked_iterable,
                                body: Box::new(checked_body),
                            },
                        })
                    }
                    TypeKind::Array(_, inner_type) => {
                        let checked_iterator = CheckedVariable {
                            name: iterator.text.clone(),
                            type_kind: *inner_type.clone(),
                            declaration_loc: iterator.loc.clone(),
                        };
                        self.scope
                            .try_declare_variable(checked_iterator.clone(), iterator.loc.clone())?;

                        let checked_body = self.type_check_statement(body)?;

                        Ok(CheckedStatement {
                            kind: CheckedStatementKind::ForIn {
                                iterator: checked_iterator,
                                iterable: checked_iterable,
                                body: Box::new(checked_body),
                            },
                        })
                    }
                    _ => Err(TypeCheckError::CannotIterateType {
                        type_kind: checked_iterable.type_kind,
                        loc: checked_iterable.loc,
                    }),
                }
            }
        }
    }

    fn type_check_match_cases(
        &mut self,
        cases: &Statement,
        expression_type_kind: TypeKind,
    ) -> Result<CheckedStatement, TypeCheckError> {
        if let StatementKind::MatchCases { cases } = &cases.kind {
            let mut checked_cases = Vec::new();
            for case in cases {
                if let ExpressionKind::MatchCase {
                    pattern,
                    fat_arrow: _,
                    result,
                } = &case.kind
                {
                    let checked_pattern = self.type_check_expression(pattern)?;
                    self.expect_type(
                        expression_type_kind.clone(),
                        checked_pattern.type_kind.clone(),
                        pattern.kind.get_loc(),
                    )?;

                    let checked_result = self.type_check_expression(result)?;

                    let checked_case = CheckedExpression {
                        kind: CheckedExpressionKind::MatchCase {
                            pattern: Box::new(checked_pattern),
                            result: Box::new(checked_result),
                        },
                        type_kind: TypeKind::Unit, //TODO: allow returning values from matches
                        loc: case.kind.get_loc(),
                    };

                    checked_cases.push(checked_case);
                } else {
                    unreachable!()
                }
            }
            Ok(CheckedStatement {
                kind: CheckedStatementKind::MatchCases {
                    cases: checked_cases,
                },
            })
        } else {
            unreachable!()
        }
    }

    fn type_check_function_definition(
        &mut self,
        identifier: &Token,
        return_annotation: &Option<Expression>,
        args: &Vec<Expression>,
        function_keyword: &Token,
        body: &Statement,
    ) -> Result<CheckedStatement, TypeCheckError> {
        let name = &identifier.text;
        let return_type: Option<TypeKind> = match return_annotation {
            Some(type_annotation) => {
                if let ExpressionKind::TypeAnnotation {
                    colon: _,
                    type_expression_kind,
                } = &type_annotation.kind
                {
                    let type_kind = self.scope.try_get_type(type_expression_kind)?;
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
                    type_expression_kind,
                } = &type_annotation.kind
                {
                    checked_args.push(self.scope.try_get_type(type_expression_kind)?)
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
        if return_type.is_some() && !Self::all_paths_return_value(&checked_body) {
            return Err(TypeCheckError::NotAllCodePathsReturnValue {
                name: name.clone(),
                loc: identifier.loc.clone(),
            });
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
                    type_expression_kind,
                } = &type_annotation.kind
                {
                    let type_kind = self.scope.try_get_type(type_expression_kind)?;
                    let arg = CheckedVariable {
                        name,
                        type_kind: type_kind.clone(),
                        declaration_loc: span_locs(
                            &arg_identifier.loc,
                            &type_expression_kind.get_loc(),
                        ),
                    };

                    for modifier in modifiers {
                        match modifier.kind {
                            TokenKind::WithKeyword => {
                                if let TypeKind::Record(_, _, members) = &type_kind {
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

    fn expect_type(
        &mut self,
        expected: TypeKind,
        actual: TypeKind,
        loc: Loc,
    ) -> Result<(), TypeCheckError> {
        if expected == actual {
            return Ok(());
        }
        if let TypeKind::GenericParameter(name) = &expected {
            match self.scope.try_get_generic_erasure(name) {
                Some(concrete_type) => {
                    return self.expect_type(concrete_type.clone(), actual, loc);
                }
                None => {
                    return Err(TypeCheckError::TypeMismatch {
                        expected,
                        actual,
                        loc,
                    })
                }
            }
        }
        if let TypeKind::Array(_, from_el_type) = &expected {
            if let TypeKind::Slice(to_el_type) = actual {
                return self.expect_type(*from_el_type.clone(), *to_el_type, loc);
            }
        }
        if let TypeKind::Record(_expected_name, expected_generic_params, _) = &expected {
            if let TypeKind::Record(_actual_name, actual_generic_params, _) = &actual {
                //TODO: There has to be a better condition
                if !expected_generic_params.is_empty()
                    && expected_generic_params.len() == actual_generic_params.len()
                    && !self.module.types.contains(&expected)
                {
                    self.module.types.push(expected);
                }
                return Ok(());
            }
        }
        Err(TypeCheckError::TypeMismatch {
            expected: expected.clone(),
            actual,
            loc,
        })
    }

    fn type_check_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<CheckedExpression, TypeCheckError> {
        //TODO: the generic matching needs to be applied to ALL expressions, not just literals!!!!
        //same for optionals

        let checked_expression_result: Result<CheckedExpression, TypeCheckError> = match &expression
            .kind
        {
            ExpressionKind::BoolLiteral { token } => {
                let (type_kind, checked_expression_kind) =
                    match self.scope.assign_context.last().unwrap() {
                        TypeKind::Bool => (
                            TypeKind::Bool,
                            CheckedExpressionKind::BoolLiteral {
                                value: match token.text.as_str() {
                                    "true" => true,
                                    "false" => false,
                                    _ => unreachable!(),
                                },
                            },
                        ),
                        TypeKind::GenericParameter(name) => {
                            match self.scope.try_get_generic_erasure(name) {
                                Some(concrete_type) => {
                                    self.expect_type(
                                        concrete_type.clone(),
                                        TypeKind::Bool,
                                        token.loc.clone(),
                                    )?;
                                    (
                                        concrete_type,
                                        CheckedExpressionKind::BoolLiteral {
                                            value: token.text.parse().expect("should not fail"),
                                        },
                                    )
                                }
                                None => {
                                    self.scope
                                        .generic_erasures
                                        .insert(name.clone(), TypeKind::Bool);
                                    (
                                        TypeKind::Bool,
                                        CheckedExpressionKind::BoolLiteral {
                                            value: token.text.parse().expect("should not fail"),
                                        },
                                    )
                                }
                            }
                        }
                        _ => {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: self.scope.assign_context.last().unwrap().clone(),
                                actual: TypeKind::Bool,
                                loc: expression.kind.get_loc().clone(),
                            })
                        }
                    };
                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::IntLiteral { token } => {
                let (type_kind, checked_expression_kind) =
                    match self.scope.assign_context.clone().last() {
                        //oof that clone
                        Some(assign_context_type) => match assign_context_type {
                            TypeKind::U32 => (
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
                            TypeKind::GenericParameter(name) => {
                                match self.scope.try_get_generic_erasure(name) {
                                    Some(concrete_type) => {
                                        self.expect_type(
                                            concrete_type.clone(),
                                            TypeKind::F32,
                                            token.loc.clone(),
                                        )?;
                                        (
                                            concrete_type,
                                            CheckedExpressionKind::F32Literal {
                                                value: token.text.parse().expect("should not fail"),
                                            },
                                        )
                                    }
                                    None => {
                                        self.scope
                                            .generic_erasures
                                            .insert(name.clone(), TypeKind::F32);
                                        (
                                            TypeKind::F32,
                                            CheckedExpressionKind::F32Literal {
                                                value: token.text.parse().expect("should not fail"),
                                            },
                                        )
                                    }
                                }
                            }
                            TypeKind::Optional(base_type) => {
                                self.scope.assign_context.push(*base_type.clone());

                                let checked_expr = self.type_check_expression(expression)?;

                                self.scope.assign_context.pop();

                                (TypeKind::Optional(base_type.clone()), checked_expr.kind)
                            }
                            _ => {
                                return Err(TypeCheckError::TypeMismatch {
                                    expected: self.scope.assign_context.last().unwrap().clone(),
                                    actual: TypeKind::I32,
                                    loc: expression.kind.get_loc().clone(),
                                })
                            }
                        },
                        None => (
                            TypeKind::I32,
                            CheckedExpressionKind::I32Literal {
                                value: token.text.parse().expect("should not fail"),
                            },
                        ),
                    };
                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::RealLiteral { token } => {
                let (type_kind, checked_expression_kind) =
                    match self.scope.assign_context.last().unwrap() {
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
                        TypeKind::GenericParameter(name) => {
                            match self.scope.try_get_generic_erasure(name) {
                                Some(concrete_type) => {
                                    self.expect_type(
                                        concrete_type.clone(),
                                        TypeKind::F32,
                                        token.loc.clone(),
                                    )?;
                                    (
                                        concrete_type,
                                        CheckedExpressionKind::F32Literal {
                                            value: token.text.parse().expect("should not fail"),
                                        },
                                    )
                                }
                                None => {
                                    self.scope
                                        .generic_erasures
                                        .insert(name.clone(), TypeKind::F32);
                                    (
                                        TypeKind::F32,
                                        CheckedExpressionKind::F32Literal {
                                            value: token.text.parse().expect("should not fail"),
                                        },
                                    )
                                }
                            }
                        }
                        _ => unreachable!("{:?}", self.scope.assign_context),
                    };
                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::StringLiteral { token } => Ok(CheckedExpression {
                kind: CheckedExpressionKind::StringLiteral {
                    value: token.text.to_owned(),
                },
                type_kind: TypeKind::String,
                loc: expression.kind.get_loc().clone(),
            }),
            ExpressionKind::Unary { op, operand } => {
                let checked_operand = self.type_check_expression(operand)?;

                let type_kind = Self::check_unary_expression(op, &checked_operand.type_kind)?;

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Unary {
                        op: op.kind.clone(),
                        operand: Box::new(checked_operand),
                    },
                    type_kind,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::Binary { left, op, right } => {
                let checked_left = self.type_check_expression(left)?;

                self.scope
                    .assign_context
                    .push(checked_left.type_kind.clone());

                let checked_right = self.type_check_expression(right)?;

                self.scope.assign_context.pop();

                let type_kind = Self::check_binary_expression(
                    &checked_left.type_kind,
                    op,
                    &checked_right.type_kind,
                )?;

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Binary {
                        left: Box::new(checked_left),
                        op: op.kind.clone(),
                        right: Box::new(checked_right),
                    },
                    type_kind,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::Parenthesised { expression } => {
                let checked_inner = self.type_check_expression(expression)?;
                let checked_type = checked_inner.type_kind.clone();

                Ok(CheckedExpression {
                    kind: CheckedExpressionKind::Parenthesised {
                        expression: Box::new(checked_inner),
                    },
                    type_kind: checked_type,
                    loc: expression.kind.get_loc().clone(),
                })
            }
            ExpressionKind::Assignment {
                lhs,
                equals: _,
                rhs,
            } => {
                let checked_lhs = self.type_check_expression(lhs)?;

                self.scope
                    .assign_context
                    .push(checked_lhs.type_kind.clone());

                let checked_rhs = self.type_check_expression(rhs)?;

                self.scope.assign_context.pop();

                if !self.is_assignable_from(&checked_rhs.type_kind, &checked_lhs.type_kind) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: checked_lhs.type_kind,
                        actual: checked_rhs.type_kind,
                        loc: rhs.kind.get_loc().clone(),
                    });
                }

                return Ok(CheckedExpression {
                    type_kind: checked_lhs.type_kind.clone(),
                    kind: CheckedExpressionKind::Assignment {
                        lhs: Box::new(checked_lhs),
                        rhs: Box::new(checked_rhs),
                    },
                    loc: expression.kind.get_loc().clone(),
                });
            }
            ExpressionKind::Variable { identifier } => {
                return match self
                    .scope
                    .try_get_variable(&identifier.text, &identifier.loc)
                {
                    Ok(variable) => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::Variable {
                            variable: variable.clone(),
                        },
                        type_kind: variable.type_kind.clone(),
                        loc: expression.kind.get_loc().clone(),
                    }),
                    // Err(_) => match self.scope.try_get_lookup_override(
                    //     &identifier.text,
                    //     &expression.kind.get_loc().clone(),
                    // ) {
                    //     Ok((record_name, member)) => Ok(CheckedExpression {
                    //         kind: CheckedExpressionKind::Accessor {
                    //             accessee: Box::new(CheckedExpression {
                    //                 kind: CheckedExpressionKind::Variable {
                    //                     name: record_name.clone(),
                    //                 },
                    //                 type_kind: member.type_kind.clone(),
                    //                 loc: expression.kind.get_loc().clone(),
                    //             }),
                    //             member: member.name.clone(),
                    //         },
                    //         type_kind: member.type_kind.clone(),
                    //         loc: expression.kind.get_loc().clone(),
                    //     }),
                    Err(e) => Err(e),
                };
            }
            ExpressionKind::TypeAnnotation { .. } => {
                unreachable!("Should be handled in another function")
            }
            ExpressionKind::FunctionCall {
                callee,
                open_paren,
                args,
                close_paren,
            } => {
                let loc = callee.kind.get_loc().clone();
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
                    ExpressionKind::SafeAccessor {
                        accessee,
                        question_dot: _,
                        member_identifier,
                    } => {
                        args.insert(0, *accessee.clone());

                        &member_identifier.text
                    }
                    _ => todo!("{:?}", callee.kind),
                };

                //TMP
                if name == "print" {
                    let checked_arg = self.type_check_expression(&args[0])?;
                    if let TypeKind::Optional(_base_type) = &checked_arg.type_kind {
                        return Err(TypeCheckError::CannotPassOptionalTypeToFunction {
                            type_kind: checked_arg.type_kind,
                            loc: args[0].kind.get_loc(),
                        });
                    }
                    return Ok(CheckedExpression {
                        kind: CheckedExpressionKind::FunctionCall {
                            name: "print".to_owned(),
                            args: vec![checked_arg],
                        },
                        type_kind: TypeKind::Unit,
                        loc,
                    });
                }
                if name == "println" {
                    let checked_arg = self.type_check_expression(&args[0])?;
                    if let TypeKind::Optional(_base_type) = &checked_arg.type_kind {
                        return Err(TypeCheckError::CannotPassOptionalTypeToFunction {
                            type_kind: checked_arg.type_kind,
                            loc: args[0].kind.get_loc(),
                        });
                    }
                    return Ok(CheckedExpression {
                        kind: CheckedExpressionKind::FunctionCall {
                            name: "println".to_owned(),
                            args: vec![checked_arg],
                        },
                        type_kind: TypeKind::Unit,
                        loc,
                    });
                }
                //ENDTMP

                let checked_function = self
                    .scope
                    .try_get_function(name, &callee.kind.get_loc())?
                    .clone();

                let mut checked_args: Vec<CheckedExpression> = Vec::new();

                if args.len() != checked_function.args.len() {
                    return Err(TypeCheckError::ArgLengthMismatch {
                        expected: checked_function.args.len(),
                        actual: args.len(),
                        loc: span_locs(&open_paren.loc, &close_paren.loc),
                    });
                }

                for (i, arg) in args.iter().enumerate() {
                    self.scope
                        .assign_context
                        .push(checked_function.args.get(i).unwrap().clone());

                    let checked_arg = self.type_check_expression(arg)?;

                    self.scope.assign_context.pop();

                    self.expect_type(
                        checked_function.args.get(i).unwrap().clone(),
                        checked_arg.type_kind.clone(),
                        arg.kind.get_loc().clone(),
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
                array_type_expression_kind,
                open_curly,
                elements,
                close_curly,
            } => {
                let array_type = self.scope.try_get_type(array_type_expression_kind)?;

                match &array_type {
                    TypeKind::Array(size, element_type) => {
                        let mut checked_elements: Vec<CheckedExpression> = Vec::new();

                        self.scope.assign_context.push(*element_type.clone());

                        for element in elements {
                            let checked_element = self.type_check_expression(element)?;

                            self.expect_type(
                                *element_type.clone(),
                                checked_element.type_kind.clone(),
                                element.kind.get_loc().clone(),
                            )?;

                            checked_elements.push(checked_element);
                        }
                        self.scope.assign_context.pop();

                        if size != &elements.len() {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: array_type.clone(),
                                actual: TypeKind::Array(elements.len(), element_type.clone()),
                                loc: span_locs(&open_curly.loc, &close_curly.loc),
                            });
                        }

                        Ok(CheckedExpression {
                            kind: CheckedExpressionKind::ArrayLiteral {
                                elements: checked_elements,
                            },
                            type_kind: TypeKind::Array(elements.len(), element_type.to_owned()),
                            loc: expression.kind.get_loc(),
                        })
                    }
                    TypeKind::Slice(_element_type) => todo!("Slice literals!"),
                    _ => unreachable!(),
                }
            }
            ExpressionKind::ArrayIndex { array, index } => {
                let checked_array = self.type_check_expression(array)?;

                match &checked_array.type_kind.clone() {
                    TypeKind::Array(_, el_type) | TypeKind::Slice(el_type) => {
                        let checked_index = self.type_check_expression(index)?;

                        if !Self::is_integer_type(&checked_index.type_kind) {
                            return Err(TypeCheckError::TypeMismatch {
                                expected: TypeKind::U32,
                                actual: checked_index.type_kind,
                                loc: index.kind.get_loc().clone(),
                            });
                        }

                        return Ok(CheckedExpression {
                            kind: CheckedExpressionKind::ArrayIndex {
                                array: Box::new(checked_array),
                                index: Box::new(checked_index),
                            },
                            type_kind: *el_type.clone(),
                            loc: index.kind.get_loc().clone(),
                        });
                    }
                    _ => (),
                }
                Err(TypeCheckError::CannotIndexType {
                    type_kind: checked_array.type_kind,
                    loc: array.kind.get_loc().clone(),
                })
            }
            ExpressionKind::RecordLiteral {
                record_identifier,
                open_curly,
                args,
                close_curly,
            } => {
                let record_name = record_identifier.text.to_owned();
                let record_type = self.scope.try_get_type(&TypeExpressionKind::Basic {
                    identifier: record_identifier.clone(),
                })?;

                let (type_kind, checked_expression_kind) = match self
                    .scope
                    .assign_context
                    .clone()
                    .last()
                {
                    Some(ty) => {
                        if let TypeKind::Optional(base_type) = ty {
                            self.scope.assign_context.push(*base_type.clone());

                            let checked_expr = self.type_check_expression(expression)?;

                            self.scope.assign_context.pop();

                            (TypeKind::Optional(base_type.clone()), checked_expr.kind)
                        } else if let TypeKind::Record(name, _generic_params, members) =
                            &record_type
                        {
                            let outer_scope = self.scope.clone();
                            self.scope = Scope::new_inner_scope(outer_scope.clone());

                            let n_members = members.len();

                            //generic_params = T, U, V etc
                            let mut checked_args: Vec<CheckedExpression> = Vec::new();
                            for (i, member) in members.iter().enumerate() {
                                match args.get(i) {
                                    Some(arg) => {
                                        self.scope.assign_context.push(member.type_kind.clone());
                                        let checked_arg = self.type_check_expression(arg)?;

                                        //TODO: this needs to erase record_type's generics

                                        self.scope.assign_context.pop();

                                        self.expect_type(
                                            member.type_kind.clone(),
                                            checked_arg.type_kind.clone(),
                                            arg.kind.get_loc().clone(),
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

                            self.scope = outer_scope;

                            if args.len() > n_members {
                                let checked_arg = self.type_check_expression(&args[n_members])?;
                                return Err(TypeCheckError::UnexpectedArgForRecord {
                                    type_kind: checked_arg.type_kind,
                                    record_name: record_name.clone(),
                                    loc: args[n_members].kind.get_loc().clone(),
                                });
                            }
                            (
                                record_type,
                                CheckedExpressionKind::RecordLiteral {
                                    arguments: checked_args,
                                },
                            )
                        } else {
                            //TODO: maybe better error
                            return Err(TypeCheckError::NoSuchTypeDeclaredInScope {
                                name: record_name.clone(),
                                loc: open_curly.loc.clone(),
                            });
                        }
                    }
                    None => {
                        if let TypeKind::Record(name, _generic_params, members) = &record_type {
                            let outer_scope = self.scope.clone();
                            self.scope = Scope::new_inner_scope(outer_scope.clone());

                            let n_members = members.len();

                            //generic_params = T, U, V etc
                            let mut checked_args: Vec<CheckedExpression> = Vec::new();
                            for (i, member) in members.iter().enumerate() {
                                match args.get(i) {
                                    Some(arg) => {
                                        self.scope.assign_context.push(member.type_kind.clone());
                                        let checked_arg = self.type_check_expression(arg)?;

                                        //TODO: this needs to erase record_type's generics

                                        self.scope.assign_context.pop();

                                        self.expect_type(
                                            member.type_kind.clone(),
                                            checked_arg.type_kind.clone(),
                                            arg.kind.get_loc().clone(),
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

                            self.scope = outer_scope;

                            if args.len() > n_members {
                                let checked_arg = self.type_check_expression(&args[n_members])?;
                                return Err(TypeCheckError::UnexpectedArgForRecord {
                                    type_kind: checked_arg.type_kind,
                                    record_name: record_name.clone(),
                                    loc: args[n_members].kind.get_loc().clone(),
                                });
                            }
                            (
                                record_type,
                                CheckedExpressionKind::RecordLiteral {
                                    arguments: checked_args,
                                },
                            )
                        } else {
                            //TODO: maybe better error
                            return Err(TypeCheckError::NoSuchTypeDeclaredInScope {
                                name: record_name.clone(),
                                loc: open_curly.loc.clone(),
                            });
                        }
                    }
                };

                Ok(CheckedExpression {
                    kind: checked_expression_kind,
                    type_kind,
                    loc: open_curly.loc.clone(),
                })
            }
            ExpressionKind::Accessor {
                accessee,
                dot,
                member_identifier,
            } => {
                let checked_accessee = self.type_check_expression(accessee)?;
                if let TypeKind::Record(name, _generic_params, members) =
                    &checked_accessee.type_kind.clone()
                {
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
                    loc: accessee.kind.get_loc().clone(),
                })
            }
            ExpressionKind::SafeAccessor {
                accessee,
                question_dot,
                member_identifier,
            } => {
                let checked_accessee = self.type_check_expression(accessee)?;
                if let TypeKind::Optional(base_type) = &checked_accessee.type_kind.clone() {
                    if let TypeKind::Record(name, _generic_params, members) = *base_type.clone() {
                        let member_name = &member_identifier.text;

                        if let Some(member) = members.iter().find(|m| &m.name == member_name) {
                            return Ok(CheckedExpression {
                                kind: CheckedExpressionKind::SafeAccessor {
                                    accessee: Box::new(checked_accessee),
                                    member: member.name.to_string(),
                                },
                                type_kind: TypeKind::Optional(Box::new(member.type_kind.clone())),
                                loc: question_dot.loc.clone(),
                            });
                        } else {
                            return Err(TypeCheckError::NoSuchMember {
                                member_name: member_name.to_string(),
                                name: name.to_string(),
                                loc: member_identifier.loc.clone(),
                            });
                        }
                    }
                    return Err(TypeCheckError::CannotAccessType {
                        type_kind: checked_accessee.type_kind.clone(),
                        loc: accessee.kind.get_loc().clone(),
                    });
                }
                Err(TypeCheckError::TypeMismatch {
                    expected: TypeKind::Optional(Box::new(checked_accessee.type_kind.clone())),
                    actual: checked_accessee.type_kind.clone(),
                    loc: accessee.kind.get_loc().clone(),
                })
            }
            ExpressionKind::FunctionParameter { .. } => unreachable!(),
            ExpressionKind::Range {
                lower,
                dotdot: _,
                upper,
            } => {
                let checked_lower = self.type_check_expression(lower)?;

                if !Self::is_number_type(&checked_lower.type_kind) {
                    return Err(TypeCheckError::TypeMismatch {
                        expected: TypeKind::U32,
                        actual: checked_lower.type_kind,
                        loc: lower.kind.get_loc().clone(),
                    });
                }

                let checked_upper = self.type_check_expression(upper)?;

                self.expect_type(
                    checked_lower.type_kind.clone(),
                    checked_upper.type_kind.clone(),
                    upper.kind.get_loc().clone(),
                )?;

                Ok(CheckedExpression {
                    type_kind: TypeKind::Range(Box::new(checked_lower.type_kind.clone())),
                    kind: CheckedExpressionKind::Range {
                        lower: Box::new(checked_lower),
                        upper: Box::new(checked_upper),
                    },
                    loc: span_locs(&lower.kind.get_loc(), &upper.kind.get_loc()),
                })
            }
            ExpressionKind::StaticAccessor {
                namespace,
                colon_colon: _,
                member,
            } => match &member.kind {
                ExpressionKind::Variable { identifier } => {
                    //TODO: Completely overhaul this branch once we have global constants, e.g. math::PI
                    let checked_member = self.scope.try_get_module_member(
                        &namespace.text.clone(),
                        &identifier.text,
                        &member.kind.get_loc(),
                    )?;
                    Ok(CheckedExpression {
                        kind: CheckedExpressionKind::StaticAccessor {
                            name: namespace.text.clone(),
                            member: checked_member.clone(),
                        },
                        type_kind: checked_member.type_kind.clone(),
                        loc: member.kind.get_loc().clone(),
                    })
                }
                ExpressionKind::RecordLiteral {
                    record_identifier: _,
                    open_curly: _,
                    args: _,
                    close_curly: _,
                } => match self.module.imports.get(&namespace.text) {
                    Some(module) => {
                        todo!("{:?}", module)
                    }
                    None => Err(TypeCheckError::NoSuchNamespaceDeclaredInScope {
                        namespace: namespace.clone(),
                    }),
                },
                _ => todo!(
                    "static accessor not implemented for member type: {:?}",
                    member.kind
                ),
            },
            ExpressionKind::MatchCase {
                pattern: _,
                fat_arrow: _,
                result: _,
            } => todo!(),
            ExpressionKind::Nil { nil_keyword } => match self.scope.assign_context.last() {
                Some(type_kind) => match type_kind {
                    TypeKind::Optional(base_type) => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::Nil,
                        type_kind: TypeKind::Optional(Box::new(*base_type.clone())),
                        loc: nil_keyword.loc.clone(),
                    }),
                    _ => Err(TypeCheckError::NonNillableType(
                        type_kind.clone(),
                        nil_keyword.loc.clone(),
                    )),
                },
                None => Err(TypeCheckError::CannotDeduceTypeFromContext(
                    nil_keyword.loc.clone(),
                )),
            },
            ExpressionKind::ForceUnwrap {
                expression: inner_expression,
                bang: _,
            } => {
                let checked_inner = self.type_check_expression(inner_expression)?;
                match &checked_inner.type_kind {
                    TypeKind::Optional(base_type) => Ok(CheckedExpression {
                        kind: CheckedExpressionKind::ForceUnwrap {
                            expression: Box::new(checked_inner.clone()),
                        },
                        type_kind: *base_type.clone(),
                        loc: expression.kind.get_loc().clone(),
                    }),
                    _ => Err(TypeCheckError::NonNillableType(
                        checked_inner.type_kind.clone(),
                        expression.kind.get_loc().clone(),
                    )),
                }
            }
            ExpressionKind::NilCoalesce {
                optional,
                question_colon: _,
                default,
            } => {
                let checked_optional = self.type_check_expression(optional)?;
                if let TypeKind::Optional(base_type) = checked_optional.type_kind.clone() {
                    let checked_default = self.type_check_expression(default)?;
                    self.expect_type(
                        *base_type.clone(),
                        checked_default.type_kind.clone(),
                        default.kind.get_loc(),
                    )?;

                    return Ok(CheckedExpression {
                        kind: CheckedExpressionKind::NilCoalesce {
                            optional: Box::new(checked_optional),
                            default: Box::new(checked_default),
                        },
                        type_kind: *base_type,
                        loc: expression.kind.get_loc(),
                    });
                } else {
                    return Err(TypeCheckError::NonNillableType(
                        checked_optional.type_kind,
                        optional.kind.get_loc(),
                    ));
                }
            }
        };
        return match &checked_expression_result {
            Ok(checked_expression) => {
                if let Some(TypeKind::GenericParameter(t)) = self.scope.assign_context.last() {
                    match self.scope.try_get_generic_erasure(t) {
                        Some(concrete_type) => {
                            self.expect_type(
                                concrete_type.clone(),
                                checked_expression.type_kind.clone(),
                                checked_expression.loc.clone(),
                            )?;
                        }
                        None => {
                            self.scope
                                .generic_erasures
                                .insert(t.clone(), checked_expression.type_kind.clone());
                        }
                    }
                }
                Ok(checked_expression.clone())
            }
            Err(e) => Err(e.clone()),
        };
    }

    fn is_integer_type(kind: &TypeKind) -> bool {
        matches!(
            kind,
            TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::I8
                | TypeKind::Char
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
        )
    }

    fn is_number_type(kind: &TypeKind) -> bool {
        matches!(
            kind,
            TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::I8
                | TypeKind::Char
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::F32
                | TypeKind::F64
        )
    }

    fn check_unary_expression(
        op: &UnaryOp,
        operand: &TypeKind,
    ) -> Result<TypeKind, TypeCheckError> {
        match op.kind {
            parser::UnaryOpKind::Negation | parser::UnaryOpKind::Identity => {
                if Self::is_number_type(operand) {
                    return Ok(operand.clone()); //todo pick right width
                }
                Err(TypeCheckError::UnaryOpNotImplementedForType {
                    op: op.kind.clone(),
                    operand: operand.clone(),
                    loc: op.loc.clone(),
                })
            }
            parser::UnaryOpKind::Not => {
                if operand == &TypeKind::Bool {
                    return Ok(TypeKind::Bool);
                }
                Err(TypeCheckError::UnaryOpNotImplementedForType {
                    op: op.kind.clone(),
                    operand: operand.clone(),
                    loc: op.loc.clone(),
                })
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
                if Self::is_number_type(left) && Self::is_number_type(right) {
                    return Ok(left.clone()); //todo pick right width
                }
                Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                })
            }
            BinaryOpKind::Lt | BinaryOpKind::LtEq | BinaryOpKind::Gt | BinaryOpKind::GtEq => {
                if Self::is_number_type(left) && Self::is_number_type(right) {
                    return Ok(TypeKind::Bool);
                }
                Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                })
            }
            BinaryOpKind::And | BinaryOpKind::Or | BinaryOpKind::Xor => {
                if left == &TypeKind::Bool && right == &TypeKind::Bool {
                    return Ok(TypeKind::Bool);
                }
                Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                })
            }
            BinaryOpKind::Eq | BinaryOpKind::NEq => {
                if Self::is_number_type(left) && Self::is_number_type(right) {
                    return Ok(TypeKind::Bool);
                }
                if left == right {
                    return Ok(TypeKind::Bool);
                }
                Err(TypeCheckError::BinaryOpNotImplementedForTypes {
                    left: left.clone(),
                    op: op.kind.clone(),
                    right: right.clone(),
                    loc: op.loc.clone(),
                })
            }
        }
    }

    fn all_paths_return_value(statement: &CheckedStatement) -> bool {
        match &statement.kind {
            CheckedStatementKind::Block { statements } => {
                if statements.is_empty() {
                    return false;
                }
                for s in statements {
                    if Self::all_paths_return_value(s) {
                        return true;
                    }
                }
                //TODO warn about unreachable code
                false
            }
            CheckedStatementKind::If {
                condition: _,
                body,
                else_body,
            } => match else_body {
                Some(else_body) => {
                    Self::all_paths_return_value(body) && Self::all_paths_return_value(else_body)
                }
                None => false,
            },
            CheckedStatementKind::Return { .. } => true,
            CheckedStatementKind::Expression { .. }
            | CheckedStatementKind::VariableDeclaration { .. }
            | CheckedStatementKind::FunctionDeclaration { .. }
            | CheckedStatementKind::While { .. }
            | CheckedStatementKind::Record { .. }
            | CheckedStatementKind::Enum { .. }
            | CheckedStatementKind::Break
            | CheckedStatementKind::Continue
            | CheckedStatementKind::MatchCases { .. }
            | CheckedStatementKind::Match { .. }
            | CheckedStatementKind::NoOp => false,
            CheckedStatementKind::ForIn {
                iterator: _,
                iterable: _,
                body,
            } => Self::all_paths_return_value(body),
        }
    }

    fn is_assignable_from(&self, from: &TypeKind, to: &TypeKind) -> bool {
        if from == to {
            return true;
        }
        //TODO more complex coersion
        false
    }
}
