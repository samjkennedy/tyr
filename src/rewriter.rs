use crate::type_checker::{
    CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedStatementKind,
};

pub enum RewriterError {}

pub fn rewrite_statements(
    statements: Vec<CheckedStatement>,
) -> Result<Vec<CheckedStatement>, RewriterError> {
    let mut rewritten_statements: Vec<CheckedStatement> = Vec::new();

    for statement in statements {
        rewritten_statements.push(rewrite_statement(statement)?);
    }

    Ok(rewritten_statements)
}

fn rewrite_statement(statement: CheckedStatement) -> Result<CheckedStatement, RewriterError> {
    match statement.kind {
        CheckedStatementKind::Block { statements } => {
            let mut rewritten_statements: Vec<CheckedStatement> = Vec::new();

            for statement in statements {
                rewritten_statements.push(rewrite_statement(statement)?);
            }

            Ok(CheckedStatement {
                kind: CheckedStatementKind::Block {
                    statements: rewritten_statements,
                },
            })
        }
        CheckedStatementKind::Expression { expression } => Ok(CheckedStatement {
            kind: CheckedStatementKind::Expression {
                expression: rewrite_expression(expression)?,
            },
        }),
        CheckedStatementKind::VariableDeclaration {
            name,
            type_kind,
            initialiser,
        } => {
            let rewritten_initialiser = rewrite_expression(initialiser)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::VariableDeclaration {
                    name,
                    type_kind,
                    initialiser: rewritten_initialiser,
                },
            })
        }
        CheckedStatementKind::FunctionDeclaration {
            name,
            args,
            return_type,
            body,
        } => {
            let rewritten_body = rewrite_statement(*body)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::FunctionDeclaration {
                    name,
                    args,
                    return_type,
                    body: Box::new(rewritten_body),
                },
            })
        }
        CheckedStatementKind::If {
            condition,
            body,
            else_body,
        } => {
            let rewritten_body = rewrite_statement(*body)?;

            let rewritten_else_body = match else_body {
                Some(else_body) => Some(Box::new(rewrite_statement(*else_body)?)),
                None => None,
            };

            Ok(CheckedStatement {
                kind: CheckedStatementKind::If {
                    condition,
                    body: Box::new(rewritten_body),
                    else_body: rewritten_else_body,
                },
            })
        }
        CheckedStatementKind::While { condition, body } => {
            let rewritten_body = rewrite_statement(*body)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::While {
                    condition,
                    body: Box::new(rewritten_body),
                },
            })
        }
        CheckedStatementKind::Return { return_value } => {
            let rewritten_return_value = match return_value {
                Some(return_value) => Some(rewrite_expression(return_value)?),
                None => None,
            };
            Ok(CheckedStatement {
                kind: CheckedStatementKind::Return {
                    return_value: rewritten_return_value,
                },
            })
        }
        CheckedStatementKind::Record {
            name,
            generic_params,
            members,
        } => Ok(CheckedStatement {
            kind: CheckedStatementKind::Record {
                name,
                generic_params,
                members,
            },
        }),
        CheckedStatementKind::Break => Ok(CheckedStatement {
            kind: CheckedStatementKind::Break,
        }),
        CheckedStatementKind::Continue => Ok(CheckedStatement {
            kind: CheckedStatementKind::Continue,
        }),
        CheckedStatementKind::Enum { name, variants } => Ok(CheckedStatement {
            kind: CheckedStatementKind::Enum { name, variants },
        }),
        CheckedStatementKind::MatchCases { cases } => {
            let mut rewritten_cases = Vec::new();
            for case in cases {
                rewritten_cases.push(rewrite_expression(case)?);
            }
            Ok(CheckedStatement {
                kind: CheckedStatementKind::MatchCases {
                    cases: rewritten_cases,
                },
            })
        }
        CheckedStatementKind::Match { expression, cases } => {
            let rewritten_expression = rewrite_expression(expression)?;
            let rewritten_cases = rewrite_statement(*cases)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::Match {
                    expression: rewritten_expression,
                    cases: Box::new(rewritten_cases),
                },
            })
        }
    }
}

fn rewrite_expression(expression: CheckedExpression) -> Result<CheckedExpression, RewriterError> {
    return match expression.kind {
        CheckedExpressionKind::BoolLiteral { value: _ } => Ok(expression),
        CheckedExpressionKind::U8Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U16Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U32Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U64Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I8Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I16Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I32Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I64Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::F32Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::F64Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::StringLiteral { value: _ } => Ok(expression),
        CheckedExpressionKind::ArrayLiteral { elements } => {
            let mut rewritten_elements = Vec::new();

            for el in elements {
                rewritten_elements.push(rewrite_expression(el)?);
            }

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::ArrayLiteral {
                    elements: rewritten_elements,
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::RecordLiteral { arguments } => {
            let mut rewritten_arguments = Vec::new();

            for arg in arguments {
                rewritten_arguments.push(rewrite_expression(arg)?);
            }

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::RecordLiteral {
                    arguments: rewritten_arguments,
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Binary { left, op, right } => {
            let rewritten_left = rewrite_expression(*left)?;
            let rewritten_right = rewrite_expression(*right)?;

            //TODO: const analysis?

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Binary {
                    left: Box::new(rewritten_left),
                    op,
                    right: Box::new(rewritten_right),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Unary { op, operand } => {
            let rewritten_operand = rewrite_expression(*operand)?;

            //TODO: const analysis?

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Unary {
                    op,
                    operand: Box::new(rewritten_operand),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Parenthesised { expression: inner } => {
            let rewritten_expression = rewrite_expression(*inner)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Parenthesised {
                    expression: Box::new(rewritten_expression),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Variable { variable } => Ok(CheckedExpression {
            kind: CheckedExpressionKind::Variable { variable },
            type_kind: expression.type_kind,
            loc: expression.loc,
        }),
        CheckedExpressionKind::Assignment { lhs, rhs } => {
            let rewritten_lhs = rewrite_expression(*lhs)?;
            let rewritten_rhs = rewrite_expression(*rhs)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Assignment {
                    lhs: Box::new(rewritten_lhs),
                    rhs: Box::new(rewritten_rhs),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::FunctionCall { name, args } => {
            let mut rewritten_arguments = Vec::new();

            for arg in args {
                rewritten_arguments.push(rewrite_expression(arg)?);
            }

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::FunctionCall {
                    name,
                    args: rewritten_arguments,
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::ArrayIndex { array, index } => {
            let rewritten_array = rewrite_expression(*array)?;
            let rewritten_index = rewrite_expression(*index)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::ArrayIndex {
                    array: Box::new(rewritten_array),
                    index: Box::new(rewritten_index),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Accessor { accessee, member } => {
            let rewritten_accessee = rewrite_expression(*accessee)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Accessor {
                    accessee: Box::new(rewritten_accessee),
                    member,
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::SafeAccessor { accessee, member } => {
            let rewritten_accessee = rewrite_expression(*accessee)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::SafeAccessor {
                    accessee: Box::new(rewritten_accessee),
                    member,
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::StaticAccessor { name, member } => Ok(CheckedExpression {
            kind: CheckedExpressionKind::StaticAccessor { name, member },
            type_kind: expression.type_kind,
            loc: expression.loc,
        }),
        CheckedExpressionKind::MatchCase { pattern, result } => {
            let rewritten_pattern = rewrite_expression(*pattern)?;
            let rewritten_result = rewrite_expression(*result)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::MatchCase {
                    pattern: Box::new(rewritten_pattern),
                    result: Box::new(rewritten_result),
                },
                type_kind: expression.type_kind,
                loc: expression.loc,
            })
        }
        CheckedExpressionKind::Nil => Ok(expression),
        CheckedExpressionKind::ForceUnwrap { expression } => Ok(CheckedExpression {
            kind: CheckedExpressionKind::ForceUnwrap {
                expression: Box::new(rewrite_expression(*expression.clone())?),
            },
            type_kind: expression.type_kind.clone(),
            loc: expression.loc.clone(),
        }),
        CheckedExpressionKind::NilCoalesce { optional, default } => {
            let rewritten_optional = rewrite_expression(*optional)?;
            let rewritten_default = rewrite_expression(*default)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::NilCoalesce {
                    optional: Box::new(rewritten_optional),
                    default: Box::new(rewritten_default),
                },
                type_kind: expression.type_kind.clone(),
                loc: expression.loc.clone(),
            })
        }
    };
}
