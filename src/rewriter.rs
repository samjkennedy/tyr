use crate::{
    lexer::Loc,
    parser::BinaryOpKind,
    type_checker::{
        CheckedExpression, CheckedExpressionKind, CheckedStatement, CheckedStatementKind,
        CheckedVariable, TypeKind,
    },
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
        CheckedStatementKind::ValueDeclaration {
            name,
            mutable,
            type_kind,
            initialiser,
        } => {
            let rewritten_initialiser = rewrite_expression(initialiser)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::ValueDeclaration {
                    name,
                    mutable,
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
        CheckedStatementKind::MatchArms { arms: cases } => {
            let mut rewritten_cases = Vec::new();
            for case in cases {
                rewritten_cases.push(rewrite_expression(case)?);
            }
            Ok(CheckedStatement {
                kind: CheckedStatementKind::MatchArms {
                    arms: rewritten_cases,
                },
            })
        }
        CheckedStatementKind::Match {
            expression,
            match_arms: cases,
        } => {
            let rewritten_expression = rewrite_expression(expression)?;
            let rewritten_cases = rewrite_statement(*cases)?;

            Ok(CheckedStatement {
                kind: CheckedStatementKind::Match {
                    expression: rewritten_expression,
                    match_arms: Box::new(rewritten_cases),
                },
            })
        }
        CheckedStatementKind::NoOp => Ok(CheckedStatement {
            kind: CheckedStatementKind::NoOp,
        }),
        CheckedStatementKind::ForIn {
            iterator,
            iterable,
            body,
        } => {
            let mut statements: Vec<CheckedStatement> = Vec::new();

            match &iterable.type_kind {
                TypeKind::Range(inner_type) => {
                    if let CheckedExpressionKind::Range { lower, upper } = iterable.kind {
                        //Need to rewrite this into a while loop...
                        /*
                           for x in 0..10 { body }

                           becomes

                           var x = 0;
                           while x < 10 {
                               body;
                               x = x + 1;
                           }
                        */

                        //var x = 0;
                        let declare_iterator = CheckedStatement {
                            kind: CheckedStatementKind::ValueDeclaration {
                                name: iterator.name.clone(),
                                mutable: true,
                                type_kind: *inner_type.clone(),
                                initialiser: *lower,
                            },
                        };
                        statements.push(declare_iterator);

                        /*
                        {
                            body;
                            x = x + 1;
                        }
                        */
                        let mut rewritten_body: Vec<CheckedStatement> = Vec::new();
                        rewritten_body.push(*body);
                        let increment = CheckedExpression {
                            kind: CheckedExpressionKind::Binary {
                                left: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::Variable {
                                        variable: iterator.clone(),
                                    },
                                    type_kind: iterator.type_kind.clone(),
                                    loc: Loc::null(),
                                }),
                                op: BinaryOpKind::Add,
                                right: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::I32Literal { value: 1 },
                                    type_kind: iterator.type_kind.clone(),
                                    loc: Loc::null(),
                                }),
                            },
                            type_kind: iterator.type_kind.clone(),
                            loc: Loc::null(),
                        };
                        let assignment = CheckedExpression {
                            kind: CheckedExpressionKind::Assignment {
                                lhs: Box::new(CheckedExpression {
                                    kind: CheckedExpressionKind::Variable {
                                        variable: iterator.clone(),
                                    },
                                    type_kind: iterator.type_kind.clone(),
                                    loc: Loc::null(),
                                }),
                                rhs: Box::new(increment),
                            },
                            type_kind: TypeKind::Unit,
                            loc: Loc::null(),
                        };
                        rewritten_body.push(CheckedStatement {
                            kind: CheckedStatementKind::Expression {
                                expression: assignment,
                            },
                        });

                        //while x < 10
                        let while_loop = CheckedStatement {
                            kind: CheckedStatementKind::While {
                                condition: CheckedExpression {
                                    kind: CheckedExpressionKind::Binary {
                                        left: Box::new(CheckedExpression {
                                            kind: CheckedExpressionKind::Variable {
                                                variable: iterator.clone(),
                                            },
                                            type_kind: iterator.type_kind.clone(),
                                            loc: Loc::null(),
                                        }),
                                        op: BinaryOpKind::Lt,
                                        right: upper,
                                    },
                                    type_kind: TypeKind::Bool,
                                    loc: Loc::null(),
                                },
                                body: Box::new(CheckedStatement {
                                    kind: CheckedStatementKind::Block {
                                        statements: rewritten_body,
                                    },
                                }),
                            },
                        };
                        statements.push(while_loop);

                        self::rewrite_statement(CheckedStatement {
                            kind: CheckedStatementKind::Block { statements },
                        })
                    } else {
                        unreachable!()
                    }
                }
                TypeKind::Array(size, inner_type) => {
                    //Need to rewrite this into a while loop...
                    /*
                       for x in array { body }

                       becomes

                       var it = 0;
                       while it < arraysize {
                           var x = array[it];
                           body;
                           it = it + 1;
                       }
                    */

                    //var it = 0;
                    let it_variable = CheckedVariable {
                        name: "it".to_string(),
                        mutable: false,
                        type_kind: TypeKind::I32,
                        declaration_loc: Loc::null(),
                    };
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: "it".to_string(),
                            mutable: true,
                            type_kind: TypeKind::I32,
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 0 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            },
                        },
                    };
                    statements.push(declare_iterator);

                    /*
                    {
                        var x = array[it];
                        body;
                        it = it + 1;
                    }
                    */
                    let mut rewritten_body: Vec<CheckedStatement> = Vec::new();

                    //var x = array[it];
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: iterator.name.clone(),
                            mutable: false, //TODO: is this a good idea?
                            type_kind: *inner_type.clone(),
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::ArrayIndex {
                                    array: Box::new(iterable.clone()),
                                    index: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: it_variable.type_kind.clone(),
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: *inner_type.clone(),
                                loc: Loc::null(),
                            },
                        },
                    };
                    rewritten_body.push(declare_iterator);

                    //body;
                    rewritten_body.push(*body);

                    //it = it + 1;
                    let increment = CheckedExpression {
                        kind: CheckedExpressionKind::Binary {
                            left: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: it_variable.type_kind.clone(),
                                loc: Loc::null(),
                            }),
                            op: BinaryOpKind::Add,
                            right: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 1 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                        },
                        type_kind: TypeKind::I32,
                        loc: Loc::null(),
                    };
                    let assignment = CheckedExpression {
                        kind: CheckedExpressionKind::Assignment {
                            lhs: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                            rhs: Box::new(increment),
                        },
                        type_kind: TypeKind::Unit,
                        loc: Loc::null(),
                    };
                    rewritten_body.push(CheckedStatement {
                        kind: CheckedStatementKind::Expression {
                            expression: assignment,
                        },
                    });

                    //while it < arraysize
                    let while_loop = CheckedStatement {
                        kind: CheckedStatementKind::While {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Binary {
                                    left: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                    op: BinaryOpKind::Lt,
                                    right: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::I32Literal {
                                            value: *size as i32,
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: TypeKind::Bool,
                                loc: Loc::null(),
                            },
                            body: Box::new(CheckedStatement {
                                kind: CheckedStatementKind::Block {
                                    statements: rewritten_body,
                                },
                            }),
                        },
                    };
                    statements.push(while_loop);

                    self::rewrite_statement(CheckedStatement {
                        kind: CheckedStatementKind::Block { statements },
                    })
                }
                TypeKind::DynamicArray(inner_type) => {
                    //Need to rewrite this into a while loop...
                    /*
                       for x in array { body }

                       becomes

                       var it = 0;
                       while it < arraysize {
                           var x = array[it];
                           body;
                           it = it + 1;
                       }
                    */

                    //var it = 0;
                    let it_variable = CheckedVariable {
                        name: "it".to_string(),
                        mutable: false,
                        type_kind: TypeKind::I32,
                        declaration_loc: Loc::null(),
                    };
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: "it".to_string(),
                            mutable: true,
                            type_kind: TypeKind::I32,
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 0 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            },
                        },
                    };
                    statements.push(declare_iterator);

                    /*
                    {
                        var x = array[it];
                        body;
                        it = it + 1;
                    }
                    */
                    let mut rewritten_body: Vec<CheckedStatement> = Vec::new();

                    //var x = array[it];
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: iterator.name.clone(),
                            mutable: false, //TODO: same here
                            type_kind: *inner_type.clone(),
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::ArrayIndex {
                                    array: Box::new(iterable.clone()),
                                    index: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: it_variable.type_kind.clone(),
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: *inner_type.clone(),
                                loc: Loc::null(),
                            },
                        },
                    };
                    rewritten_body.push(declare_iterator);

                    //body;
                    rewritten_body.push(*body);

                    //it = it + 1;
                    let increment = CheckedExpression {
                        kind: CheckedExpressionKind::Binary {
                            left: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: it_variable.type_kind.clone(),
                                loc: Loc::null(),
                            }),
                            op: BinaryOpKind::Add,
                            right: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 1 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                        },
                        type_kind: TypeKind::I32,
                        loc: Loc::null(),
                    };
                    let assignment = CheckedExpression {
                        kind: CheckedExpressionKind::Assignment {
                            lhs: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                            rhs: Box::new(increment),
                        },
                        type_kind: TypeKind::Unit,
                        loc: Loc::null(),
                    };
                    rewritten_body.push(CheckedStatement {
                        kind: CheckedStatementKind::Expression {
                            expression: assignment,
                        },
                    });

                    //while it < arraysize
                    let while_loop = CheckedStatement {
                        kind: CheckedStatementKind::While {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Binary {
                                    left: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                    op: BinaryOpKind::Lt,
                                    right: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Accessor {
                                            accessee: Box::new(iterable.clone()),
                                            member: "count".to_string(),
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: TypeKind::Bool,
                                loc: Loc::null(),
                            },
                            body: Box::new(CheckedStatement {
                                kind: CheckedStatementKind::Block {
                                    statements: rewritten_body,
                                },
                            }),
                        },
                    };
                    statements.push(while_loop);

                    self::rewrite_statement(CheckedStatement {
                        kind: CheckedStatementKind::Block { statements },
                    })
                }
                TypeKind::Slice(inner_type) => {
                    //Need to rewrite this into a while loop...
                    /*
                       for x in slice { body }

                       becomes

                       var it = 0;
                       while it < slice.count {
                           var x = slice[it + slice.offset];
                           body;
                           it = it + 1;
                       }
                    */

                    //var it = 0;
                    let it_variable = CheckedVariable {
                        name: "it".to_string(),
                        mutable: false,
                        type_kind: TypeKind::I32,
                        declaration_loc: Loc::null(),
                    };
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: "it".to_string(),
                            mutable: true,
                            type_kind: TypeKind::I32,
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 0 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            },
                        },
                    };
                    statements.push(declare_iterator);

                    /*
                    {
                        var x = array[it];
                        body;
                        it = it + 1;
                    }
                    */
                    let mut rewritten_body: Vec<CheckedStatement> = Vec::new();

                    //var x = array[it];
                    let declare_iterator = CheckedStatement {
                        kind: CheckedStatementKind::ValueDeclaration {
                            name: iterator.name.clone(),
                            mutable: false,
                            type_kind: *inner_type.clone(),
                            initialiser: CheckedExpression {
                                kind: CheckedExpressionKind::ArrayIndex {
                                    array: Box::new(iterable.clone()),
                                    index: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: it_variable.type_kind.clone(),
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: *inner_type.clone(),
                                loc: Loc::null(),
                            },
                        },
                    };
                    rewritten_body.push(declare_iterator);

                    //body;
                    rewritten_body.push(*body);

                    //it = it + 1;
                    let increment = CheckedExpression {
                        kind: CheckedExpressionKind::Binary {
                            left: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: it_variable.type_kind.clone(),
                                loc: Loc::null(),
                            }),
                            op: BinaryOpKind::Add,
                            right: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::I32Literal { value: 1 },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                        },
                        type_kind: TypeKind::I32,
                        loc: Loc::null(),
                    };
                    let assignment = CheckedExpression {
                        kind: CheckedExpressionKind::Assignment {
                            lhs: Box::new(CheckedExpression {
                                kind: CheckedExpressionKind::Variable {
                                    variable: it_variable.clone(),
                                },
                                type_kind: TypeKind::I32,
                                loc: Loc::null(),
                            }),
                            rhs: Box::new(increment),
                        },
                        type_kind: TypeKind::Unit,
                        loc: Loc::null(),
                    };
                    rewritten_body.push(CheckedStatement {
                        kind: CheckedStatementKind::Expression {
                            expression: assignment,
                        },
                    });

                    //while it < arraysize
                    let while_loop = CheckedStatement {
                        kind: CheckedStatementKind::While {
                            condition: CheckedExpression {
                                kind: CheckedExpressionKind::Binary {
                                    left: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Variable {
                                            variable: it_variable.clone(),
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                    op: BinaryOpKind::Lt,
                                    right: Box::new(CheckedExpression {
                                        kind: CheckedExpressionKind::Accessor {
                                            accessee: Box::new(iterable.clone()),
                                            member: "count".to_string(),
                                        },
                                        type_kind: TypeKind::I32,
                                        loc: Loc::null(),
                                    }),
                                },
                                type_kind: TypeKind::Bool,
                                loc: Loc::null(),
                            },
                            body: Box::new(CheckedStatement {
                                kind: CheckedStatementKind::Block {
                                    statements: rewritten_body,
                                },
                            }),
                        },
                    };
                    statements.push(while_loop);

                    self::rewrite_statement(CheckedStatement {
                        kind: CheckedStatementKind::Block { statements },
                    })
                }
                _ => todo!("other iterable types"),
            }
        }
    }
}

fn rewrite_expression(expression: CheckedExpression) -> Result<CheckedExpression, RewriterError> {
    match expression.kind {
        CheckedExpressionKind::BoolLiteral { value: _ } => Ok(expression),
        CheckedExpressionKind::U8Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U16Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U32Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::U64Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I8Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I16Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I32Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::I64Literal { value: _ } => Ok(expression),
        CheckedExpressionKind::CharLiteral { value: _ } => Ok(expression),
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
        CheckedExpressionKind::SwitchCase { pattern, result } => {
            let rewritten_result = rewrite_expression(*result)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::SwitchCase {
                    pattern,
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
        CheckedExpressionKind::Range { lower, upper } => {
            let rewritten_lower = rewrite_expression(*lower)?;
            let rewritten_upper = rewrite_expression(*upper)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Range {
                    lower: Box::new(rewritten_lower),
                    upper: Box::new(rewritten_upper),
                },
                type_kind: expression.type_kind.clone(),
                loc: expression.loc.clone(),
            })
        }
        CheckedExpressionKind::DefaultArrayInitializer { value } => {
            let rewritten_value = rewrite_expression(*value)?;

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::DefaultArrayInitializer {
                    value: Box::new(rewritten_value),
                },
                type_kind: expression.type_kind.clone(),
                loc: expression.loc.clone(),
            })
        }
        CheckedExpressionKind::Cast {
            expression: inner_expression,
            type_kind,
        } => {
            let rewritten_expression = rewrite_expression(*inner_expression)?;
            Ok(CheckedExpression {
                kind: CheckedExpressionKind::Cast {
                    expression: Box::new(rewritten_expression),
                    type_kind: type_kind.clone(),
                },
                type_kind: expression.type_kind.clone(),
                loc: expression.loc.clone(),
            })
        }
        CheckedExpressionKind::TaggedUnionLiteral {
            tag,
            union_name,
            variant_name,
            args,
        } => {
            let mut rewritten_args = Vec::new();

            for arg in args {
                rewritten_args.push(rewrite_expression(arg)?);
            }

            Ok(CheckedExpression {
                kind: CheckedExpressionKind::TaggedUnionLiteral {
                    tag,
                    union_name,
                    variant_name,
                    args: rewritten_args,
                },
                type_kind: expression.type_kind.clone(),
                loc: expression.loc.clone(),
            })
        }
    }
}
