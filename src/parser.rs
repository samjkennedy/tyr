use crate::lexer::{Loc, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Block {
        statements: Vec<Statement>,
    },
    VariableDeclaration {
        var_keyword: Token,
        identifier: Token,
        type_annotation: Option<Expression>,
        equals: Token,
        initialiser: Expression,
        semicolon: Token,
    },
    Expression {
        expression: Expression,
        semicolon: Token,
    },
    FunctionDeclaration {
        function_keyword: Token,
        identifier: Token,
        open_paren: Token,
        args: Vec<Expression>,
        close_paren: Token,
        return_annotation: Option<Expression>,
        body: Box<Statement>,
    },
    If {
        if_keyword: Token,
        condition: Expression,
        body: Box<Statement>,
        else_body: Option<Box<Statement>>,
    },
    While {
        while_keyword: Token,
        condition: Expression,
        body: Box<Statement>,
    },
    Return {
        return_keyword: Token,
        return_value: Option<Expression>,
    },
    Record {
        record_keyword: Token,
        identifier: Token,
        members: Vec<(Token, Expression)>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    BoolLiteral {
        token: Token,
    },
    IntLiteral {
        token: Token,
    },
    RealLiteral {
        token: Token,
    },
    StringLiteral {
        token: Token,
    },
    ArrayLiteral {
        open_square: Token,
        elements: Vec<Expression>,
        close_square: Token,
    },
    Assignment {
        lhs: Box<Expression>,
        equals: Token,
        rhs: Box<Expression>,
    },
    FunctionParameter {
        modifiers: Vec<Token>,
        identifier: Token,
        type_annotation: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Parenthesised {
        expression: Box<Expression>,
    },
    Variable {
        identifier: Token,
    },
    TypeAnnotation {
        colon: Token,
        identifier: Token, //TODO This will get more complex, e.g. arrays, generics
    },
    FunctionCall {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    RecordLiteral {
        record_name: String,
        open_curly: Token,
        args: Vec<Expression>,
        close_curly: Token,
    },
    Accessor {
        accessee: Box<Expression>,
        dot: Token,
        member_identifier: Token,
    },
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    NEq,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    TokenMismatch { expected: TokenKind, actual: Token },
    ExpectedSemicolon(Loc),
    UnexpectedEOF,
}
#[derive(Debug, Clone)]
pub struct Parser {
    allow_record_literals: bool, //This might be a bit hacky but disambiguates nicely for now
}

impl Parser {
    pub fn new() -> Parser {
        return Parser {
            allow_record_literals: false,
        };
    }

    pub fn parse_statements(&mut self, tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
        //println!("{:#?}", tokens);
        let mut statements: Vec<Statement> = Vec::new();

        let mut token_iter = tokens.iter().peekable();

        while let Some(_) = token_iter.peek() {
            let statement = self.parse_statement(&mut token_iter)?;

            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_statement(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Statement, ParseError> {
        let current_token = match tokens.peek() {
            Some(token) => token,
            None => return Err(ParseError::UnexpectedEOF),
        };
        match current_token.kind {
            TokenKind::VarKeyword => {
                let var_keyword = Self::expect_token(tokens, TokenKind::VarKeyword)?;
                let loc = var_keyword.loc.clone();

                let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;

                let type_annotation: Option<Expression> = match tokens.peek() {
                    Some(token) if token.kind == TokenKind::Colon => {
                        Some(Self::parse_type_annotation(tokens)?)
                    }
                    Some(_) => None,
                    None => return Err(ParseError::UnexpectedEOF),
                };

                let equals = Self::expect_token(tokens, TokenKind::Equals)?;
                if let Some(_) = tokens.peek() {
                    self.allow_record_literals = true;
                    let initialiser = self.parse_binary_expression(tokens, 0)?;
                    self.allow_record_literals = false;
                    let semicolon = Self::expect_token(tokens, TokenKind::Semicolon)?;

                    return Ok(Statement {
                        kind: StatementKind::VariableDeclaration {
                            var_keyword: var_keyword.clone(),
                            identifier,
                            type_annotation,
                            equals,
                            initialiser,
                            semicolon,
                        },
                        loc,
                    });
                } else {
                    Err(ParseError::UnexpectedEOF)
                }
            }
            TokenKind::FunctionKeyword => {
                let function_keyword = Self::expect_token(tokens, TokenKind::FunctionKeyword)?;
                let loc = function_keyword.loc.clone();

                let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                let open_paren = Self::expect_token(tokens, TokenKind::OpenParen)?;

                let mut args: Vec<Expression> = Vec::new();
                while tokens.peek().is_some()
                    && tokens.peek().unwrap().kind != TokenKind::CloseParen
                {
                    let mut modifiers = Vec::new();
                    while tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::Identifier
                    {
                        let modifier = tokens.next().unwrap();
                        match modifier.kind {
                            TokenKind::WithKeyword => modifiers.push(modifier.clone()),
                            _ => return Err(ParseError::UnexpectedToken(modifier.clone())),
                        }
                    }

                    let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                    let loc = identifier.loc.clone();
                    let type_annotation = Self::parse_type_annotation(tokens)?;

                    args.push(Expression {
                        kind: ExpressionKind::FunctionParameter {
                            modifiers,
                            identifier,
                            type_annotation: Box::new(type_annotation),
                        },
                        loc,
                    });

                    if tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseParen
                    {
                        Self::expect_token(tokens, TokenKind::Comma)?;
                    }
                }

                let close_paren = Self::expect_token(tokens, TokenKind::CloseParen)?;

                let return_annotation: Option<Expression> = match tokens.peek() {
                    Some(token) if token.kind == TokenKind::Colon => {
                        Some(Self::parse_type_annotation(tokens)?)
                    }
                    Some(_) => None,
                    None => return Err(ParseError::UnexpectedEOF),
                };
                let body = self.parse_block_statement(tokens)?;

                Ok(Statement {
                    kind: StatementKind::FunctionDeclaration {
                        function_keyword: function_keyword.clone(),
                        identifier,
                        open_paren,
                        args,
                        close_paren,
                        return_annotation,
                        body: Box::new(body),
                    },
                    loc,
                })
            }
            TokenKind::OpenCurly => self.parse_block_statement(tokens),
            TokenKind::IfKeyword => {
                let if_keyword = Self::expect_token(tokens, TokenKind::IfKeyword)?;
                let loc = if_keyword.loc.clone();
                self.allow_record_literals = false;
                let condition = self.parse_binary_expression(tokens, 0)?;
                let body = self.parse_statement(tokens)?;

                let else_body: Option<Statement> = match tokens.peek() {
                    Some(token) => match token.kind {
                        TokenKind::ElseKeyword => {
                            Self::expect_token(tokens, TokenKind::ElseKeyword)?;

                            Some(self.parse_statement(tokens)?)
                        }
                        _ => None,
                    },
                    None => None,
                };

                Ok(Statement {
                    kind: StatementKind::If {
                        if_keyword,
                        condition,
                        body: Box::new(body),
                        else_body: else_body.map(|e| Box::new(e)),
                    },
                    loc,
                })
            }
            TokenKind::WhileKeyword => {
                let while_keyword = Self::expect_token(tokens, TokenKind::WhileKeyword)?;
                let loc = while_keyword.loc.clone();
                let condition = self.parse_binary_expression(tokens, 0)?;

                self.allow_record_literals = false;
                let body = self.parse_statement(tokens)?;

                Ok(Statement {
                    kind: StatementKind::While {
                        while_keyword,
                        condition,
                        body: Box::new(body),
                    },
                    loc,
                })
            }
            TokenKind::ReturnKeyword => {
                let return_keyword = Self::expect_token(tokens, TokenKind::ReturnKeyword)?;
                let loc = return_keyword.loc.clone();

                self.allow_record_literals = true;
                let return_value: Option<Expression> = match tokens.peek() {
                    Some(token) => match token.kind {
                        TokenKind::Semicolon => None,
                        _ => Some(self.parse_binary_expression(tokens, 0)?),
                    },
                    None => return Err(ParseError::UnexpectedEOF),
                };
                self.allow_record_literals = false;

                Self::expect_token(tokens, TokenKind::Semicolon)?;

                Ok(Statement {
                    kind: StatementKind::Return {
                        return_keyword,
                        return_value,
                    },
                    loc,
                })
            }
            TokenKind::RecordKeyword => {
                let record_keyword = Self::expect_token(tokens, TokenKind::RecordKeyword)?;
                let loc = record_keyword.loc.clone();

                let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                Self::expect_token(tokens, TokenKind::OpenCurly)?;

                let mut members: Vec<(Token, Expression)> = Vec::new();
                while tokens.peek().is_some()
                    && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                {
                    let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                    let type_annotation = Self::parse_type_annotation(tokens)?;

                    members.push((identifier, type_annotation));

                    if tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                    {
                        Self::expect_token(tokens, TokenKind::Comma)?;
                    }
                }

                Self::expect_token(tokens, TokenKind::CloseCurly)?;

                Ok(Statement {
                    kind: StatementKind::Record {
                        record_keyword,
                        identifier,
                        members,
                    },
                    loc,
                })
            }
            TokenKind::BreakKeyword => {
                let break_keyword = Self::expect_token(tokens, TokenKind::BreakKeyword)?;
                let loc = break_keyword.loc.clone();
                Self::expect_token(tokens, TokenKind::Semicolon)?;
                Ok(Statement {
                    kind: StatementKind::Break,
                    loc,
                })
            }
            TokenKind::ContinueKeyword => {
                let continue_keyword = Self::expect_token(tokens, TokenKind::ContinueKeyword)?;
                let loc = continue_keyword.loc.clone();
                Self::expect_token(tokens, TokenKind::Semicolon)?;
                Ok(Statement {
                    kind: StatementKind::Continue,
                    loc,
                })
            }
            _ => {
                let expression = self.parse_binary_expression(tokens, 0)?;
                let loc = expression.loc.clone();

                let semicolon = Self::expect_token(tokens, TokenKind::Semicolon)?;

                Ok(Statement {
                    kind: StatementKind::Expression {
                        expression,
                        semicolon,
                    },
                    loc,
                })
            }
        }
    }

    fn parse_block_statement(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Statement, ParseError> {
        let open_curly = Self::expect_token(tokens, TokenKind::OpenCurly)?;
        let open_curly_loc = open_curly.loc.clone();
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(current_token) = tokens.peek() {
            match current_token.kind {
                TokenKind::CloseCurly => break,
                _ => statements.push(self.parse_statement(tokens)?),
            }
        }
        Self::expect_token(tokens, TokenKind::CloseCurly)?;

        Ok(Statement {
            kind: StatementKind::Block { statements },
            loc: open_curly_loc,
        })
    }

    fn parse_type_annotation(
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let colon = Self::expect_token(tokens, TokenKind::Colon)?;
        let type_identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
        Ok(Expression {
            kind: ExpressionKind::TypeAnnotation {
                colon: colon.clone(),
                identifier: type_identifier,
            },
            loc: colon.loc.clone(),
        })
    }

    fn get_binary_operator_precedence(kind: &TokenKind) -> usize {
        return match kind {
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 10,
            TokenKind::Plus | TokenKind::Minus => 9,
            TokenKind::CloseAngle
            | TokenKind::CloseAngleEquals
            | TokenKind::OpenAngle
            | TokenKind::OpenAngleEquals => 8,
            TokenKind::EqualsEquals => 7,
            TokenKind::AndKeyword => 6,
            TokenKind::XorKeyword => 5,
            TokenKind::OrKeyword => 4,
            _ => 0,
        };
    }

    fn parse_binary_op(token: Token) -> Result<BinaryOp, ParseError> {
        return match token.kind {
            TokenKind::Plus => Ok(BinaryOp {
                kind: BinaryOpKind::Add,
                loc: token.loc,
            }),
            TokenKind::Minus => Ok(BinaryOp {
                kind: BinaryOpKind::Sub,
                loc: token.loc,
            }),
            TokenKind::Star => Ok(BinaryOp {
                kind: BinaryOpKind::Mul,
                loc: token.loc,
            }),
            TokenKind::Slash => Ok(BinaryOp {
                kind: BinaryOpKind::Div,
                loc: token.loc,
            }),
            TokenKind::Percent => Ok(BinaryOp {
                kind: BinaryOpKind::Rem,
                loc: token.loc,
            }),
            TokenKind::AndKeyword => Ok(BinaryOp {
                kind: BinaryOpKind::And,
                loc: token.loc,
            }),
            TokenKind::OrKeyword => Ok(BinaryOp {
                kind: BinaryOpKind::Or,
                loc: token.loc,
            }),
            TokenKind::XorKeyword => Ok(BinaryOp {
                kind: BinaryOpKind::Xor,
                loc: token.loc,
            }),
            TokenKind::EqualsEquals => Ok(BinaryOp {
                kind: BinaryOpKind::Eq,
                loc: token.loc,
            }),
            TokenKind::OpenAngle => Ok(BinaryOp {
                kind: BinaryOpKind::Lt,
                loc: token.loc,
            }),
            TokenKind::OpenAngleEquals => Ok(BinaryOp {
                kind: BinaryOpKind::LtEq,
                loc: token.loc,
            }),
            TokenKind::CloseAngle => Ok(BinaryOp {
                kind: BinaryOpKind::Gt,
                loc: token.loc,
            }),
            TokenKind::CloseAngleEquals => Ok(BinaryOp {
                kind: BinaryOpKind::GtEq,
                loc: token.loc,
            }),
            TokenKind::BangEquals => Ok(BinaryOp {
                kind: BinaryOpKind::NEq,
                loc: token.loc,
            }),
            _ => Err(ParseError::UnexpectedToken(token)),
        };
    }

    fn parse_binary_expression(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
        parent_precedence: usize,
    ) -> Result<Expression, ParseError> {
        let mut left = self.parse_expression(tokens)?;

        let loc = left.loc.clone();

        while let Some(token) = tokens.peek() {
            let precedence = Self::get_binary_operator_precedence(&token.kind);

            if precedence == 0 || precedence <= parent_precedence {
                match token.kind {
                    TokenKind::Equals => {
                        let equals = tokens.next().expect("should never fail");
                        left = self.parse_assignment(left, tokens, equals)?
                    }
                    TokenKind::OpenParen => left = self.parse_function_call(left, tokens)?,
                    TokenKind::OpenSquare => left = self.parse_array_index(left, tokens)?,
                    TokenKind::OpenCurly => match left.kind {
                        ExpressionKind::Variable { .. } if self.allow_record_literals => {
                            left = self.parse_record_literal(left, tokens)?
                        } //TODO: This gets toom eager in things like while x < `n {}`
                        _ => return Ok(left),
                    },
                    TokenKind::Dot => left = Self::parse_accessor(left, tokens)?,
                    _ => return Ok(left),
                };
                continue;
            }

            let token = tokens.next().expect("should never fail");
            let binary_op = Self::parse_binary_op(token.clone())?;

            if let Some(_) = tokens.peek() {
                let right = self.parse_binary_expression(tokens, precedence)?;
                left = Expression {
                    kind: ExpressionKind::Binary {
                        left: Box::new(left),
                        op: binary_op,
                        right: Box::new(right),
                    },
                    loc: loc.clone(),
                };
            }
        }

        Ok(left)
    }

    fn parse_accessor(
        accessee: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let dot = Self::expect_token(tokens, TokenKind::Dot)?;
        let loc = dot.loc.clone();
        let member = Self::expect_token(tokens, TokenKind::Identifier)?;

        Ok(Expression {
            kind: ExpressionKind::Accessor {
                accessee: Box::new(accessee),
                dot,
                member_identifier: member,
            },
            loc,
        })
    }

    fn parse_record_literal(
        &mut self,
        identifier: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        if let ExpressionKind::Variable { identifier } = identifier.kind {
            let open_curly = Self::expect_token(tokens, TokenKind::OpenCurly)?;
            let loc = open_curly.loc.clone();

            let mut args: Vec<Expression> = Vec::new();
            while tokens.peek().is_some() && tokens.peek().unwrap().kind != TokenKind::CloseCurly {
                self.allow_record_literals = true;
                let arg = self.parse_binary_expression(tokens, 0)?;
                self.allow_record_literals = false;
                args.push(arg);

                if tokens.peek().is_some() && tokens.peek().unwrap().kind != TokenKind::CloseCurly {
                    Self::expect_token(tokens, TokenKind::Comma)?;
                }
            }
            let close_curly = Self::expect_token(tokens, TokenKind::CloseCurly)?;

            return Ok(Expression {
                kind: ExpressionKind::RecordLiteral {
                    record_name: identifier.text,
                    open_curly,
                    args,
                    close_curly,
                },
                loc,
            });
        }
        Err(ParseError::ExpectedSemicolon(identifier.loc))
    }

    fn parse_array_index(
        &mut self,
        left: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let open_square = Self::expect_token(tokens, TokenKind::OpenSquare)?;
        let loc = open_square.loc.clone();
        let index = self.parse_binary_expression(tokens, 0)?;

        Self::expect_token(tokens, TokenKind::CloseSquare)?;

        return Ok(Expression {
            kind: ExpressionKind::ArrayIndex {
                array: Box::new(left),
                index: Box::new(index),
            },
            loc,
        });
    }

    fn parse_assignment(
        &mut self,
        lhs: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
        equals: &Token,
    ) -> Result<Expression, ParseError> {
        let loc = lhs.loc.clone();
        if let Some(_) = tokens.peek() {
            let rhs = self.parse_binary_expression(tokens, 0)?;
            return Ok(Expression {
                kind: ExpressionKind::Assignment {
                    lhs: Box::new(lhs),
                    equals: equals.clone(),
                    rhs: Box::new(rhs),
                },
                loc,
            });
        }
        Err(ParseError::UnexpectedEOF)
    }

    fn parse_function_call(
        &mut self,
        callee: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let loc = callee.loc.clone();

        let mut args: Vec<Expression> = Vec::new();

        Self::expect_token(tokens, TokenKind::OpenParen)?;

        if tokens.peek().is_some() && tokens.peek().unwrap().kind == TokenKind::CloseParen {
            Self::expect_token(tokens, TokenKind::CloseParen)?;
        } else {
            while tokens.peek().is_some() && tokens.peek().unwrap().kind != TokenKind::CloseParen {
                self.allow_record_literals = true;
                args.push(self.parse_binary_expression(tokens, 0)?);
                self.allow_record_literals = false;

                match tokens.next() {
                    Some(token) => match token.kind {
                        TokenKind::Comma => {}
                        TokenKind::CloseParen => {
                            break;
                        }
                        _ => return Err(ParseError::UnexpectedToken(token.clone())),
                    },
                    None => return Err(ParseError::UnexpectedEOF),
                }
            }
        }

        return Ok(Expression {
            kind: ExpressionKind::FunctionCall {
                callee: Box::new(callee),
                args,
            },
            loc,
        });
    }

    fn parse_expression(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        if let Some(current_token) = tokens.next() {
            return match current_token.kind {
                TokenKind::TrueKeyword | TokenKind::FalseKeyword => Ok(Expression {
                    kind: ExpressionKind::BoolLiteral {
                        token: current_token.clone(),
                    },
                    loc: current_token.loc.clone(),
                }),
                TokenKind::IntLiteral => Ok(Expression {
                    kind: ExpressionKind::IntLiteral {
                        token: current_token.clone(),
                    },
                    loc: current_token.loc.clone(),
                }),
                TokenKind::RealLiteral => Ok(Expression {
                    kind: ExpressionKind::RealLiteral {
                        token: current_token.clone(),
                    },
                    loc: current_token.loc.clone(),
                }),
                TokenKind::StringLiteral => Ok(Expression {
                    kind: ExpressionKind::StringLiteral {
                        token: current_token.clone(),
                    },
                    loc: current_token.loc.clone(),
                }),
                TokenKind::OpenParen => {
                    if let Some(current_token) = tokens.peek() {
                        let loc = current_token.loc.clone();
                        let expression = self.parse_binary_expression(tokens, 0)?;
                        Self::expect_token(tokens, TokenKind::CloseParen)?;

                        Ok(Expression {
                            kind: ExpressionKind::Parenthesised {
                                expression: Box::new(expression),
                            },
                            loc,
                        })
                    } else {
                        Err(ParseError::UnexpectedEOF)
                    }
                }
                TokenKind::OpenSquare => {
                    let open_square = current_token.clone();
                    let mut elements: Vec<Expression> = Vec::new();
                    while tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseSquare
                    {
                        elements.push(self.parse_binary_expression(tokens, 0)?);
                        if tokens.peek().is_some()
                            && tokens.peek().unwrap().kind != TokenKind::CloseSquare
                        {
                            Self::expect_token(tokens, TokenKind::Comma)?;
                        }
                    }
                    let close_square = Self::expect_token(tokens, TokenKind::CloseSquare)?;

                    Ok(Expression {
                        kind: ExpressionKind::ArrayLiteral {
                            open_square,
                            elements,
                            close_square,
                        },
                        loc: current_token.loc.clone(),
                    })
                }
                TokenKind::Identifier => {
                    let loc = current_token.loc.clone();
                    Ok(Expression {
                        kind: ExpressionKind::Variable {
                            identifier: current_token.clone(),
                        },
                        loc,
                    })
                }
                _ => Err(ParseError::UnexpectedToken(current_token.clone())),
            };
        }
        Err(ParseError::UnexpectedEOF)
    }

    fn expect_token(
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
        kind: TokenKind,
    ) -> Result<Token, ParseError> {
        match tokens.next() {
            Some(token) if token.kind == kind => Ok(token.clone()),
            Some(token) => Err(ParseError::TokenMismatch {
                expected: kind,
                actual: token.clone(),
            }),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
