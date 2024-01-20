use crate::{
    lexer::{span_locs, Loc, Token, TokenKind},
    type_checker::{CheckedExpression, CheckedStatement},
};

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
        generic_type_parameters: Vec<Token>,
    },
    Enum {
        enum_keyword: Token,
        identifier: Token,
        variants: Vec<Token>,
    },
    Match {
        match_keyword: Token,
        expression: Expression,
        cases: Box<Statement>,
    },
    MatchCases {
        cases: Vec<Expression>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    //pub loc: Loc, //TODO: remove this now that get_loc exists
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
        array_type_expression_kind: TypeExpressionKind,
        open_curly: Token,
        elements: Vec<Expression>,
        close_curly: Token,
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
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    Parenthesised {
        expression: Box<Expression>,
    },
    Variable {
        identifier: Token,
    },
    TypeAnnotation {
        colon: Token,
        type_expression_kind: TypeExpressionKind,
    },
    FunctionCall {
        callee: Box<Expression>,
        open_paren: Token,
        args: Vec<Expression>,
        close_paren: Token,
    },
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    RecordLiteral {
        record_identifier: Token,
        open_curly: Token,
        args: Vec<Expression>,
        close_curly: Token,
    },
    Accessor {
        accessee: Box<Expression>,
        dot: Token,
        member_identifier: Token,
    },
    StaticAccessor {
        namespace: Token,
        colon_colon: Token,
        member: Box<Expression>,
    },
    Range {
        lower: Box<Expression>,
        dotdot: Token,
        upper: Box<Expression>,
    },
    MatchCase {
        pattern: Box<Expression>,
        fat_arrow: Token,
        result: Box<Expression>,
    },
}

pub trait Location {
    fn get_loc(&self) -> Loc;
}

impl Location for ExpressionKind {
    fn get_loc(&self) -> Loc {
        match self {
            ExpressionKind::BoolLiteral { token }
            | ExpressionKind::IntLiteral { token }
            | ExpressionKind::RealLiteral { token }
            | ExpressionKind::StringLiteral { token } => token.loc.clone(),
            ExpressionKind::ArrayLiteral {
                array_type_expression_kind,
                open_curly,
                elements,
                close_curly,
            } => return span_locs(&array_type_expression_kind.get_loc(), &close_curly.loc),
            ExpressionKind::Assignment { lhs, equals, rhs } => {
                return span_locs(&lhs.kind.get_loc(), &rhs.kind.get_loc());
            }
            ExpressionKind::FunctionParameter {
                modifiers,
                identifier,
                type_annotation,
            } => todo!(),
            ExpressionKind::Binary { left, op, right } => {
                span_locs(&left.kind.get_loc(), &right.kind.get_loc())
            }
            ExpressionKind::Unary { op, operand } => span_locs(&op.loc, &operand.kind.get_loc()),
            ExpressionKind::Parenthesised { expression } => todo!(),
            ExpressionKind::Variable { identifier } => identifier.loc.clone(),
            ExpressionKind::TypeAnnotation {
                colon,
                type_expression_kind,
            } => todo!(),
            ExpressionKind::FunctionCall {
                callee,
                open_paren,
                args,
                close_paren,
            } => {
                return span_locs(&callee.kind.get_loc(), &close_paren.loc);
            }
            ExpressionKind::ArrayIndex { array, index } => todo!(),
            ExpressionKind::RecordLiteral {
                record_identifier,
                open_curly,
                args,
                close_curly,
            } => span_locs(&record_identifier.loc, &close_curly.loc),
            ExpressionKind::Accessor {
                accessee,
                dot,
                member_identifier,
            } => {
                return span_locs(&accessee.kind.get_loc(), &member_identifier.loc);
            }
            ExpressionKind::Range {
                lower,
                dotdot,
                upper,
            } => todo!(),
            ExpressionKind::StaticAccessor {
                namespace,
                colon_colon: _,
                member,
            } => span_locs(&namespace.loc, &member.kind.get_loc()),
            ExpressionKind::MatchCase {
                pattern,
                fat_arrow: _,
                result,
            } => span_locs(&pattern.kind.get_loc(), &result.kind.get_loc()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpressionKind {
    //f32, MyRecord, etc
    Basic {
        identifier: Token,
    },
    // [5]i32, [5][6]f32, [2]MyRecord, etc
    Array {
        open_square: Token,
        length: usize,
        close_square: Token,
        element_type: Box<TypeExpressionKind>,
    },
    // []i32, [][6]f32, [][]u8, []MyRecord, etc
    Slice {
        open_square: Token,
        close_square: Token,
        element_type: Box<TypeExpressionKind>,
    },
    //MyRecord<u32>, Foo<u32, MyRecord<u32>>, List<[6]u32> etc
    Generic {
        generic_type: Box<TypeExpressionKind>,
        open_angle: Token,
        generic_parameter_types: Vec<TypeExpressionKind>,
        close_angle: Token,
    },
}

impl Location for TypeExpressionKind {
    fn get_loc(&self) -> Loc {
        match self {
            TypeExpressionKind::Basic { identifier } => identifier.loc.clone(),
            TypeExpressionKind::Array {
                open_square,
                length,
                close_square,
                element_type,
            } => span_locs(&open_square.loc, &element_type.get_loc()),
            TypeExpressionKind::Slice {
                open_square,
                close_square,
                element_type,
            } => todo!(),
            TypeExpressionKind::Generic {
                generic_type,
                open_angle,
                generic_parameter_types,
                close_angle,
            } => span_locs(&generic_type.get_loc(), &close_angle.loc),
        }
    }
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

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Negation,
    Not,
    Identity,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    TokenMismatch { expected: TokenKind, actual: Token },
    ExpectedSemicolon(Loc),
    UnexpectedEOF,
    CannotStaticallyAccess(Expression),
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
                        Some(self.parse_type_annotation(tokens)?)
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
                    let type_annotation = self.parse_type_annotation(tokens)?;

                    args.push(Expression {
                        kind: ExpressionKind::FunctionParameter {
                            modifiers,
                            identifier,
                            type_annotation: Box::new(type_annotation),
                        },
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
                        Some(self.parse_type_annotation(tokens)?)
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

                let mut generic_type_parameters: Vec<Token> = Vec::new();
                if tokens.peek().is_some() && tokens.peek().unwrap().kind == TokenKind::OpenAngle {
                    Self::expect_token(tokens, TokenKind::OpenAngle)?;
                    while tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseAngle
                    {
                        self.allow_record_literals = true;
                        let generic_type_parameter =
                            Self::expect_token(tokens, TokenKind::Identifier)?;
                        self.allow_record_literals = false;
                        generic_type_parameters.push(generic_type_parameter);

                        if tokens.peek().is_some()
                            && tokens.peek().unwrap().kind != TokenKind::CloseAngle
                        {
                            Self::expect_token(tokens, TokenKind::Comma)?;
                        }
                    }
                    Self::expect_token(tokens, TokenKind::CloseAngle)?;
                }
                Self::expect_token(tokens, TokenKind::OpenCurly)?;

                let mut members: Vec<(Token, Expression)> = Vec::new();
                while tokens.peek().is_some()
                    && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                {
                    let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                    let type_annotation = self.parse_type_annotation(tokens)?;

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
                        generic_type_parameters,
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
            TokenKind::EnumKeyword => {
                let enum_keyword = Self::expect_token(tokens, TokenKind::EnumKeyword)?;
                let loc = enum_keyword.loc.clone();

                let identifier = Self::expect_token(tokens, TokenKind::Identifier)?;
                Self::expect_token(tokens, TokenKind::OpenCurly)?;

                let mut variants: Vec<Token> = Vec::new();
                while tokens.peek().is_some()
                    && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                {
                    variants.push(Self::expect_token(tokens, TokenKind::Identifier)?);

                    if tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                    {
                        Self::expect_token(tokens, TokenKind::Comma)?;
                    }
                }

                Self::expect_token(tokens, TokenKind::CloseCurly)?;

                Ok(Statement {
                    kind: StatementKind::Enum {
                        enum_keyword,
                        identifier,
                        variants,
                    },
                    loc,
                })
            }
            TokenKind::MatchKeyword => {
                let match_keyword = Self::expect_token(tokens, TokenKind::MatchKeyword)?;
                let loc = match_keyword.loc.clone();
                let expression = self.parse_binary_expression(tokens, 0)?;

                let cases = self.parse_match_cases(tokens)?;

                Ok(Statement {
                    kind: StatementKind::Match {
                        match_keyword,
                        expression,
                        cases: Box::new(cases),
                    },
                    loc,
                })
            }
            _ => {
                let expression = self.parse_binary_expression(tokens, 0)?;
                let loc = expression.kind.get_loc().clone();

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
                _ => {
                    let statement = self.parse_statement(tokens)?;
                    statements.push(statement);
                }
            }
        }
        Self::expect_token(tokens, TokenKind::CloseCurly)?;

        Ok(Statement {
            kind: StatementKind::Block { statements },
            loc: open_curly_loc,
        })
    }

    fn parse_type_annotation(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let colon = Self::expect_token(tokens, TokenKind::Colon)?;
        let type_expression_kind = self.parse_type_expression_kind(tokens)?;
        Ok(Expression {
            kind: ExpressionKind::TypeAnnotation {
                colon: colon.clone(),
                type_expression_kind,
            },
        })
    }

    fn parse_type_expression_kind(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<TypeExpressionKind, ParseError> {
        if let Some(token) = tokens.next() {
            match &token.kind {
                TokenKind::Identifier => {
                    if let Some(next) = tokens.peek() {
                        match next.kind {
                            TokenKind::OpenAngle => {
                                let open_angle = Self::expect_token(tokens, TokenKind::OpenAngle)?;
                                let mut generic_type_parameters: Vec<TypeExpressionKind> =
                                    Vec::new();
                                while tokens.peek().is_some()
                                    && tokens.peek().unwrap().kind != TokenKind::CloseAngle
                                {
                                    let generic_type_parameter =
                                        self.parse_type_expression_kind(tokens)?;
                                    generic_type_parameters.push(generic_type_parameter);

                                    if tokens.peek().is_some()
                                        && tokens.peek().unwrap().kind != TokenKind::CloseAngle
                                    {
                                        Self::expect_token(tokens, TokenKind::Comma)?;
                                    }
                                }
                                let close_angle =
                                    Self::expect_token(tokens, TokenKind::CloseAngle)?;

                                return Ok(TypeExpressionKind::Generic {
                                    generic_type: Box::new(TypeExpressionKind::Basic {
                                        identifier: token.clone(),
                                    }),
                                    open_angle,
                                    generic_parameter_types: generic_type_parameters,
                                    close_angle,
                                });
                            }
                            _ => {
                                return Ok(TypeExpressionKind::Basic {
                                    identifier: token.clone(),
                                })
                            }
                        }
                    }
                    return Ok(TypeExpressionKind::Basic {
                        identifier: token.clone(),
                    });
                }
                TokenKind::OpenSquare => {
                    let open_square = token;

                    let next = Self::next_token(tokens)?;

                    match next.kind {
                        TokenKind::IntLiteral => {
                            let length_token = next;
                            let close_square = Self::expect_token(tokens, TokenKind::CloseSquare)?;

                            let element_type_kind = self.parse_type_expression_kind(tokens)?;

                            return Ok(TypeExpressionKind::Array {
                                open_square: open_square.clone(),
                                length: length_token.text.parse().expect("should not fail"),
                                close_square,
                                element_type: Box::new(element_type_kind),
                            });
                        }
                        TokenKind::CloseSquare => {
                            let element_type_kind = self.parse_type_expression_kind(tokens)?;

                            return Ok(TypeExpressionKind::Slice {
                                open_square: open_square.clone(),
                                close_square: token.clone(),
                                element_type: Box::new(element_type_kind),
                            });
                        }
                        _ => return Err(ParseError::UnexpectedToken(next)),
                    }
                }
                _ => Err(ParseError::UnexpectedToken(token.clone())),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn get_unary_operator_precedence(kind: &TokenKind) -> usize {
        return match kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::NotKeyword => 1,
            _ => 0,
        };
    }

    fn parse_unary_op(token: Token) -> Result<UnaryOp, ParseError> {
        return match token.kind {
            TokenKind::Plus => Ok(UnaryOp {
                kind: UnaryOpKind::Identity,
                loc: token.loc,
            }),
            TokenKind::Minus => Ok(UnaryOp {
                kind: UnaryOpKind::Negation,
                loc: token.loc,
            }),
            TokenKind::NotKeyword => Ok(UnaryOp {
                kind: UnaryOpKind::Not,
                loc: token.loc,
            }),
            _ => Err(ParseError::UnexpectedToken(token)),
        };
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
        if let Some(token) = tokens.peek() {
            let precedence = Self::get_unary_operator_precedence(&token.kind);

            let mut left = if precedence == 0 || precedence <= parent_precedence {
                self.parse_expression(tokens)?
            } else {
                let token = tokens.next().expect("should never fail");
                let unary_op = Self::parse_unary_op(token.clone())?;

                let operand = self.parse_binary_expression(tokens, precedence)?;

                Expression {
                    kind: ExpressionKind::Unary {
                        op: unary_op.clone(),
                        operand: Box::new(operand),
                    },
                }
            };
            let loc = left.kind.get_loc().clone();

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
                            }
                            _ => return Ok(left),
                        },
                        TokenKind::Dot => left = Self::parse_accessor(left, tokens)?,
                        TokenKind::DotDot => left = self.parse_range(left, tokens)?,
                        TokenKind::ColonColon => match left.kind {
                            ExpressionKind::Variable { identifier } => {
                                left = self.parse_static_accessor(identifier, tokens)?;
                            }
                            _ => return Err(ParseError::CannotStaticallyAccess(left)),
                        },
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
                    };
                }
            }
            Ok(left)
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn parse_accessor(
        accessee: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let dot = Self::expect_token(tokens, TokenKind::Dot)?;
        let member = Self::expect_token(tokens, TokenKind::Identifier)?;

        Ok(Expression {
            kind: ExpressionKind::Accessor {
                accessee: Box::new(accessee),
                dot,
                member_identifier: member,
            },
        })
    }

    fn parse_static_accessor(
        &mut self,
        namespace: Token,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let colon_colon = Self::expect_token(tokens, TokenKind::ColonColon)?;
        let member = self.parse_binary_expression(tokens, 0)?;

        Ok(Expression {
            kind: ExpressionKind::StaticAccessor {
                namespace,
                colon_colon,
                member: Box::new(member),
            },
        })
    }

    fn parse_range(
        &mut self,
        lower: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let dotdot = Self::expect_token(tokens, TokenKind::DotDot)?;
        let upper = self.parse_binary_expression(tokens, 0)?;

        Ok(Expression {
            kind: ExpressionKind::Range {
                lower: Box::new(lower),
                dotdot,
                upper: Box::new(upper),
            },
        })
    }

    fn parse_record_literal(
        &mut self,
        identifier: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        if let ExpressionKind::Variable { identifier } = identifier.kind {
            let open_curly = Self::expect_token(tokens, TokenKind::OpenCurly)?;

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
                    record_identifier: identifier,
                    open_curly,
                    args,
                    close_curly,
                },
            });
        }
        Err(ParseError::ExpectedSemicolon(identifier.kind.get_loc()))
    }

    fn parse_array_index(
        &mut self,
        left: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let open_square = Self::expect_token(tokens, TokenKind::OpenSquare)?;
        let index = self.parse_binary_expression(tokens, 0)?;

        Self::expect_token(tokens, TokenKind::CloseSquare)?;

        return Ok(Expression {
            kind: ExpressionKind::ArrayIndex {
                array: Box::new(left),
                index: Box::new(index),
            },
        });
    }

    fn parse_assignment(
        &mut self,
        lhs: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
        equals: &Token,
    ) -> Result<Expression, ParseError> {
        if let Some(_) = tokens.peek() {
            let rhs = self.parse_binary_expression(tokens, 0)?;
            return Ok(Expression {
                kind: ExpressionKind::Assignment {
                    lhs: Box::new(lhs),
                    equals: equals.clone(),
                    rhs: Box::new(rhs),
                },
            });
        }
        Err(ParseError::UnexpectedEOF)
    }

    fn parse_function_call(
        &mut self,
        callee: Expression,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        let mut args: Vec<Expression> = Vec::new();

        let open_paren = Self::expect_token(tokens, TokenKind::OpenParen)?;

        let mut close_paren: Option<Token> = None;
        if tokens.peek().is_some() && tokens.peek().unwrap().kind == TokenKind::CloseParen {
            close_paren = Some(Self::expect_token(tokens, TokenKind::CloseParen)?);
        } else {
            while close_paren.is_none()
                && tokens.peek().is_some()
                && tokens.peek().unwrap().kind != TokenKind::CloseParen
            {
                self.allow_record_literals = true;
                args.push(self.parse_binary_expression(tokens, 0)?);
                self.allow_record_literals = false;

                let token = tokens.next();
                match token {
                    Some(token) => match token.kind {
                        TokenKind::Comma => {}
                        TokenKind::CloseParen => close_paren = Some(token.clone()),
                        _ => return Err(ParseError::UnexpectedToken(token.clone())),
                    },
                    None => return Err(ParseError::UnexpectedEOF),
                }
            }
        }

        return Ok(Expression {
            kind: ExpressionKind::FunctionCall {
                callee: Box::new(callee),
                open_paren,
                args,
                close_paren: close_paren.unwrap(),
            },
        });
    }

    fn parse_match_cases(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Statement, ParseError> {
        let open_curly = Self::expect_token(tokens, TokenKind::OpenCurly)?;

        let mut cases: Vec<Expression> = Vec::new();
        while tokens.peek().is_some() && tokens.peek().unwrap().kind != TokenKind::CloseCurly {
            let pattern = self.parse_binary_expression(tokens, 0)?;
            let fat_arrow = Self::expect_token(tokens, TokenKind::FatArrow)?;
            let result = self.parse_binary_expression(tokens, 0)?;

            cases.push(Expression {
                kind: ExpressionKind::MatchCase {
                    pattern: Box::new(pattern),
                    fat_arrow,
                    result: Box::new(result),
                },
            });

            if tokens.peek().is_some() && tokens.peek().unwrap().kind != TokenKind::CloseCurly {
                Self::expect_token(tokens, TokenKind::Comma)?;
            }
        }
        let close_curly = Self::expect_token(tokens, TokenKind::CloseCurly)?;

        Ok(Statement {
            kind: StatementKind::MatchCases { cases },
            loc: span_locs(&open_curly.loc, &close_curly.loc),
        })
    }

    fn parse_expression(
        &mut self,
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Expression, ParseError> {
        //This one needs me to not advance the iterator
        if let Some(token) = tokens.peek() {
            match &token.kind {
                TokenKind::OpenSquare => {
                    let array_type_expression_kind = self.parse_type_expression_kind(tokens)?;

                    let open_curly = Self::expect_token(tokens, TokenKind::OpenCurly)?;

                    let mut elements: Vec<Expression> = Vec::new();
                    while tokens.peek().is_some()
                        && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                    {
                        elements.push(self.parse_binary_expression(tokens, 0)?);
                        if tokens.peek().is_some()
                            && tokens.peek().unwrap().kind != TokenKind::CloseCurly
                        {
                            Self::expect_token(tokens, TokenKind::Comma)?;
                        }
                    }
                    let close_curly = Self::expect_token(tokens, TokenKind::CloseCurly)?;

                    return Ok(Expression {
                        kind: ExpressionKind::ArrayLiteral {
                            array_type_expression_kind,
                            open_curly,
                            elements,
                            close_curly,
                        },
                    });
                }
                _ => {}
            }
        }
        //These ones do...
        if let Some(current_token) = tokens.next() {
            return match current_token.kind {
                TokenKind::TrueKeyword | TokenKind::FalseKeyword => Ok(Expression {
                    kind: ExpressionKind::BoolLiteral {
                        token: current_token.clone(),
                    },
                }),
                TokenKind::IntLiteral => Ok(Expression {
                    kind: ExpressionKind::IntLiteral {
                        token: current_token.clone(),
                    },
                }),
                TokenKind::RealLiteral => Ok(Expression {
                    kind: ExpressionKind::RealLiteral {
                        token: current_token.clone(),
                    },
                }),
                TokenKind::StringLiteral => Ok(Expression {
                    kind: ExpressionKind::StringLiteral {
                        token: current_token.clone(),
                    },
                }),
                TokenKind::OpenParen => {
                    if let Some(current_token) = tokens.peek() {
                        let expression = self.parse_binary_expression(tokens, 0)?;
                        Self::expect_token(tokens, TokenKind::CloseParen)?;

                        Ok(Expression {
                            kind: ExpressionKind::Parenthesised {
                                expression: Box::new(expression),
                            },
                        })
                    } else {
                        Err(ParseError::UnexpectedEOF)
                    }
                }
                TokenKind::Identifier => Ok(Expression {
                    kind: ExpressionKind::Variable {
                        identifier: current_token.clone(),
                    },
                }),
                _ => Err(ParseError::UnexpectedToken(tokens.next().unwrap().clone())),
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

    fn next_token(
        tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<Token, ParseError> {
        match tokens.next() {
            Some(token) => Ok(token.clone()),
            None => Err(ParseError::UnexpectedEOF),
        }
    }
}
