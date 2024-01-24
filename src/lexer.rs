use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

#[derive(Debug)]
pub enum LexError {
    IoError(io::Error),
    InvalidCharacter(char, Loc),
    UnterminatedCharLiteral(Loc),
}

impl From<io::Error> for LexError {
    fn from(error: io::Error) -> Self {
        LexError::IoError(error)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    IntLiteral,
    CharLiteral,
    RealLiteral,
    StringLiteral,
    Identifier,
    TrueKeyword,
    FalseKeyword,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    EqualsEquals,
    OpenAngle,
    CloseAngle,
    OpenAngleEquals,
    CloseAngleEquals,
    OpenSquare,
    CloseSquare,
    Bang,
    BangEquals,
    AndKeyword,
    OrKeyword,
    XorKeyword,
    NotKeyword,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Semicolon,
    Colon,
    VarKeyword,
    FunctionKeyword,
    ReturnKeyword,
    Comma,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    ForKeyword,
    InKeyword,
    RecordKeyword,
    Dot,
    DotDot,
    DotDotDot,
    BreakKeyword,
    ContinueKeyword,
    WithKeyword,
    EnumKeyword,
    ColonColon,
    MatchKeyword,
    FatArrow,
    QuestionMark,
    NilKeyword,
    QuestionDot,
    QuestionColon,
    ImportKeyword,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc {
    pub file: String,
    pub row: usize,
    pub col: usize,
    pub len: usize,
}
impl Loc {
    pub fn null() -> Loc {
        Loc {
            file: "".to_string(),
            row: 0,
            col: 0,
            len: 0,
        }
    }
}

pub fn span_locs(start: &Loc, end: &Loc) -> Loc {
    if start.file != end.file {
        panic!("Locs are in different files");
    }
    if start.row != end.row {
        return start.clone();
    }

    Loc {
        file: start.file.clone(),
        row: start.row,
        col: start.col,
        len: end.col + end.len - start.col,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Loc,
}

impl Token {
    fn new(kind: TokenKind, text: String, loc: Loc) -> Self {
        Token { kind, text, loc }
    }
}

fn take_while(line: &str, col: usize, mut predicate: impl FnMut(char) -> bool) -> String {
    let mut result = String::new();
    let mut current_col = col;

    while let Some(next_char) = line.chars().nth(current_col) {
        if predicate(next_char) {
            result.push(next_char);
            current_col += 1;
        } else {
            break;
        }
    }

    result
}

pub fn lex_file(file: String) -> Result<Vec<Token>, LexError> {
    let file_path = Path::new(&file);
    let file = File::open(&file)?;

    let reader = BufReader::new(file);
    let mut tokens: Vec<Token> = Vec::new();
    let mut row = 1;
    let mut col: usize;

    for line in reader.lines() {
        let line = line?;
        col = 0;

        while col < line.len() {
            let c = line.chars().nth(col).unwrap(); //Doesn't handle unicode

            if c.is_whitespace() {
                col += 1;
                continue;
            }

            match c {
                '0'..='9' => {
                    let int_literal = take_while(&line, col, |c| c.is_ascii_digit());
                    col += int_literal.len();

                    // Check for a dot after parsing digits
                    if line.chars().nth(col).is_some()
                        && line.chars().nth(col).unwrap() == '.'
                        && line.chars().nth(col + 1).is_some()
                        && line.chars().nth(col + 1).unwrap() != '.'
                    {
                        col += 1; // Move past the dot
                        let decimal_part = take_while(&line, col, |c| c.is_ascii_digit());
                        col += decimal_part.len();

                        let real_literal = format!("{}.{}", int_literal, decimal_part);
                        tokens.push(Token::new(
                            TokenKind::RealLiteral,
                            real_literal.clone(),
                            Loc {
                                file: file_path.to_string_lossy().into_owned(),
                                row,
                                col: col - real_literal.len(), // Adjust col to the start of the real literal
                                len: real_literal.len(),
                            },
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenKind::IntLiteral,
                            int_literal.clone(),
                            Loc {
                                file: file_path.to_string_lossy().into_owned(),
                                row,
                                col: col - int_literal.len(), // Adjust col to the start of the int literal
                                len: int_literal.len(),
                            },
                        ));
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    let identifier =
                        take_while(&line, col, |c| c.is_ascii_alphanumeric() || c == '_');

                    if let Some(keyword_kind) = match_keyword(&identifier) {
                        tokens.push(Token::new(
                            keyword_kind,
                            identifier.clone(),
                            Loc {
                                file: file_path.to_string_lossy().into_owned(),
                                row,
                                col,
                                len: identifier.len(),
                            },
                        ));
                        col += identifier.len();
                        continue;
                    }

                    tokens.push(Token::new(
                        TokenKind::Identifier,
                        identifier.clone(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: identifier.len(),
                        },
                    ));
                    col += identifier.len();
                }
                '"' => {
                    let mut string_literal = String::new();
                    col += 1; // Move past the opening double quote

                    // Consume characters until the closing double quote
                    while let Some(c) = line.chars().nth(col) {
                        if c == '"' {
                            // Move past the closing double quote
                            col += 1;
                            break;
                        }
                        string_literal.push(c);
                        col += 1;
                    }

                    tokens.push(Token::new(
                        TokenKind::StringLiteral,
                        string_literal.clone(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col: col - string_literal.len() - 2, // Adjust col to the start of the string literal
                            len: string_literal.len() + 2,
                        },
                    ));
                }
                '\'' => {
                    col += 1; // Move past the opening quote

                    if let Some(c) = line.chars().nth(col) {
                        //TODO this is all assuming no escape characters
                        col += 1;

                        if let Some('\'') = line.chars().nth(col) {
                            col += 1;
                            tokens.push(Token::new(
                                TokenKind::CharLiteral,
                                c.to_string(),
                                Loc {
                                    file: file_path.to_string_lossy().into_owned(),
                                    row,
                                    col: col - 2, // Adjust col to the start of the char literal
                                    len: 3,
                                },
                            ));
                        } else {
                            return Err(LexError::UnterminatedCharLiteral(Loc {
                                file: file_path.to_string_lossy().into_owned(),
                                row,
                                col: col - 2, // Adjust col to the start of the char literal
                                len: 3,
                            }));
                        }
                    }
                }
                '+' => {
                    tokens.push(Token::new(
                        TokenKind::Plus,
                        "+".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '-' => {
                    tokens.push(Token::new(
                        TokenKind::Minus,
                        "-".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '*' => {
                    tokens.push(Token::new(
                        TokenKind::Star,
                        "*".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '/' => match line.chars().nth(col + 1) {
                    Some('/') => {
                        //Comment
                        row += 1;
                        break;
                    }
                    _ => {
                        tokens.push(Token::new(
                            TokenKind::Slash,
                            "/".to_string(),
                            Loc {
                                file: file_path.to_string_lossy().into_owned(),
                                row,
                                col,
                                len: 1,
                            },
                        ));
                        col += 1;
                    }
                },
                '%' => {
                    tokens.push(Token::new(
                        TokenKind::Percent,
                        "%".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '(' => {
                    tokens.push(Token::new(
                        TokenKind::OpenParen,
                        "(".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                ')' => {
                    tokens.push(Token::new(
                        TokenKind::CloseParen,
                        ")".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '[' => {
                    tokens.push(Token::new(
                        TokenKind::OpenSquare,
                        "[".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                ']' => {
                    tokens.push(Token::new(
                        TokenKind::CloseSquare,
                        "]".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                ',' => {
                    tokens.push(Token::new(
                        TokenKind::Comma,
                        ",".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '.' => {
                    let (token_kind, advance_by) = match line.chars().nth(col + 1) {
                        Some('.') => match line.chars().nth(col + 2) {
                            Some('.') => (TokenKind::DotDotDot, 3),
                            _ => (TokenKind::DotDot, 2),
                        },
                        _ => (TokenKind::Dot, 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        ".".repeat(advance_by),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                '?' => {
                    let (token_kind, text, advance_by) = match line.chars().nth(col + 1) {
                        Some('.') => (TokenKind::QuestionDot, "?.", 2),
                        Some(':') => (TokenKind::QuestionColon, "?:", 2),
                        _ => (TokenKind::QuestionMark, "?", 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        text.to_owned(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                '{' => {
                    tokens.push(Token::new(
                        TokenKind::OpenCurly,
                        "{".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '}' => {
                    tokens.push(Token::new(
                        TokenKind::CloseCurly,
                        "}".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                '=' => {
                    let (token_kind, advance_by) = match line.chars().nth(col + 1) {
                        Some('=') => (TokenKind::EqualsEquals, 2),
                        Some('>') => (TokenKind::FatArrow, 2),
                        _ => (TokenKind::Equals, 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        "=".repeat(advance_by),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                '<' => {
                    let (token_kind, text, advance_by) = match line.chars().nth(col + 1) {
                        Some('=') => (TokenKind::OpenAngleEquals, "<=", 2),
                        _ => (TokenKind::OpenAngle, "<", 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        text.to_owned(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                '!' => {
                    let (token_kind, text, advance_by) = match line.chars().nth(col + 1) {
                        Some('=') => (TokenKind::BangEquals, "!=", 2),
                        _ => (TokenKind::Bang, "!", 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        text.to_owned(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                '>' => {
                    let (token_kind, text, advance_by) = match line.chars().nth(col + 1) {
                        Some('=') => (TokenKind::CloseAngleEquals, ">=", 2),
                        _ => (TokenKind::CloseAngle, ">", 1),
                    };

                    tokens.push(Token::new(
                        token_kind,
                        text.to_owned(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                ':' => {
                    let (token_kind, text, advance_by) = match line.chars().nth(col + 1) {
                        Some(':') => (TokenKind::ColonColon, "::", 2),
                        _ => (TokenKind::Colon, ":", 1),
                    };
                    tokens.push(Token::new(
                        token_kind,
                        text.to_owned(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: advance_by,
                        },
                    ));

                    col += advance_by;
                }
                ';' => {
                    tokens.push(Token::new(
                        TokenKind::Semicolon,
                        ";".to_string(),
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                    col += 1;
                }
                _ => {
                    return Err(LexError::InvalidCharacter(
                        c,
                        Loc {
                            file: file_path.to_string_lossy().into_owned(),
                            row,
                            col,
                            len: 1,
                        },
                    ));
                }
            }
        }

        // Increment the row after processing a line
        row += 1;
    }

    Ok(tokens)
}

fn match_keyword(identifier: &str) -> Option<TokenKind> {
    match identifier {
        "true" => Some(TokenKind::TrueKeyword),
        "false" => Some(TokenKind::FalseKeyword),
        "and" => Some(TokenKind::AndKeyword),
        "or" => Some(TokenKind::OrKeyword),
        "xor" => Some(TokenKind::XorKeyword),
        "not" => Some(TokenKind::NotKeyword),
        "var" => Some(TokenKind::VarKeyword),
        "function" => Some(TokenKind::FunctionKeyword),
        "if" => Some(TokenKind::IfKeyword),
        "else" => Some(TokenKind::ElseKeyword),
        "while" => Some(TokenKind::WhileKeyword),
        "return" => Some(TokenKind::ReturnKeyword),
        "record" => Some(TokenKind::RecordKeyword),
        "break" => Some(TokenKind::BreakKeyword),
        "continue" => Some(TokenKind::ContinueKeyword),
        "with" => Some(TokenKind::WithKeyword),
        "enum" => Some(TokenKind::EnumKeyword),
        "match" => Some(TokenKind::MatchKeyword),
        "nil" => Some(TokenKind::NilKeyword),
        "import" => Some(TokenKind::ImportKeyword),
        "for" => Some(TokenKind::ForKeyword),
        "in" => Some(TokenKind::InKeyword),
        _ => None,
    }
}
