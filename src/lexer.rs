use crate::{
    error::{Error, ErrorKind, ErrorSeverity},
    tokens::{Token, TokenKind},
    utils::{
        is_alpha, is_alphanumeric, is_binary_numeric, is_hex_numeric, is_numeric, is_octal_numeric,
        is_space, Span,
    },
};

pub struct Lexer {
    cursor: usize,
    column: usize,
    line: usize,
    input: String,
    bytes: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let bytes = input.as_bytes().to_vec();
        Self {
            cursor: 0,
            line: 0,
            column: 0,
            bytes,
            input,
        }
    }

    fn peek(&self, relative_index: usize) -> u8 {
        if let Some(value) = self.bytes.get(self.cursor + relative_index) {
            *value
        } else {
            b'\0'
        }
    }

    fn next_char_is(&self, chr: u8) -> bool {
        self.peek(1) == chr
    }

    fn next_chars_are(&self, chrs: &[u8]) -> bool {
        if let Some(values) = self
            .bytes
            .get(self.cursor + 1..self.cursor + 1 + chrs.len())
        {
            chrs == values
        } else {
            false
        }
    }

    fn create_token(&mut self, kind: TokenKind, length: usize) -> Result<Token, Error> {
        let result = Token {
            span: Span {
                id: 0,
                line: self.line,
                start: self.column,
                end: self.column + length,
            },
            kind,
            slice: self
                .input
                .get(self.cursor..self.cursor + length)
                .unwrap()
                .to_string(),
        };
        self.cursor += length;
        self.column += length;
        Ok(result)
    }

    fn keyword_or_identifier(&mut self) -> Result<Token, Error> {
        let mut length = 1;
        loop {
            let chr = self.peek(length);
            if !(is_alphanumeric(chr) || chr == b'_') {
                break;
            }
            length += 1;
        }
        self.create_token(
            TokenKind::from(self.input.get(self.cursor..self.cursor + length).unwrap()),
            length,
        )
    }

    fn string(&mut self, quote: u8) -> Result<Token, Error> {
        let mut length = 1;
        let mut escaped = false;
        loop {
            let chr = self.peek(length);
            if chr == b'\0' {
                return Err(Error {
                    kind: ErrorKind::UnexpectedToken,
                    severity: ErrorSeverity::Error,
                    span: Span {
                        line: self.line,
                        start: self.column,
                        end: self.column + length,
                        id: 0,
                    },
                });
            }
            length += 1;
            if !escaped && chr == quote {
                break;
            }
            escaped = !escaped && chr == b'\\';
        }
        self.create_token(
            if quote == b'\'' {
                TokenKind::Char
            } else {
                TokenKind::String
            },
            length,
        )
    }

    fn number(&mut self, current: u8) -> Result<Token, Error> {
        let mut length = 1;

        if current == b'0' {
            match self.peek(length) {
                b'x' => {
                    length += 1;
                    while is_hex_numeric(self.peek(length)) {
                        length += 1;
                    }
                    return self.create_token(TokenKind::HexidecmialNumber, length);
                }
                b'o' => {
                    length += 1;
                    while is_octal_numeric(self.peek(length)) {
                        length += 1;
                    }
                    return self.create_token(TokenKind::OctalNumber, length);
                }
                b'b' => {
                    length += 1;
                    while is_binary_numeric(self.peek(length)) {
                        length += 1;
                    }
                    return self.create_token(TokenKind::BinaryNumber, length);
                }
                _ => {}
            }
        }

        let mut is_float = false;
        loop {
            match self.peek(length) {
                b'.' if self.peek(length + 1) != b'.' => {
                    if is_float {
                        return Err(Error {
                            kind: ErrorKind::UnexpectedToken,
                            severity: ErrorSeverity::Error,
                            span: Span {
                                line: self.line,
                                start: self.column,
                                end: self.column + length,
                                id: 0,
                            },
                        });
                    }
                    length += 1;
                    is_float = true;
                }
                b'_' if is_numeric(self.peek(length + 1)) => length += 2,
                chr if is_numeric(chr) => length += 1,
                _ => break,
            }
        }

        self.create_token(
            if is_float {
                TokenKind::FloatNumber
            } else {
                TokenKind::IntegerNumber
            },
            length,
        )
    }

    fn rest(&mut self, current: u8) -> Result<Token, Error> {
        if is_alpha(current) || current == b'_' {
            self.keyword_or_identifier()
        } else if is_numeric(current) {
            self.number(current)
        } else if current == b'"' || current == b'\'' {
            self.string(current)
        } else {
            Err(Error {
                kind: ErrorKind::UnexpectedToken,
                severity: ErrorSeverity::Error,
                span: Span {
                    line: self.line,
                    start: self.column - 1,
                    end: self.column,
                    id: 0,
                },
            })
        }
    }

    fn next(&mut self) -> Result<Token, Error> {
        // Ignore whitespace
        while is_space(self.peek(0)) {
            self.column += 1;
            self.cursor += 1;
        }

        match self.peek(0) {
            b'\0' => self.create_token(TokenKind::EndOfFile, 0),
            b'(' => self.create_token(TokenKind::ParenthesesOpen, 1),
            b')' => self.create_token(TokenKind::ParenthesesClose, 1),
            b'{' => self.create_token(TokenKind::CurlyBracketOpen, 1),
            b'}' => self.create_token(TokenKind::CurlyBracketClose, 1),
            b'[' => self.create_token(TokenKind::SquareBracketOpen, 1),
            b']' => self.create_token(TokenKind::SquareBracketClose, 1),
            b':' => self.create_token(TokenKind::Colon, 1),
            b';' => self.create_token(TokenKind::Semicolon, 1),
            b',' => self.create_token(TokenKind::Comma, 1),
            b'-' if self.next_char_is(b'=') => self.create_token(TokenKind::MinusEqual, 2),
            b'-' if self.next_char_is(b'>') => self.create_token(TokenKind::Arrow, 2),
            b'-' => self.create_token(TokenKind::Minus, 1),
            b'+' if self.next_char_is(b'=') => self.create_token(TokenKind::PlusEqual, 2),
            b'+' => self.create_token(TokenKind::Plus, 1),
            b'*' if self.next_char_is(b'=') => self.create_token(TokenKind::AsteriskEqual, 2),
            b'*' => self.create_token(TokenKind::Asterisk, 1),
            b'/' if self.next_char_is(b'=') => self.create_token(TokenKind::SlashEqual, 2),
            b'/' => self.create_token(TokenKind::Slash, 1),
            b'=' if self.next_char_is(b'=') => self.create_token(TokenKind::DoubleEqual, 2),
            b'=' => self.create_token(TokenKind::Equal, 1),
            b'!' if self.next_char_is(b'=') => self.create_token(TokenKind::BangEqual, 2),
            b'>' if self.next_chars_are(">=".as_bytes()) => {
                self.create_token(TokenKind::RightShiftEqual, 3)
            }
            b'>' if self.next_char_is(b'>') => self.create_token(TokenKind::RightShift, 2),
            b'>' if self.next_char_is(b'=') => self.create_token(TokenKind::GreatEqual, 2),
            b'>' => self.create_token(TokenKind::Great, 1),
            b'<' if self.next_char_is(b'<') => self.create_token(TokenKind::LeftShift, 2),
            b'<' if self.next_char_is(b'=') => self.create_token(TokenKind::LessEqual, 2),
            b'<' if self.next_chars_are("<=".as_bytes()) => {
                self.create_token(TokenKind::LeftShiftEqual, 3)
            }
            b'<' => self.create_token(TokenKind::Less, 1),
            b'\n' => {
                self.line += 1;
                self.column = 0;
                self.create_token(TokenKind::EndLine, 1)
            }
            rest => self.rest(rest),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Error> {
        let mut result = vec![];
        loop {
            let token = self.next()?;
            if token.kind == TokenKind::EndOfFile {
                break;
            }
            result.push(token);
        }

        Ok(result)
    }
}
