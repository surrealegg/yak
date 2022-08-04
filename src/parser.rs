use crate::{
    ast::{
        Binary, BinaryKind, Call, Expression, Grouping, Literal, LiteralKind, Statement, Unary,
        UnaryKind,
    },
    error::{Error, ErrorKind, ErrorSeverity},
    tokens::{Token, TokenKind},
    utils::Span,
};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    start: usize,
    current_span: Span,
    previous_span: Span,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            start: 0,
            previous_span: Span::default(),
            current_span: Span::default(),
        }
    }

    fn span(&self) -> Span {
        Span {
            line: self.previous_span.line,
            start: self.start,
            end: self.previous_span.end,
            id: self.previous_span.id,
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        if let Some(token) = self.tokens.get(self.cursor) {
            token.kind == kind
        } else {
            false
        }
    }

    fn is_eof(&self) -> bool {
        if let Some(token) = self.tokens.get(self.cursor) {
            token.kind == TokenKind::EndOfFile
        } else {
            true
        }
    }

    fn matches_bool(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds.iter() {
            if self.check(kind.clone()) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(&mut self, kinds: &[TokenKind]) -> Result<Token, Error> {
        if let Some(token) = self.matches(kinds) {
            Ok(token)
        } else {
            let mut span = Span {
                line: self.current_span.line,
                start: self.current_span.end,
                end: self.current_span.end + 1,
                id: self.current_span.id,
            };
            if let Some(token) = self.peek(1) {
                span = token.span;
            }

            Err(Error {
                kind: ErrorKind::ExpectedToken(kinds.to_vec()),
                severity: ErrorSeverity::Error,
                span,
            })
        }
    }

    fn matches(&mut self, kinds: &[TokenKind]) -> Option<Token> {
        for kind in kinds.iter() {
            if self.check(kind.clone()) {
                self.advance();
                if let Some(token) = self.tokens.get(self.cursor - 1) {
                    return Some(token.clone());
                }
            }
        }
        return None;
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            kind,
            severity: ErrorSeverity::Error,
            span: self.current_span,
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.previous_span = self.current_span;
        self.cursor += 1;
        if let Some(token) = self.tokens.get(self.cursor) {
            self.current_span = token.span.clone();
        }
        self.tokens.get(self.cursor - 1)
    }

    fn peek(&self, relative_index: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + relative_index)
    }

    fn literal(&mut self) -> Result<Expression, Error> {
        if let Some(token) = self.peek(0) {
            let kind = match token.kind {
                TokenKind::True => LiteralKind::Boolean(true),
                TokenKind::False => LiteralKind::Boolean(false),
                TokenKind::Char => LiteralKind::Char(token.slice.clone()),
                TokenKind::String => LiteralKind::String(token.slice.clone()),
                TokenKind::Identifier => LiteralKind::Identifier(token.slice.clone()),
                TokenKind::IntegerNumber => LiteralKind::Integer(
                    token
                        .slice
                        .parse::<i64>()
                        .or_else(|_| Err(self.error(ErrorKind::InvalidInteger)))?,
                ),
                _ => return Err(self.error(ErrorKind::ExpectedLiteral)),
            };

            Ok(Expression::Literal(Literal {
                kind,
                span: token.span,
            }))
        } else {
            Err(self.error(ErrorKind::ExpectedLiteral))
        }
    }

    fn primary(&mut self) -> Result<Expression, Error> {
        if self.matches_bool(&[TokenKind::ParenthesesOpen]) {
            let result = Ok(Expression::Grouping(Grouping {
                expr: Box::from(self.expression()?),
                span: self.span(),
            }));
            self.consume(&[TokenKind::ParenthesesClose])?;
            result
        } else {
            let literal = self.literal()?;
            self.advance();
            Ok(literal)
        }
    }

    fn call(&mut self) -> Result<Expression, Error> {
        if let Some(name) = self.matches(&[TokenKind::Identifier]) {
            if self.matches_bool(&[TokenKind::ParenthesesOpen]) {
                let mut arguments = vec![];
                if !self.check(TokenKind::ParenthesesClose) {
                    loop {
                        arguments.push(self.expression()?);
                        if !self.matches_bool(&[TokenKind::Comma]) {
                            break;
                        }
                    }
                }
                self.consume(&[TokenKind::ParenthesesClose])?;
                return Ok(Expression::Call(Call {
                    span: self.span(),
                    arguments,
                    name: name.slice,
                }));
            }
            // backtrack
            self.cursor -= 1;
        }
        return self.primary();
    }

    fn unary(&mut self) -> Result<Expression, Error> {
        if let Some(op) = self.matches(&[TokenKind::Plus, TokenKind::Minus, TokenKind::Not]) {
            Ok(Expression::Unary(Unary {
                expr: Box::from(self.call()?),
                span: self.span(),
                kind: UnaryKind::try_from(op.kind)
                    .or_else(|_| Err(self.error(ErrorKind::UnexpectedUnaryOperator)))?,
            }))
        } else {
            self.call()
        }
    }

    fn assignment(&mut self) -> Result<Expression, Error> {
        let mut left = self.unary()?;
        while let Some(op) = self.matches(&[
            TokenKind::PlusEqual,
            TokenKind::MinusEqual,
            TokenKind::SlashEqual,
            TokenKind::AsteriskEqual,
            TokenKind::RightShiftEqual,
            TokenKind::LeftShiftEqual,
            TokenKind::Equal,
        ]) {
            let right = self.unary()?;
            left = Expression::Binary(Binary {
                span: self.span(),
                left: Box::from(left),
                right: Box::from(right),
                kind: BinaryKind::try_from(op.kind)
                    .or_else(|_| Err(self.error(ErrorKind::UnexpectedBinaryOperator)))?,
            });
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expression, Error> {
        let mut left = self.assignment()?;
        while let Some(op) = self.matches(&[
            TokenKind::Asterisk,
            TokenKind::Slash,
            TokenKind::RightShift,
            TokenKind::LeftShift,
        ]) {
            let right = self.assignment()?;
            left = Expression::Binary(Binary {
                span: self.span(),
                left: Box::from(left),
                right: Box::from(right),
                kind: BinaryKind::try_from(op.kind)
                    .or_else(|_| Err(self.error(ErrorKind::UnexpectedBinaryOperator)))?,
            });
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expression, Error> {
        let mut left = self.factor()?;
        while let Some(op) = self.matches(&[TokenKind::Plus, TokenKind::Minus]) {
            let right = self.factor()?;
            left = Expression::Binary(Binary {
                span: self.span(),
                left: Box::from(left),
                right: Box::from(right),
                kind: BinaryKind::try_from(op.kind.clone())
                    .or_else(|_| Err(self.error(ErrorKind::UnexpectedBinaryOperator)))?,
            });
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expression, Error> {
        let mut left = self.term()?;
        while let Some(op) = self.matches(&[
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Great,
            TokenKind::GreatEqual,
            TokenKind::BangEqual,
            TokenKind::DoubleEqual,
        ]) {
            let right = self.term()?;
            left = Expression::Binary(Binary {
                span: self.span(),
                left: Box::from(left),
                right: Box::from(right),
                kind: BinaryKind::try_from(op.kind)
                    .or_else(|_| Err(self.error(ErrorKind::UnexpectedBinaryOperator)))?,
            });
        }

        Ok(left)
    }

    fn expression(&mut self) -> Result<Expression, Error> {
        self.comparison()
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        let expr = Statement::Expression(self.expression()?);
        self.consume(&[TokenKind::EndLine, TokenKind::Semicolon])?;
        Ok(expr)
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Error> {
        let mut result = vec![];
        loop {
            while self.check(TokenKind::Semicolon) || self.check(TokenKind::EndLine) {
                self.advance();
            }
            if self.is_eof() {
                break;
            }
            self.start = self.current_span.start;
            let statement = self.statement()?;
            result.push(statement);
        }
        Ok(result)
    }
}
