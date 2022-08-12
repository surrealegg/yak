use crate::{
    ast::{
        Argument, Binary, BinaryKind, Block, Break, Call, Cast, Continue, Expression, Function,
        Grouping, If, Literal, LiteralKind, Param, Prototype, Ref, Return, Statement, Type, Unary,
        UnaryKind, Unsafe, VariableDeclaration, While,
    },
    error::{Error, ErrorKind, ErrorSeverity},
    tokens::{Token, TokenKind},
    utils::{unescape_string, Span},
};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    start: usize,
    current_span: Span,
    previous_span: Span,
    in_function: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            start: 0,
            previous_span: Span::default(),
            current_span: Span::default(),
            in_function: 0,
        }
    }

    fn span(&self) -> Span {
        Span {
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

    fn ignore_whitespace(&mut self) {
        while self.check(TokenKind::Semicolon) || self.check(TokenKind::EndLine) {
            self.advance();
        }
    }

    fn consume(&mut self, kinds: &[TokenKind]) -> Result<Token, Error> {
        if let Some(token) = self.matches(kinds) {
            Ok(token)
        } else {
            let mut span = Span {
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

    fn consume_endline(&mut self) -> Result<(), Error> {
        self.consume(&[TokenKind::EndLine, TokenKind::Semicolon])?;
        Ok(())
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

    fn peek_kind(&self, relative_index: usize) -> TokenKind {
        if let Some(token) = self.tokens.get(self.cursor + relative_index) {
            token.kind
        } else {
            TokenKind::EndOfFile
        }
    }

    fn peek(&self, relative_index: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + relative_index)
    }

    fn must_be_in_function(&mut self) -> Result<(), Error> {
        if self.in_function == 0 {
            Err(self.error(ErrorKind::ExpectedAnItem))
        } else {
            Ok(())
        }
    }

    fn literal(&mut self) -> Result<Expression, Error> {
        if let Some(token) = self.peek(0) {
            let kind = match token.kind {
                TokenKind::True => LiteralKind::Boolean(true),
                TokenKind::False => LiteralKind::Boolean(false),
                TokenKind::Char => {
                    LiteralKind::Char(token.slice[1..token.slice.len() - 1].to_string())
                }
                TokenKind::String => {
                    LiteralKind::String(unescape_string(&token.slice[1..token.slice.len() - 1]))
                }
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
                        let mut name = String::new();
                        let mut expr = self.expression()?;
                        if let Expression::Literal(literal) = &expr {
                            if let LiteralKind::Identifier(identifier) = &literal.kind {
                                if self.matches_bool(&[TokenKind::Colon]) {
                                    name = identifier.clone();
                                    expr = self.expression()?;
                                }
                            }
                        }

                        arguments.push(Argument { name, value: expr });
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
        if let Some(op) = self.matches(&[
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Not,
            TokenKind::Asterisk,
        ]) {
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

    fn reference(&mut self) -> Result<Expression, Error> {
        if self.matches_bool(&[TokenKind::Ampersand]) {
            let mutable = self.matches_bool(&[TokenKind::Mut]);
            let identifer = self.consume(&[TokenKind::Identifier])?;
            Ok(Expression::Ref(Ref {
                mutable,
                name: identifer.slice.clone(),
                span: self.span(),
            }))
        } else {
            self.unary()
        }
    }

    fn assignment(&mut self) -> Result<Expression, Error> {
        let mut left = self.reference()?;
        while let Some(op) = self.matches(&[
            TokenKind::PlusEqual,
            TokenKind::MinusEqual,
            TokenKind::SlashEqual,
            TokenKind::AsteriskEqual,
            TokenKind::RightShiftEqual,
            TokenKind::LeftShiftEqual,
            TokenKind::PercentEqual,
            TokenKind::Equal,
        ]) {
            let right = self.reference()?;
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
            TokenKind::Percent,
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
        self.must_be_in_function()?;
        let expr = self.comparison()?;
        if self.matches_bool(&[TokenKind::As]) {
            let kind = self.consume_type()?;
            Ok(Expression::Cast(Cast {
                expr: Box::new(expr),
                kind,
                span: self.span(),
            }))
        } else {
            Ok(expr)
        }
    }

    fn variable_declaration(&mut self, mutable: bool) -> Result<Statement, Error> {
        self.must_be_in_function()?;
        self.advance();
        let identifier = self.consume(&[TokenKind::Identifier])?;
        let mut variable_type = Type::Unknown;
        if self.matches_bool(&[TokenKind::Colon]) {
            variable_type = self.consume_type()?;
        }
        self.consume(&[TokenKind::Equal])?;
        let mut value = self.expression()?;
        if variable_type != Type::Unknown {
            value = Expression::Cast(Cast {
                expr: Box::new(value.clone()),
                kind: variable_type.clone(),
                span: value.span(),
            });
        }

        self.consume_endline()?;
        Ok(Statement::VariableDeclaration(VariableDeclaration {
            name: identifier.slice,
            variable_type,
            mutable,
            span: self.span(),
            value,
        }))
    }

    fn block(&mut self) -> Result<Vec<Statement>, Error> {
        self.must_be_in_function()?;
        self.consume(&[TokenKind::CurlyBracketOpen])?;
        let mut statements = vec![];
        self.ignore_whitespace();
        if !self.check(TokenKind::CurlyBracketClose) {
            loop {
                statements.push(self.statement()?);
                self.ignore_whitespace();
                if self.check(TokenKind::CurlyBracketClose) || self.check(TokenKind::EndOfFile) {
                    break;
                }
            }
        }
        self.consume(&[TokenKind::CurlyBracketClose])?;
        Ok(statements)
    }

    fn loop_statement(&mut self) -> Result<Statement, Error> {
        self.must_be_in_function()?;
        self.consume(&[TokenKind::Loop])?;
        let block = self.block()?;
        Ok(Statement::Loop(Block {
            statements: block,
            span: self.span(),
        }))
    }

    fn while_statement(&mut self) -> Result<Statement, Error> {
        self.must_be_in_function()?;
        self.consume(&[TokenKind::While])?;
        let expression = self.expression()?;
        let block = self.block()?;
        Ok(Statement::While(While {
            block,
            expression,
            span: self.span(),
        }))
    }

    fn if_statement(&mut self) -> Result<Statement, Error> {
        self.must_be_in_function()?;
        self.consume(&[TokenKind::If])?;
        let expression = self.expression()?;
        let true_block = self.block()?;
        let mut else_block = vec![];

        if self.matches_bool(&[TokenKind::Else]) {
            if self.check(TokenKind::If) {
                else_block.push(self.if_statement()?);
            } else {
                else_block = self.block()?;
            }
        }

        Ok(Statement::If(If {
            true_block,
            else_block,
            expression,
            span: self.span(),
        }))
    }

    fn consume_type(&mut self) -> Result<Type, Error> {
        let result = match self.peek_kind(0) {
            TokenKind::I8 => Type::I8,
            TokenKind::U8 => Type::U8,
            TokenKind::I16 => Type::I16,
            TokenKind::U16 => Type::U16,
            TokenKind::I32 => Type::I32,
            TokenKind::U32 => Type::U32,
            TokenKind::I64 => Type::I64,
            TokenKind::U64 => Type::U64,
            TokenKind::F32 => Type::F32,
            TokenKind::F64 => Type::F64,
            TokenKind::Bool => Type::Bool,
            TokenKind::CInt => Type::CInt,
            TokenKind::CChar => Type::CChar,
            TokenKind::USize => Type::USize,
            TokenKind::Void => Type::Void,
            TokenKind::Char => Type::Char,
            TokenKind::StringType => Type::String,
            TokenKind::Ampersand => {
                self.advance();
                let mutable = self.matches_bool(&[TokenKind::Mut]);
                let inner = Box::new(self.consume_type()?);
                if mutable {
                    return Ok(Type::MutRef(inner));
                }
                return Ok(Type::Ref(inner));
            }
            TokenKind::Raw => {
                self.advance();
                let kind = self.consume_type()?;
                return Ok(Type::Raw(Box::new(kind)));
            }
            _ => return Err(self.error(ErrorKind::ExpectedType)),
        };
        self.advance();
        Ok(result)
    }

    fn param(&mut self) -> Result<Param, Error> {
        let anon = self.matches_bool(&[TokenKind::Anon]);
        let name = self.consume(&[TokenKind::Identifier])?;
        self.consume(&[TokenKind::Colon])?;
        let kind = self.consume_type()?;
        Ok(Param {
            anon,
            name: name.slice,
            kind,
        })
    }

    fn prototype(&mut self, is_extern: bool) -> Result<Prototype, Error> {
        if is_extern {
            self.consume(&[TokenKind::Extern])?;
        }
        self.consume(&[TokenKind::Function])?;

        let name = self.consume(&[TokenKind::Identifier])?;
        let mut params = vec![];
        self.consume(&[TokenKind::ParenthesesOpen])?;

        if !self.check(TokenKind::ParenthesesClose) {
            loop {
                params.push(self.param()?);
                if !self.matches_bool(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(&[TokenKind::ParenthesesClose])?;
        let mut return_type = Type::Unknown;
        if self.matches_bool(&[TokenKind::Arrow]) {
            return_type = self.consume_type()?;
        }

        Ok(Prototype {
            is_extern,
            name: name.slice,
            params,
            return_type,
            span: self.span(),
        })
    }

    fn function(&mut self) -> Result<Statement, Error> {
        let prototype = self.prototype(false)?;
        self.in_function += 1;
        let statements = self.block()?;
        self.in_function -= 1;
        Ok(Statement::Function(Function {
            prototype,
            statements,
            span: self.span(),
        }))
    }

    fn unsafe_statement(&mut self) -> Result<Statement, Error> {
        self.must_be_in_function()?;
        self.advance();
        let block = self.block()?;
        Ok(Statement::Unsafe(Unsafe {
            statements: block,
            span: self.span(),
        }))
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        self.ignore_whitespace();
        self.start = self.current_span.start;
        match self.peek_kind(0) {
            TokenKind::Extern => {
                let prototype = self.prototype(true)?;
                self.consume_endline()?;
                Ok(Statement::Prototype(prototype))
            }
            TokenKind::Function => self.function(),
            TokenKind::Cpp => Err(self.error(ErrorKind::CPPInteropNotSupported)),
            TokenKind::Let => self.variable_declaration(false),
            TokenKind::Mut => self.variable_declaration(true),
            TokenKind::CurlyBracketOpen => Ok(Statement::Block(Block {
                statements: self.block()?,
                span: self.span(),
            })),
            TokenKind::Loop => self.loop_statement(),
            TokenKind::Unsafe => self.unsafe_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::Return => {
                self.advance();
                let expression = self.expression().ok();
                self.consume_endline()?;
                Ok(Statement::Return(Return {
                    expression,
                    span: self.span(),
                }))
            }
            TokenKind::Break => {
                self.advance();
                self.consume_endline()?;
                Ok(Statement::Break(Break { span: self.span() }))
            }
            TokenKind::Continue => {
                self.advance();
                self.consume_endline()?;
                Ok(Statement::Continue(Continue { span: self.span() }))
            }
            _ => {
                let result = Statement::Expression(self.expression()?);
                self.consume_endline()?;
                Ok(result)
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Error> {
        let mut result = vec![];
        loop {
            if self.is_eof() {
                break;
            }
            let statement = self.statement()?;
            result.push(statement);
        }
        Ok(result)
    }
}
