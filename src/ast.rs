use crate::{
    tokens::TokenKind,
    utils::{variant_eq, Span},
};

#[derive(Debug)]
pub enum LiteralKind {
    Boolean(bool),
    Char(String),
    String(String),
    Identifier(String),
    Integer(i64),
}

#[derive(Debug)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum BinaryKind {
    As,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    DoubleEqual,
    BangEqual,
    Great,
    GreatEqual,
    Less,
    LessEqual,
    RightShift,
    LeftShift,
    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    RightShiftEqual,
    LeftShiftEqual,
}

impl TryFrom<TokenKind> for BinaryKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::As => Ok(BinaryKind::As),
            TokenKind::Plus => Ok(BinaryKind::Plus),
            TokenKind::Minus => Ok(BinaryKind::Minus),
            TokenKind::Asterisk => Ok(BinaryKind::Asterisk),
            TokenKind::Slash => Ok(BinaryKind::Slash),
            TokenKind::Equal => Ok(BinaryKind::Equal),
            TokenKind::DoubleEqual => Ok(BinaryKind::DoubleEqual),
            TokenKind::BangEqual => Ok(BinaryKind::BangEqual),
            TokenKind::Great => Ok(BinaryKind::Great),
            TokenKind::GreatEqual => Ok(BinaryKind::GreatEqual),
            TokenKind::Less => Ok(BinaryKind::Less),
            TokenKind::LessEqual => Ok(BinaryKind::LessEqual),
            TokenKind::RightShift => Ok(BinaryKind::RightShift),
            TokenKind::LeftShift => Ok(BinaryKind::LeftShift),
            TokenKind::PlusEqual => Ok(BinaryKind::PlusEqual),
            TokenKind::MinusEqual => Ok(BinaryKind::MinusEqual),
            TokenKind::AsteriskEqual => Ok(BinaryKind::AsteriskEqual),
            TokenKind::SlashEqual => Ok(BinaryKind::SlashEqual),
            TokenKind::RightShiftEqual => Ok(BinaryKind::RightShiftEqual),
            TokenKind::LeftShiftEqual => Ok(BinaryKind::LeftShiftEqual),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum UnaryKind {
    Not,
    Plus,
    Minus,
}

impl TryFrom<TokenKind> for UnaryKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Not => Ok(UnaryKind::Not),
            TokenKind::Plus => Ok(UnaryKind::Plus),
            TokenKind::Minus => Ok(UnaryKind::Minus),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Unary {
    pub expr: Box<Expression>,
    pub kind: UnaryKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Binary {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub kind: BinaryKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Call {
    pub name: String,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Grouping {
    pub expr: Box<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Call(Call),
    Grouping(Grouping),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub variable_type: Type,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct While {
    pub expression: Expression,
    pub block: Vec<Statement>,
}

#[derive(Debug)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug)]
pub struct Continue {
    pub span: Span,
}

#[derive(Debug)]
pub struct If {
    pub expression: Expression,
    pub true_block: Vec<Statement>,
    pub else_block: Vec<Statement>,
}

#[derive(Debug)]
pub enum Type {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    CInt,
    CChar,
    USize,
    Void,
    Char,
    String,
    Unknown,
    Raw(Box<Type>),
}

impl Type {
    pub fn equal(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Raw(inner_a), Type::Raw(inner_b)) => inner_a.equal(inner_b),
            (a, b) => variant_eq(a, b),
        }
    }
}

impl TryFrom<TokenKind> for Type {
    type Error = ();

    fn try_from(kind: TokenKind) -> Result<Self, Self::Error> {
        match kind {
            TokenKind::I8 => Ok(Type::I8),
            TokenKind::U8 => Ok(Type::U8),
            TokenKind::I16 => Ok(Type::I16),
            TokenKind::U16 => Ok(Type::U16),
            TokenKind::I32 => Ok(Type::I32),
            TokenKind::U32 => Ok(Type::U32),
            TokenKind::I64 => Ok(Type::I64),
            TokenKind::U64 => Ok(Type::U64),
            TokenKind::F32 => Ok(Type::F32),
            TokenKind::F64 => Ok(Type::F64),
            TokenKind::Bool => Ok(Type::Bool),
            TokenKind::CInt => Ok(Type::CInt),
            TokenKind::CChar => Ok(Type::CChar),
            TokenKind::USize => Ok(Type::USize),
            TokenKind::Void => Ok(Type::Void),
            TokenKind::Char => Ok(Type::Char),
            TokenKind::String => Ok(Type::String),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Param {
    pub kind: Type,
    pub name: String,
    pub anon: bool,
}

#[derive(Debug)]
pub struct Prototype {
    pub params: Vec<Param>,
    pub return_type: Type,
    pub name: String,
    pub is_extern: bool,
    pub span: Span,
}

#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    Block(Block),
    Loop(Block),
    While(While),
    Break(Break),
    Continue(Continue),
    If(If),
    Prototype(Prototype),
    Function(Function),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(literal) => literal.span,
            Expression::Binary(binary) => binary.span,
            Expression::Unary(unary) => unary.span,
            Expression::Call(call) => call.span,
            Expression::Grouping(grouping) => grouping.span,
        }
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Expression(expr) => expr.span(),
            Statement::VariableDeclaration(decl) => decl.span,
            Statement::Break(stat) => stat.span,
            Statement::Continue(stat) => stat.span,
            Statement::Prototype(prototype) => prototype.span,
            _ => Span::default(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Type;

    #[test]
    fn test_variant_eq() {
        assert_eq!(Type::Bool.equal(&Type::Bool), true);
        assert_eq!(Type::Bool.equal(&Type::CChar), false);
        assert_eq!(
            Type::Raw(Box::new(Type::Bool)).equal(&Type::Raw(Box::new(Type::Bool))),
            true
        );
        assert_eq!(
            Type::Raw(Box::new(Type::Bool)).equal(&Type::Raw(Box::new(Type::CChar))),
            false
        );
    }
}
