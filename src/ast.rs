use crate::{
    tokens::TokenKind,
    utils::{variant_eq, Span},
};

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Boolean(bool),
    Char(String),
    String(String),
    Identifier(String),
    Integer(i64),
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Unary {
    pub expr: Box<Expression>,
    pub kind: UnaryKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub kind: BinaryKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: String,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub expr: Box<Expression>,
    pub kind: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Call(Call),
    Grouping(Grouping),
    Cast(Cast),
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

#[derive(Debug, Clone, PartialEq)]
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

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::I8 => "i8".to_string(),
            Type::U8 => "u8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::U16 => "u16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::U32 => "u32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::CInt => "c_int".to_string(),
            Type::CChar => "c_char".to_string(),
            Type::USize => "usize".to_string(),
            Type::Void => "void".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "String".to_string(),
            Type::Unknown => "unknown".to_string(),
            Type::Raw(inner) => format!("raw {}", inner.to_string()),
        }
    }
}

impl Type {
    pub fn equal(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Raw(inner_a), Type::Raw(inner_b)) => inner_a.equal(inner_b),
            (a, b) => variant_eq(a, b),
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::CInt
            | Type::CChar
            | Type::USize
            | Type::Char => true,
            _ => false,
        }
    }

    pub fn can_cast(&self, other: &Type) -> bool {
        self.is_primitive() && other.is_primitive()
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
            TokenKind::String => Ok(Type::Raw(Box::new(Type::CChar))),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
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
pub struct Return {
    pub expression: Expression,
    pub span: Span,
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
    Return(Return),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(literal) => literal.span,
            Expression::Binary(binary) => binary.span,
            Expression::Unary(unary) => unary.span,
            Expression::Call(call) => call.span,
            Expression::Grouping(grouping) => grouping.span,
            Expression::Cast(cast) => cast.span,
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
            Statement::Return(ret) => ret.span,
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
