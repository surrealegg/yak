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

#[derive(Debug, Clone, PartialEq)]
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
    Modulo,
    ModuloEqual,
}

impl BinaryKind {
    pub fn is_assign(&self) -> bool {
        match self {
            BinaryKind::Equal
            | BinaryKind::PlusEqual
            | BinaryKind::MinusEqual
            | BinaryKind::AsteriskEqual
            | BinaryKind::SlashEqual
            | BinaryKind::RightShiftEqual
            | BinaryKind::LeftShiftEqual => true,
            _ => false,
        }
    }
}

impl ToString for BinaryKind {
    fn to_string(&self) -> String {
        match self {
            BinaryKind::As => "as",
            BinaryKind::Plus => "+",
            BinaryKind::Minus => "-",
            BinaryKind::Asterisk => "* ",
            BinaryKind::Slash => "/",
            BinaryKind::Equal => "=",
            BinaryKind::DoubleEqual => "==",
            BinaryKind::BangEqual => "!=",
            BinaryKind::Great => ">",
            BinaryKind::GreatEqual => ">=",
            BinaryKind::Less => "<",
            BinaryKind::LessEqual => "<=",
            BinaryKind::RightShift => ">>",
            BinaryKind::LeftShift => "<<",
            BinaryKind::PlusEqual => "+=",
            BinaryKind::MinusEqual => "-=",
            BinaryKind::AsteriskEqual => "*=",
            BinaryKind::SlashEqual => "/=",
            BinaryKind::RightShiftEqual => ">>=",
            BinaryKind::LeftShiftEqual => "<<=",
            BinaryKind::Modulo => "%",
            BinaryKind::ModuloEqual => "%=",
        }
        .to_string()
    }
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
            TokenKind::Percent => Ok(BinaryKind::Modulo),
            TokenKind::PercentEqual => Ok(BinaryKind::ModuloEqual),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryKind {
    Not,
    Plus,
    Minus,
    Load,
}

impl TryFrom<TokenKind> for UnaryKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Not => Ok(UnaryKind::Not),
            TokenKind::Plus => Ok(UnaryKind::Plus),
            TokenKind::Minus => Ok(UnaryKind::Minus),
            TokenKind::Asterisk => Ok(UnaryKind::Load),
            _ => Err(()),
        }
    }
}

impl UnaryKind {
    pub fn can_apply(&self, kind: &Type) -> bool {
        match self {
            UnaryKind::Not if kind != &Type::Bool => false,
            UnaryKind::Plus | UnaryKind::Minus if !kind.is_numeric() => false,
            _ => true,
        }
    }
}

impl ToString for UnaryKind {
    fn to_string(&self) -> String {
        match self {
            UnaryKind::Not => "not",
            UnaryKind::Plus => "+",
            UnaryKind::Minus => "-",
            UnaryKind::Load => "*",
        }
        .to_string()
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
pub struct Argument {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: String,
    pub arguments: Vec<Argument>,
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
pub struct Ref {
    pub name: String,
    pub span: Span,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub values: Vec<Expression>,
    pub kind: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub expression: Box<Expression>,
    pub index: Box<Expression>,
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
    Ref(Ref),
    Array(Array),
    ArrayAccess(ArrayAccess),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub variable_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct While {
    pub expression: Expression,
    pub block: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub expression: Expression,
    pub true_block: Vec<Statement>,
    pub else_block: Vec<Statement>,
    pub span: Span,
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
    Ref(Box<Type>),
    MutRef(Box<Type>),
    Array(Box<Type>, u32),
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
            Type::Ref(inner) => format!("&{}", inner.to_string()),
            Type::MutRef(inner) => format!("&mut {}", inner.to_string()),
            Type::Array(inner, _) => format!("[{}]", inner.to_string()),
        }
    }
}

impl Type {
    pub fn equal(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Raw(inner_a), Type::Raw(inner_b))
            | (Type::Ref(inner_a), Type::Ref(inner_b))
            | (Type::MutRef(inner_a), Type::MutRef(inner_b))
            | (Type::Array(inner_a, _), Type::Array(inner_b, _)) => inner_a.equal(inner_b),
            (a, b) => variant_eq(a, b),
        }
    }

    pub fn merge(others: &[Type]) -> Type {
        if let Some(last) = others.last() {
            let mut it = others.iter().peekable();
            while let Some(other) = it.next() {
                if !(other == &Type::Void && it.peek().is_some()) && !last.equal(other) {
                    return Type::Unknown;
                }
            }
            return last.clone();
        }
        return Type::Void;
    }

    pub fn is_numeric(&self) -> bool {
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
            | Type::CInt
            | Type::USize => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::CInt
            | Type::USize => true,
            _ => false,
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

#[derive(Debug, Clone)]
pub struct Prototype {
    pub params: Vec<Param>,
    pub return_type: Type,
    pub name: String,
    pub is_extern: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expression: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unsafe {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
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
    Unsafe(Unsafe),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Expression(expression) => expression.span(),
            Statement::VariableDeclaration(variable_declaration) => variable_declaration.span,
            Statement::Block(block) => block.span,
            Statement::Loop(block) => block.span,
            Statement::While(while_statement) => while_statement.span,
            Statement::Break(break_statement) => break_statement.span,
            Statement::Continue(continue_statement) => continue_statement.span,
            Statement::If(if_statement) => if_statement.span,
            Statement::Prototype(prototype) => prototype.span,
            Statement::Function(function) => function.span,
            Statement::Return(return_statement) => return_statement.span,
            Statement::Unsafe(unsafe_statement) => unsafe_statement.span,
        }
    }
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
            Expression::Ref(reference) => reference.span,
            Expression::Array(array) => array.span,
            Expression::ArrayAccess(array_access) => array_access.span,
        }
    }

    pub fn get_variable_name(&self) -> String {
        match self {
            Expression::Literal(literal) => {
                if let LiteralKind::Identifier(name) = &literal.kind {
                    return name.clone();
                }
            }
            Expression::Unary(unary) => {
                if let UnaryKind::Load = &unary.kind {
                    return unary.expr.get_variable_name();
                }
            }
            Expression::ArrayAccess(array_access) => {
                return array_access.expression.get_variable_name();
            }
            _ => {}
        }
        return String::new();
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
