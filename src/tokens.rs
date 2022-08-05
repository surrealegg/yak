use crate::utils::Span;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Types
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

    // Keywords
    Function,
    Extern,
    Let,
    Mut,
    If,
    Else,
    While,
    Loop,
    Return,
    Not,
    And,
    Or,
    True,
    False,
    Raw,
    Anon,
    As,
    Break,
    Continue,
    Cpp,

    // Symbols
    Arrow,
    Comma,
    Colon,
    Semicolon,
    ParenthesesOpen,
    ParenthesesClose,
    CurlyBracketOpen,
    CurlyBracketClose,
    SquareBracketOpen,
    SquareBracketClose,
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

    // Literal
    Identifier,
    HexidecmialNumber,
    OctalNumber,
    BinaryNumber,
    FloatNumber,
    IntegerNumber,

    // Other
    EndLine,
    EndOfFile,
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        format!(
            "`{}`",
            match self {
                TokenKind::I8 => "i8",
                TokenKind::U8 => "u8",
                TokenKind::I16 => "i16",
                TokenKind::U16 => "u16",
                TokenKind::I32 => "i32",
                TokenKind::U32 => "u32",
                TokenKind::I64 => "i16",
                TokenKind::U64 => "u64",
                TokenKind::F32 => "f32",
                TokenKind::F64 => "f64 ",
                TokenKind::Bool => "bool",
                TokenKind::CInt => "c_int",
                TokenKind::CChar => "c_char",
                TokenKind::USize => "usize",
                TokenKind::Void => "void",
                TokenKind::Char => "char",
                TokenKind::String => "string",
                TokenKind::Function => "function",
                TokenKind::Extern => "extern",
                TokenKind::Let => "let",
                TokenKind::Mut => "mut",
                TokenKind::If => "if",
                TokenKind::Else => "else",
                TokenKind::While => "while",
                TokenKind::Loop => "loop",
                TokenKind::Return => "return",
                TokenKind::Not => "not",
                TokenKind::And => "and",
                TokenKind::Or => "or",
                TokenKind::True => "true",
                TokenKind::False => "false",
                TokenKind::Raw => "raw",
                TokenKind::Anon => "anon",
                TokenKind::As => "as",
                TokenKind::Arrow => "->",
                TokenKind::Comma => ",",
                TokenKind::Colon => ":",
                TokenKind::Semicolon => ";",
                TokenKind::ParenthesesOpen => "(",
                TokenKind::ParenthesesClose => ")",
                TokenKind::CurlyBracketOpen => "{",
                TokenKind::CurlyBracketClose => "}",
                TokenKind::SquareBracketOpen => "[",
                TokenKind::SquareBracketClose => "]",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Asterisk => "*",
                TokenKind::Slash => "/",
                TokenKind::Equal => "=",
                TokenKind::DoubleEqual => "==",
                TokenKind::BangEqual => "!=",
                TokenKind::Great => ">",
                TokenKind::GreatEqual => ">=",
                TokenKind::Less => "<",
                TokenKind::LessEqual => "<=",
                TokenKind::RightShift => ">>",
                TokenKind::LeftShift => "<<",
                TokenKind::PlusEqual => "+=",
                TokenKind::MinusEqual => "-=",
                TokenKind::AsteriskEqual => "*=",
                TokenKind::SlashEqual => "/=",
                TokenKind::RightShiftEqual => ">>=",
                TokenKind::LeftShiftEqual => "<<=",
                TokenKind::Identifier => "identifier",
                TokenKind::HexidecmialNumber => "hexidecimal number",
                TokenKind::OctalNumber => "octal number",
                TokenKind::BinaryNumber => "binary number",
                TokenKind::FloatNumber => "float",
                TokenKind::IntegerNumber => "integer",
                TokenKind::EndLine => "\\n",
                TokenKind::EndOfFile => "\\0",
                TokenKind::Break => "break",
                TokenKind::Continue => "continue",
                TokenKind::Cpp => "cpp",
            }
        )
    }
}

impl TokenKind {
    pub fn from_array(tokens: &[TokenKind]) -> String {
        tokens
            .iter()
            .map(|item| item.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    }

    pub fn is_type(&self) -> bool {
        match self {
            TokenKind::I8
            | TokenKind::U8
            | TokenKind::I16
            | TokenKind::U16
            | TokenKind::I32
            | TokenKind::U32
            | TokenKind::I64
            | TokenKind::U64
            | TokenKind::F32
            | TokenKind::F64
            | TokenKind::Bool
            | TokenKind::CInt
            | TokenKind::CChar
            | TokenKind::USize
            | TokenKind::Void
            | TokenKind::Char
            | TokenKind::String => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Equal
            | TokenKind::DoubleEqual
            | TokenKind::BangEqual
            | TokenKind::Great
            | TokenKind::GreatEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::RightShift
            | TokenKind::LeftShift
            | TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::AsteriskEqual
            | TokenKind::SlashEqual
            | TokenKind::RightShiftEqual
            | TokenKind::LeftShiftEqual
            | TokenKind::Or
            | TokenKind::And
            | TokenKind::As => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            TokenKind::Not | TokenKind::Plus | TokenKind::Minus => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            TokenKind::True
            | TokenKind::False
            | TokenKind::Identifier
            | TokenKind::HexidecmialNumber
            | TokenKind::OctalNumber
            | TokenKind::BinaryNumber
            | TokenKind::FloatNumber
            | TokenKind::IntegerNumber => true,
            _ => false,
        }
    }

    pub fn from(str: &str) -> TokenKind {
        match str {
            "function" => TokenKind::Function,
            "extern" => TokenKind::Extern,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "return" => TokenKind::Return,
            "not" => TokenKind::Not,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "raw" => TokenKind::Raw,
            "anon" => TokenKind::Anon,
            "as" => TokenKind::As,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "i8" => TokenKind::I8,
            "u8" => TokenKind::U8,
            "i16" => TokenKind::I16,
            "u16" => TokenKind::U16,
            "i32" => TokenKind::I32,
            "u32" => TokenKind::U32,
            "i64" => TokenKind::I64,
            "u64" => TokenKind::U64,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "bool" => TokenKind::Bool,
            "c_int" => TokenKind::CInt,
            "c_char" => TokenKind::CChar,
            "usize" => TokenKind::USize,
            "void" => TokenKind::Void,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "cpp" => TokenKind::Cpp,
            _ => TokenKind::Identifier,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub slice: String,
    pub span: Span,
}
