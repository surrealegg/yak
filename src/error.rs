use crate::{
    ast::{Type, UnaryKind},
    tokens::TokenKind,
    utils::Span,
};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedToken,
    UnexpectedBinaryOperator,
    UnexpectedUnaryOperator,
    ExpectedLiteral,
    InvalidInteger,
    ExpectedToken(Vec<TokenKind>),
    CPPInteropNotSupported,
    ExpectedType,
    ExpectedAnItem,
    RedefinitionVariable(String),
    VariableNotFound(String),
    BinaryBetweenIncompatibleTypes(Type, Type),
    DeadCode,
    MismatchedTypes(Type, Type),
    FunctionNotFound(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    FunctionArgumentCountMismatch(usize, usize),
    RedefinedName(String),
    NonPrimitive(Type, Type),
    WrongLabel(String, String),
    CantApplyUnary(UnaryKind, Type),
}

impl ToString for ErrorKind {
    fn to_string(&self) -> String {
        match self {
            ErrorKind::UnexpectedToken => "unexpected token".to_string(),
            ErrorKind::ExpectedLiteral => "expected literal".to_string(),
            ErrorKind::InvalidInteger => "invalid integer".to_string(),
            ErrorKind::UnexpectedBinaryOperator => "unexpected binary operator".to_string(),
            ErrorKind::UnexpectedUnaryOperator => "unexpected unary operator".to_string(),
            ErrorKind::ExpectedToken(tokens) => {
                format!("expected token: {}", TokenKind::from_array(&tokens))
            }
            ErrorKind::CPPInteropNotSupported => {
                "c++ interoperability is not supported".to_string()
            }
            ErrorKind::ExpectedType => "expected type".to_string(),
            ErrorKind::ExpectedAnItem => "expected an item".to_string(),
            ErrorKind::RedefinitionVariable(variable) => {
                format!("redefinition of variable '{}'", variable)
            }
            ErrorKind::VariableNotFound(variable) => format!("variable '{}' not found", variable),
            ErrorKind::BinaryBetweenIncompatibleTypes(lhs, rhs) => {
                format!(
                    "binary arithmetic operation between incompatible types ('{}', '{}')",
                    lhs.to_string(),
                    rhs.to_string()
                )
            }
            ErrorKind::DeadCode => "dead code after return statement".to_string(),
            ErrorKind::MismatchedTypes(expected, actual) => format!(
                "mismatched types. expected '{}', but got '{}'",
                expected.to_string(),
                actual.to_string()
            ),
            ErrorKind::FunctionNotFound(function) => format!("function '{}' not found", function),
            ErrorKind::BreakOutsideLoop => "'break' outside of a loop".to_string(),
            ErrorKind::ContinueOutsideLoop => "'continue' outside of a loop".to_string(),
            ErrorKind::FunctionArgumentCountMismatch(expected, actual) => format!(
                "this function takes {} arguments but {} argument was supplied",
                expected, actual
            ),
            ErrorKind::RedefinedName(name) => {
                format!("the name '{}' is defined multiple times", name)
            }
            ErrorKind::NonPrimitive(src, target) => {
                format!(
                    "non-primitive cast: '{}' as '{}'",
                    src.to_string(),
                    target.to_string()
                )
            }
            ErrorKind::WrongLabel(expected, actual) => {
                format!(
                    "wrong parameter name in argument label. expected '{}', but got '{}'",
                    expected, actual
                )
            }
            ErrorKind::CantApplyUnary(op, kind) => {
                format!(
                    "cannot apply unary operator '{}' to type '{}'",
                    op.to_string(),
                    kind.to_string()
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorSeverity {
    Warning,
    Error,
}

impl ErrorSeverity {
    pub fn to_color(&self) -> String {
        match self {
            ErrorSeverity::Warning => "\u{001b}[33m",
            ErrorSeverity::Error => "\u{001b}[31m",
        }
        .to_string()
    }
}

impl ToString for ErrorSeverity {
    fn to_string(&self) -> String {
        match self {
            ErrorSeverity::Warning => "warning",
            ErrorSeverity::Error => "error",
        }
        .to_string()
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
    pub severity: ErrorSeverity,
}

impl Error {
    pub fn show(&self, contents: &[u8], path: &str) -> Option<String> {
        let summary = format!(
            "{}{}\u{001b}[0m: {}\n----- {}:{}:{}\n",
            self.severity.to_color(),
            self.severity.to_string(),
            self.kind.clone().to_string(),
            path,
            self.span.line + 1,
            self.span.start,
        );
        let color = self.severity.to_color();
        let code = self.span.show(contents, &color)?;
        Some(format!("{}{}\n-----", summary, code))
    }
}
