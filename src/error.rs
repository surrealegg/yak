use std::process::ExitCode;

use crate::{
    ast::{BinaryKind, Type, UnaryKind},
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
    BinaryBetweenIncompatibleTypes(Type, Type, BinaryKind),
    MismatchedTypes(Type, Type),
    FunctionNotFound(String),
    BreakOutsideLoop,
    ContinueOutsideLoop,
    FunctionArgumentCountMismatch(usize, usize),
    RedefinedName(String),
    NonPrimitive(Type, Type),
    WrongLabel(String, String),
    CantApplyUnary(UnaryKind, Type),
    InvalidLeftHandSideAssignment,
    CantAssignImmutableVariable(String),
    AmbiguousType,
    DeadCode,
    MutableReferenceNotAllowed,
    DeferenceNonPointerValue,
    UnnecessaryUnsafe,
}

impl From<ErrorKind> for u8 {
    fn from(kind: ErrorKind) -> Self {
        match kind {
            ErrorKind::UnexpectedToken => 1,
            ErrorKind::UnexpectedBinaryOperator => 2,
            ErrorKind::UnexpectedUnaryOperator => 3,
            ErrorKind::ExpectedLiteral => 4,
            ErrorKind::InvalidInteger => 5,
            ErrorKind::ExpectedToken(_) => 6,
            ErrorKind::CPPInteropNotSupported => 7,
            ErrorKind::ExpectedType => 8,
            ErrorKind::ExpectedAnItem => 9,
            ErrorKind::RedefinitionVariable(_) => 10,
            ErrorKind::VariableNotFound(_) => 11,
            ErrorKind::BinaryBetweenIncompatibleTypes(_, _, _) => 12,
            ErrorKind::MismatchedTypes(_, _) => 13,
            ErrorKind::FunctionNotFound(_) => 14,
            ErrorKind::BreakOutsideLoop => 15,
            ErrorKind::ContinueOutsideLoop => 16,
            ErrorKind::FunctionArgumentCountMismatch(_, _) => 17,
            ErrorKind::RedefinedName(_) => 18,
            ErrorKind::NonPrimitive(_, _) => 19,
            ErrorKind::WrongLabel(_, _) => 20,
            ErrorKind::CantApplyUnary(_, _) => 21,
            ErrorKind::InvalidLeftHandSideAssignment => 22,
            ErrorKind::CantAssignImmutableVariable(_) => 23,
            ErrorKind::AmbiguousType => 24,
            ErrorKind::DeadCode => 25,
            ErrorKind::MutableReferenceNotAllowed => 26,
            ErrorKind::DeferenceNonPointerValue => 27,
            ErrorKind::UnnecessaryUnsafe => 28,
        }
    }
}

impl From<ErrorKind> for ExitCode {
    fn from(kind: ErrorKind) -> Self {
        ExitCode::from(u8::from(kind))
    }
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
            ErrorKind::BinaryBetweenIncompatibleTypes(lhs, rhs, kind) => {
                format!(
                    "invalid operands of types '{}' and '{}' to binary operator '{}'",
                    lhs.to_string(),
                    rhs.to_string(),
                    kind.to_string(),
                )
            }
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
            ErrorKind::InvalidLeftHandSideAssignment => {
                "invalid left-hand side of assignment".to_string()
            }
            ErrorKind::CantAssignImmutableVariable(name) => {
                format!("cannot assign twice to immutable variable '{}'", name)
            }
            ErrorKind::DeadCode => "dead code after return statement".to_string(),
            ErrorKind::AmbiguousType => "an ambiguous type".to_string(),
            ErrorKind::MutableReferenceNotAllowed => {
                "cannot make mutable reference to immutable value".to_string()
            }
            ErrorKind::DeferenceNonPointerValue => "dereference of a non-pointer value".to_string(),
            ErrorKind::UnnecessaryUnsafe => "unnecessary unsafe block".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
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
            1,
            self.span.start,
        );
        let color = self.severity.to_color();
        let code = self.span.show(contents, &color)?;
        Some(format!("{}{}\n-----", summary, code))
    }
}
