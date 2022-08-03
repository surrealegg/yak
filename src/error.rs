use crate::utils::{obtain_ranges, Span};

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnexpectedToken,
    UnexpectedBinaryOperator,
    UnexpectedUnaryOperator,
    ExpectedLiteral,
    InvalidInteger,
}

impl ToString for ErrorKind {
    fn to_string(&self) -> String {
        match self {
            ErrorKind::UnexpectedToken => "unexpected token",
            ErrorKind::ExpectedLiteral => "expected literal",
            ErrorKind::InvalidInteger => "invalid integer",
            ErrorKind::UnexpectedBinaryOperator => "unexpected binary operator",
            ErrorKind::UnexpectedUnaryOperator => "unexpected unary operator",
        }
        .to_string()
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

#[derive(Debug, Clone, Copy)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
    pub severity: ErrorSeverity,
}

impl Error {
    pub fn show(&self, contents: &[u8], path: &str) -> Option<String> {
        let lines = obtain_ranges(contents);
        let line = lines.get(self.span.line)?;
        let str = String::from_utf8_lossy(&contents[line.clone()]);
        let color = self.severity.to_color();
        let summary = format!(
            "{}{}\u{001b}[0m: {}\n----- {}:{}:{}\n",
            self.severity.to_color(),
            self.severity.to_string(),
            self.kind.clone().to_string(),
            path,
            self.span.line + 1,
            self.span.start,
        );
        let code = format!("{:4}| {}\n", self.span.line + 1, str);
        let message = format!(
            "{}{}{}\u{001b}[0m",
            color,
            " ".repeat(self.span.start + 6),
            "^".repeat(self.span.end - self.span.start)
        );
        let mut result = format!("{}{}", code, message);
        if self.span.line >= 1 {
            if let Some(prev_line) = lines.get(self.span.line - 1) {
                result = format!(
                    "{:4}| {}\n{}",
                    self.span.line,
                    String::from_utf8_lossy(&contents[prev_line.clone()]),
                    result
                );
            }
        }
        if let Some(next_line) = lines.get(self.span.line + 1) {
            result = format!(
                "{}\n{:4}| {}",
                result,
                self.span.line + 2,
                String::from_utf8_lossy(&contents[next_line.clone()])
            );
        }
        result = format!("{}{}\n-----", summary, result);

        Some(result)
    }
}
