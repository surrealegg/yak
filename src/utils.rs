use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub start: usize,
    pub end: usize,
    pub id: usize,
}

impl Span {
    pub fn show(&self, contents: &[u8], color: &str) -> Option<String> {
        let lines = obtain_ranges(contents);
        let line = lines.get(self.line)?;
        let str = String::from_utf8_lossy(&contents[line.clone()]);
        let code = format!("{:4}| {}\n", self.line + 1, str);
        let message = format!(
            "    | {}{}{}\u{001b}[0m",
            " ".repeat(self.start),
            color,
            "^".repeat(self.end - self.start)
        );
        let mut result = format!("{}{}", code, message);
        if self.line >= 1 {
            if let Some(prev_line) = lines.get(self.line - 1) {
                result = format!(
                    "{:4}| {}\n{}",
                    self.line,
                    String::from_utf8_lossy(&contents[prev_line.clone()]),
                    result
                );
            }
        }
        if let Some(next_line) = lines.get(self.line + 1) {
            result = format!(
                "{}\n{:4}| {}",
                result,
                self.line + 2,
                String::from_utf8_lossy(&contents[next_line.clone()])
            );
        }
        Some(result)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            line: 0,
            start: 0,
            end: 0,
            id: 0,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            line: 0,
            start: value.start,
            end: value.end,
            id: 0,
        }
    }
}

pub(crate) fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

pub(crate) fn is_numeric(chr: u8) -> bool {
    chr >= b'0' && chr <= b'9'
}

pub(crate) fn is_alpha(chr: u8) -> bool {
    (chr >= b'a' && chr <= b'z') || (chr >= b'A' && chr <= b'Z')
}

pub(crate) fn is_alphanumeric(chr: u8) -> bool {
    is_alpha(chr) || is_numeric(chr)
}

pub(crate) fn is_hex_numeric(chr: u8) -> bool {
    (chr >= b'0' && chr <= b'9') || (chr >= b'a' && chr <= b'f') || (chr >= b'A' && chr <= b'F')
}

pub(crate) fn is_octal_numeric(chr: u8) -> bool {
    chr >= b'0' && chr <= b'7'
}

pub(crate) fn is_binary_numeric(chr: u8) -> bool {
    chr == b'0' || chr == b'1'
}

pub(crate) fn is_space(chr: u8) -> bool {
    chr == b' ' || chr == b'\t' || chr == b'\r'
}

pub(crate) fn unescape_string(input: &str) -> String {
    let mut result = String::new();
    let mut it = input.chars();

    while let Some(char) = it.next() {
        if char == '\\' {
            match it.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some(rest) => {
                    result.push('\\');
                    result.push(rest);
                }
                _ => {}
            }
        } else {
            result.push(char);
        }
    }

    result
}

pub(crate) fn obtain_ranges(str: &[u8]) -> Vec<Range<usize>> {
    let mut result = vec![];
    let mut idx = 0;
    let mut start = 0;

    while idx < str.len() {
        if str[idx] == b'\n' {
            result.push(start..idx);
            start = idx + 1;
        }
        idx += 1;
    }
    result.push(start..str.len());
    result
}
