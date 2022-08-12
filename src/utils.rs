use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub id: usize,
}

impl Span {
    pub fn show(&self, contents: &[u8], color: &str) -> Option<String> {
        let lines = obtain_ranges(contents);
        let shown_lines = lines
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, item)| item.contains(&self.start) || item.contains(&self.end))
            .collect::<Vec<(usize, Range<usize>)>>();

        let code = shown_lines
            .iter()
            .map(|(line, item)| {
                (
                    *line,
                    String::from_utf8_lossy(&contents[item.start..item.end]).to_string(),
                )
            })
            .collect::<Vec<(usize, String)>>();

        let arrows = shown_lines
            .iter()
            .map(|(_, line)| {
                format!(
                    "{}{}\u{001b}[0m",
                    color,
                    line.clone()
                        .map(|index| {
                            if index >= self.start && index <= self.end {
                                '^'
                            } else {
                                ' '
                            }
                        })
                        .collect::<String>()
                )
            })
            .collect::<Vec<String>>();

        let mut merged = vec![];
        for ((line, lhs), rhs) in code.iter().cloned().zip(arrows) {
            merged.push(format!("{:4}| {}\n", line + 1, lhs));
            merged.push(format!("{:4}| {}\n", line + 1, rhs));
        }

        merged
            .iter()
            .cloned()
            .reduce(|lhs, rhs| format!("{}{}", lhs, rhs))
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            id: 0,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
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
