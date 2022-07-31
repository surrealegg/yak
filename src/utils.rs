use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
    pub id: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            begin: 0,
            end: 0,
            id: 0,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            begin: value.start,
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
