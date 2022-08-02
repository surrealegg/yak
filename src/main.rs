use crate::{
    error::{Error, ErrorKind, ErrorSeverity},
    utils::Span,
};

mod error;
mod tokens;
mod utils;

const TEMP_CODE: &str =
    "extern function write(anon io: i32, anon buffer: raw c_char, anon count: usize) -> usize
function main() {
    write(1, \"Hello World!\\n\", 13)
}";

fn main() {
    let err = Error {
        span: Span {
            line: 2,
            start: 10,
            end: 16,
            id: 1,
        },
        severity: ErrorSeverity::Error,
        kind: ErrorKind::UnexpectedToken,
    };
    eprintln!(
        "{}",
        err.show(
            TEMP_CODE.as_bytes(),
            "/tmp/jakt-073df714-23e5-48b8-8776-13d90aa62f43.jakt"
        )
        .unwrap()
    );
}
