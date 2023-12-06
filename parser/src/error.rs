use ast::Loc;
use core::fmt;
use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError as LalrpopParseError;
use miette::Diagnostic;
use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    result,
};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum UserError {
    #[error("fail to parse `{2}` to integer: {1}")]
    #[diagnostic(code(ParseError::ParseIntegerError))]
    ParseIntegerError(#[label("here")] Loc, ParseIntError, String),
    #[error("fail to parse `{2}` to float: {1}")]
    #[diagnostic(code(ParseError::ParseFloatError))]
    ParseFloatError(#[label("here")] Loc, ParseFloatError, String),
}

#[derive(Error, Debug, Diagnostic)]
pub enum YccParseError {
    #[diagnostic(code(ParseError::InvalidToken))]
    InvalidToken(#[label("here")] Loc),
    #[diagnostic(code(ParseError::UnrecognizedEof))]
    UnrecognizedEof(#[label("here")] Loc, Vec<String>),
    #[diagnostic(code(ParseError::UnrecognizedToken))]
    UnrecognizedToken(String, #[label("here")] Range<Loc>, Vec<String>),
    #[diagnostic(code(ParseError::ExtraToken))]
    ExtraToken(String, #[label("here")] Range<Loc>),
    #[diagnostic(transparent)]
    User(UserError),
}

fn fmt_expected(f: &mut fmt::Formatter<'_>, expected: &[String]) -> result::Result<(), fmt::Error> {
    if !expected.is_empty() {
        writeln!(f)?;
        for (i, e) in expected.iter().enumerate() {
            let sep = match i {
                0 => "Expected one of",
                _ if i < expected.len() - 1 => ",",
                // Last expected message to be written
                _ => " or",
            };
            write!(f, "{} {}", sep, e)?;
        }
    }
    Ok(())
}

impl Display for YccParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YccParseError::InvalidToken(_) => write!(f, "invalid token"),
            YccParseError::UnrecognizedEof(_, expected) => {
                write!(f, "unrecognized eof")?;
                fmt_expected(f, expected)
            }
            YccParseError::UnrecognizedToken(token, _, expected) => {
                write!(f, "unrecognized token `{}`", token)?;
                fmt_expected(f, expected)
            }
            YccParseError::ExtraToken(token, _) => {
                write!(f, "extra token `{}`", token)
            }
            YccParseError::User(err) => write!(f, "{}", err),
        }
    }
}

impl<'input> From<LalrpopParseError<Loc, Token<'input>, UserError>> for YccParseError {
    fn from(err: LalrpopParseError<Loc, Token<'input>, UserError>) -> Self {
        match err {
            LalrpopParseError::InvalidToken { location } => YccParseError::InvalidToken(location),
            LalrpopParseError::UnrecognizedEof { location, expected } => {
                YccParseError::UnrecognizedEof(location, expected)
            }
            LalrpopParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => YccParseError::UnrecognizedToken(token.1.to_owned(), start..end, expected),
            LalrpopParseError::ExtraToken {
                token: (start, token, end),
            } => YccParseError::ExtraToken(token.1.to_owned(), start..end),
            LalrpopParseError::User { error } => YccParseError::User(error),
        }
    }
}
