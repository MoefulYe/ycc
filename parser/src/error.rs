use ast::Loc;
use core::fmt;
use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError as LalrpopParseError;
use miette::{Diagnostic, NamedSource};
use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
    result,
};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum UserError<'input> {
    #[error("fail to parse `{2}` to integer: {1}")]
    #[diagnostic(code(ParseError::ParseIntegerError))]
    ParseIntegerError(#[label("here")] Loc, ParseIntError, &'input str),
    #[error("fail to parse `{2}` to float: {1}")]
    #[diagnostic(code(ParseError::ParseFloatError))]
    ParseFloatError(#[label("here")] Loc, ParseFloatError, &'input str),
}

#[derive(Error, Debug, Diagnostic)]
pub enum SourcedUserError<'input> {
    #[error("fail to parse `{2}` to integer: {1}")]
    #[diagnostic(code(ParseError::ParseIntegerError))]
    ParseIntegerError(
        #[label("here")] Loc,
        ParseIntError,
        &'input str,
        #[source_code] NamedSource,
    ),
    #[error("fail to parse `{2}` to float: {1}")]
    #[diagnostic(code(ParseError::ParseFloatError))]
    ParseFloatError(
        #[label("here")] Loc,
        ParseFloatError,
        &'input str,
        #[source_code] NamedSource,
    ),
}

impl<'input> From<(UserError<'input>, NamedSource)> for SourcedUserError<'input> {
    fn from((err, src): (UserError<'input>, NamedSource)) -> Self {
        match err {
            UserError::ParseIntegerError(loc, err, tok) => {
                SourcedUserError::ParseIntegerError(loc, err, tok, src)
            }
            UserError::ParseFloatError(loc, err, tok) => {
                SourcedUserError::ParseFloatError(loc, err, tok, src)
            }
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum YccParseError<'input> {
    #[diagnostic(code(ParseError::InvalidToken))]
    InvalidToken(#[label("here")] Loc, #[source_code] NamedSource),
    #[diagnostic(code(ParseError::UnrecognizedEof))]
    UnrecognizedEof(
        #[label("here")] Loc,
        Vec<String>,
        #[source_code] NamedSource,
    ),
    #[diagnostic(code(ParseError::UnrecognizedToken))]
    UnrecognizedToken(
        Token<'input>,
        #[label("here")] Range<Loc>,
        Vec<String>,
        #[source_code] NamedSource,
    ),
    #[diagnostic(code(ParseError::ExtraToken))]
    ExtraToken(
        Token<'input>,
        #[label("here")] Range<Loc>,
        #[source_code] NamedSource,
    ),
    #[diagnostic(transparent)]
    User(SourcedUserError<'input>),
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

impl<'input> Display for YccParseError<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YccParseError::InvalidToken(_, _) => write!(f, "invalid token"),
            YccParseError::UnrecognizedEof(_, expected, _) => {
                write!(f, "unrecognized eof")?;
                fmt_expected(f, expected)
            }
            YccParseError::UnrecognizedToken(token, _, expected, _) => {
                write!(f, "unrecognized token `{}`", token)?;
                fmt_expected(f, expected)
            }
            YccParseError::ExtraToken(token, _, _) => {
                write!(f, "extra token `{}`", token)
            }
            YccParseError::User(err) => write!(f, "{}", err),
        }
    }
}

impl<'input>
    From<(
        LalrpopParseError<Loc, Token<'input>, UserError<'input>>,
        NamedSource,
    )> for YccParseError<'input>
{
    fn from(
        (err, src): (
            LalrpopParseError<Loc, Token<'input>, UserError<'input>>,
            NamedSource,
        ),
    ) -> Self {
        match err {
            LalrpopParseError::InvalidToken { location } => {
                YccParseError::InvalidToken(location, src)
            }
            LalrpopParseError::UnrecognizedEof { location, expected } => {
                YccParseError::UnrecognizedEof(location, expected, src)
            }
            LalrpopParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => YccParseError::UnrecognizedToken(token, start..end, expected, src),
            LalrpopParseError::ExtraToken {
                token: (start, token, end),
            } => YccParseError::ExtraToken(token, start..end, src),
            LalrpopParseError::User { error } => {
                YccParseError::User(SourcedUserError::from((error, src)))
            }
        }
    }
}
