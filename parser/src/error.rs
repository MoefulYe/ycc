use ast::Loc;
use miette::Diagnostic;
use std::num::{ParseFloatError, ParseIntError};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum UserError<'input> {
    #[error("fail to parse `{3}` to integer: {2}")]
    #[diagnostic(code(ParseError::ParseIntegerError))]
    ParseIntegerError(
        #[source_code] &'input str,
        #[label("0")] Loc,
        ParseIntError,
        &'input str,
    ),
    #[error("fail to parse `{3}` to float: {2}")]
    #[diagnostic(code(ParseError::ParseFloatError))]
    ParseFloatError(
        #[source_code] &'input str,
        #[label("here")] Loc,
        ParseFloatError,
        &'input str,
    ),
}
