use ast::Loc;
use std::num::{ParseFloatError, ParseIntError};
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum UserError<'input> {
    #[error("fail to parse `{2}` to integer: {1}")]
    ParseIntegerError(Loc, ParseIntError, &'input str),
    #[error("fail to parse `{2}` to float: {1}")]
    ParseFloatError(Loc, ParseFloatError, &'input str),
}
