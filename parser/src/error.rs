use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum UserError {
    #[error("fail to parse `{1}` to integer: {0}")]
    ParseIntegerError(std::num::ParseIntError, String),
    #[error("fail to parse `{1}` to float: {0}")]
    ParseFloatError(std::num::ParseFloatError, String),
}
