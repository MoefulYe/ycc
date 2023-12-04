use ast::Loc;
use miette::{Diagnostic, NamedSource};
use thiserror::Error;
pub type Result<T, E = CodeGenError> = std::result::Result<T, E>;

#[derive(Debug, Error, Diagnostic)]
pub enum CodeGenError {
    #[error("type mismatch expected `{expected}` but found `{found}`")]
    #[diagnostic(code(CodeGenError::TypeMismatch))]
    TypeMismatch {
        expected: &'static str,
        #[label("expected: `{expected}`")]
        expected_loc: Loc,
        found: &'static str,
        #[label("found: `{found}`")]
        found_loc: Loc,
    },
    #[error("void type is not allowed")]
    #[diagnostic(code(CodeGenError::IllegalVoid))]
    IllegalVoid {
        #[label("void type is not allowed")]
        loc: Loc,
    },
    #[error("array size must be positive")]
    #[diagnostic(code(CodeGenError::IllegalArraySize))]
    IllegalArraySize {
        #[label("array size must be positive")]
        loc: Loc,
    },
    //Excess elements in array initializer
    #[error("excess elements in array initializer")]
    #[diagnostic(code(CodeGenError::ExcessElementsInArrayInitializer))]
    ExcessElementsInArrayInitializer {
        #[label("excess elements in array initializer")]
        loc: Loc,
    },
}

#[derive(Error, Diagnostic, Debug)]
#[error("{err}")]
pub struct SourcedCodeGenError {
    #[diagnostic(transparent)]
    err: CodeGenError,
    #[source_code]
    src: NamedSource,
}
