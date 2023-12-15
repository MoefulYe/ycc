use ast::Loc;
use miette::Diagnostic;
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
        #[label("here")]
        loc: Loc,
    },
    #[error("array size must be positive")]
    #[diagnostic(code(CodeGenError::IllegalArraySize))]
    IllegalArraySize {
        #[label("here")]
        loc: Loc,
    },
    //Excess elements in array initializer
    #[error("excess elements in array initializer")]
    #[diagnostic(code(CodeGenError::ExcessElementsInArrayInitializer))]
    ExcessElementsInArrayInitializer {
        #[label("here")]
        loc: Loc,
    },
    //重复的标识符
    #[error("duplicate identifier `{ident}`")]
    #[diagnostic(code(CodeGenError::DuplicateIdentifier))]
    DuplicateIdentifier {
        #[label("here")]
        loc: Loc,
        ident: String,
    },
    #[error("redefined function `{ident}`")]
    #[diagnostic(code(CodeGenError::RedefinedFunction))]
    RedefinedFunction {
        #[label("here")]
        loc: Loc,
        ident: String,
    },
}
