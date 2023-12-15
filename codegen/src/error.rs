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
    #[error("excess elements in array initializer")]
    #[diagnostic(code(CodeGenError::ExcessElementsInArrayInitializer))]
    ExcessElementsInArrayInitializer {
        #[label("here")]
        loc: Loc,
    },
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
    #[error("coflicting declaration for `{ident}`")]
    #[diagnostic(code(CodeGenError::ConflictingDeclaration))]
    ConflictingDeclaration {
        #[label("here")]
        loc: Loc,
        ident: String,
    },
    #[error("verify function `{ident}` failed")]
    #[diagnostic(code(CodeGenError::VerifyFunction))]
    VerifyFunction {
        #[label("here")]
        loc: Loc,
        ident: String,
    },
    #[error("break outside loop")]
    #[diagnostic(code(CodeGenError::BreakOutsideLoop))]
    BreakOutsideLoop {
        #[label("here")]
        loc: Loc,
    },
    #[error("continue outside loop")]
    #[diagnostic(code(CodeGenError::ContinueOutsideLoop))]
    ContinueOutsideLoop {
        #[label("here")]
        loc: Loc,
    },
    #[error("illegal unary operator `{op}` for type `{ty}`")]
    #[diagnostic(code(CodeGenError::IllegalUnaryOperator))]
    IllegalUnaryOperator {
        #[label("`{op}` is illegal for type `{ty}`")]
        loc: Loc,
        op: String,
        ty: String,
    },
    #[error("illegal binary operator `{op}` for type `{lhs}` and `{rhs}`")]
    #[diagnostic(code(CodeGenError::IllegalBinaryOperator))]
    IllegalBinaryOperator {
        #[label("the type of lhs is `{lhs}`")]
        loc_lhs: Loc,
        #[label("the type of rhs is `{rhs}`")]
        loc_rhs: Loc,
        #[label("`{op}` is illegal for type `{lhs}` and `{rhs}`")]
        loc_op: Loc,
        op: String,
        lhs: String,
        rhs: String,
    },
    #[error("unresolved identifier `{ident}`")]
    #[diagnostic(code(CodeGenError::UnresolvedIdentifier))]
    UnresolvedIdentifier {
        #[label("`{ident}` is undefined before here")]
        loc: Loc,
        ident: String,
    },
}
