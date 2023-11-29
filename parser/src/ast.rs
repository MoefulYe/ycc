pub type Loc = usize;

pub type Sourced<T> = (Loc, T);

pub type AstModule_ = Vec<AstCompileUnit>;
pub type AstModule = Sourced<AstModule_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstCompileUnit_ {
    ConstDecl(AstConstDecl),
    GlobalVarDecl(AstStaticVarDecl),
    ExternalFuncDecl(AstFuncProto),
    FuncDecl(AstFuncProto),
    FuncDef(AstFuncDef),
}

pub type AstCompileUnit = Sourced<AstCompileUnit_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstPrimType_ {
    Void,
    Bool,
    Char,
    Int,
    Float,
}

pub type AstPrimType = Sourced<AstPrimType_>;

pub type BoolLiteral = bool;
pub type CharLiteral = u8;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;
pub type StringLiteral = String;

#[derive(Debug, Clone, PartialEq)]
pub enum AstLiteral_ {
    Null,
    Bool(BoolLiteral),
    Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    String(StringLiteral),
    List(Vec<AstLiteral>),
}

pub type AstLiteral = Sourced<AstLiteral_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstType_ {
    Prim(AstPrimType),
    Ptr(Box<AstType>),
    // 多维数组, 对于参数中不声明第一维那么数组的首个元素用usize::MAX占位
    Array(Box<AstType>, Vec<usize>),
}

pub type AstType = Sourced<AstType_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstConstDecl_ {
    pub ty: AstType,
    pub ident: String,
    pub init: AstLiteral,
}

pub type AstConstDecl = Sourced<AstConstDecl_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstStaticVarDecl_ {
    pub ty: AstType,
    pub ident: String,
    pub init: Option<AstLiteral>,
}

pub type AstStaticVarDecl = Sourced<AstStaticVarDecl_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstVarDecl_ {
    pub ty: AstType,
    pub ident: String,
    pub init: Option<AstExpr>,
}

pub type AstVarDecl = Sourced<AstVarDecl_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstFuncParam_ {
    pub ty: AstType,
    pub ident: String,
    pub dimension: Vec<usize>,
}
pub type AstFuncParam = Sourced<AstFuncParam_>;
pub type AstFuncParams_ = Vec<AstFuncParam>;
pub type AstFuncParams = Sourced<AstFuncParams_>;
pub type AstFuncArgs_ = Vec<AstExpr>;
pub type AstFuncArgs = Sourced<AstFuncArgs_>;
#[derive(Debug, Clone, PartialEq)]
pub struct AstFuncProto_ {
    pub ty: AstType,
    pub ident: String,
    pub params: AstFuncParams,
}
pub type AstFuncProto = Sourced<AstFuncProto_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstFuncDef_ {
    pub proto: AstFuncProto,
    pub body: AstBlock,
}

pub type AstFuncDef = Sourced<AstFuncDef_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstStmt_ {
    ConstDecl(AstConstDecl),
    StaticVarDecl(AstStaticVarDecl),
    VarDecl(AstVarDecl),
    Assign(AstAssginStmt),
    If(AstIfStmt),
    While(AstWhileStmt),
    Break,
    Continue,
    Return(Option<AstExpr>),
    Expr(AstExpr),
    Block(AstBlock),
}

pub type AstStmt = Sourced<AstStmt_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstAssginStmt_ {
    lhs: AstLValue,
    rhs: AstExpr,
}

pub type AstAssginStmt = Sourced<AstAssginStmt_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstIfStmt_ {
    If {
        cond: AstExpr,
        then: AstBlock,
    },
    IfElse {
        cond: AstExpr,
        then: AstBlock,
        else_: AstBlock,
    },
    IfElseIf {
        cond: AstExpr,
        then: AstBlock,
        else_: Box<AstIfStmt>,
    },
}

pub type AstIfStmt = Sourced<AstIfStmt_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstWhileStmt_ {
    pub cond: AstExpr,
    pub body: AstBlock,
}

pub type AstWhileStmt = Sourced<AstWhileStmt_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpr_ {
    Prim(AstPrimExpr),
    Unary(AstUnary),
    Binary(AstBinary),
}

pub type AstExpr = Sourced<AstExpr_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstPrimExpr_ {
    Ident(String),
    Literal(AstLiteral),
    Paren(Box<AstExpr>),
    Idx(Box<AstExpr>),
    Call(AstFuncArgs),
}

pub type AstPrimExpr = Sourced<AstPrimExpr_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstUnaryOp_ {
    Pos,
    Neg,
    Not,
    BitNot,
    Deref,
    Ref,
    Cast(AstType),
}

pub type AstUnaryOp = Sourced<AstUnaryOp_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstUnary_ {
    pub op: AstUnaryOp,
    pub expr: Box<AstExpr>,
}

pub type AstUnary = Sourced<AstUnary_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstBinaryOp_ {
    LogicalOr,
    LogicalAnd,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub type AstBinaryOp = Sourced<AstBinaryOp_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstBinary_ {
    pub op: AstBinaryOp,
    pub lhs: Box<AstExpr>,
    pub rhs: Box<AstExpr>,
}

pub type AstBinary = Sourced<AstBinary_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstLValue_ {
    Ident(String),
    ArrayAccess(Box<AstExpr>, Box<AstExpr>),
    PointerAccess(Box<AstExpr>),
}

pub type AstLValue = Sourced<AstLValue_>;

pub type AstBlock_ = Vec<AstStmt>;
pub type AstBlock = Sourced<AstBlock_>;
