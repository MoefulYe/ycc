pub type Loc = usize;

pub type Sourced<T> = (Loc, T);

pub type _Module = Vec<CompileUnit>;
pub type Module = Sourced<_Module>;

#[derive(Debug, Clone, PartialEq)]
pub enum _CompileUnit {
    ConstDecl(ConstDecl),
    GlobalVarDecl(StaticVarDecl),
    ExternalFuncDecl(FuncProto),
    FuncDecl(FuncProto),
    FuncDef(FuncDef),
}

pub type CompileUnit = Sourced<_CompileUnit>;

#[derive(Debug, Clone, PartialEq)]
pub enum _PrimType {
    Void,
    Bool,
    Char,
    Int,
    Float,
}

pub type PrimType = Sourced<_PrimType>;

pub type BoolLiteral = bool;
pub type CharLiteral = u8;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;
pub type StringLiteral = String;

#[derive(Debug, Clone, PartialEq)]
pub enum _Literal {
    Null,
    Bool(BoolLiteral),
    Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    String(StringLiteral),
    List(Vec<Literal>),
}

pub type Literal = Sourced<_Literal>;

#[derive(Debug, Clone, PartialEq)]
pub enum _Type {
    Prim(PrimType),
    Ptr(Box<Type>),
    // 多维数组, 对于参数中不声明第一维那么数组的首个元素用usize::MAX占位
    Array(Box<Type>, Vec<usize>),
}

pub type Type = Sourced<_Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct _ConstDecl {
    pub ty: Type,
    pub ident: String,
    pub init: Literal,
}

pub type ConstDecl = Sourced<_ConstDecl>;

#[derive(Debug, Clone, PartialEq)]
pub struct _StaticVarDecl {
    pub ty: Type,
    pub ident: String,
    pub init: Option<Literal>,
}

pub type StaticVarDecl = Sourced<_StaticVarDecl>;

#[derive(Debug, Clone, PartialEq)]
pub struct _VarDecl {
    pub ty: Type,
    pub ident: String,
    pub init: Option<Expr>,
}

pub type VarDecl = Sourced<_VarDecl>;

#[derive(Debug, Clone, PartialEq)]
pub struct _FuncParam {
    pub ty: Type,
    pub ident: String,
    pub dimension: Vec<usize>,
}
pub type FuncParam = Sourced<_FuncParam>;
pub type _FuncParams = Vec<FuncParam>;
pub type FuncParams = Sourced<_FuncParams>;
pub type _FuncArgs = Vec<Expr>;
pub type FuncArgs = Sourced<_FuncArgs>;
#[derive(Debug, Clone, PartialEq)]
pub struct _FuncProto {
    pub ty: Type,
    pub ident: String,
    pub params: FuncParams,
}
pub type FuncProto = Sourced<_FuncProto>;

#[derive(Debug, Clone, PartialEq)]
pub struct _FuncDef {
    pub proto: FuncProto,
    pub body: Block,
}

pub type FuncDef = Sourced<_FuncDef>;

#[derive(Debug, Clone, PartialEq)]
pub enum _Stmt {
    ConstDecl(ConstDecl),
    StaticVarDecl(StaticVarDecl),
    VarDecl(VarDecl),
    Assign(AssginStmt),
    If(IfStmt),
    While(WhileStmt),
    Break,
    Continue,
    Return(Option<Expr>),
    Expr(Expr),
    Block(Block),
}

pub type Stmt = Sourced<_Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub struct _AssginStmt {
    lhs: LValue,
    rhs: Expr,
}

pub type AssginStmt = Sourced<_AssginStmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum _IfStmt {
    If {
        cond: Expr,
        then: Block,
    },
    IfElse {
        cond: Expr,
        then: Block,
        else_: Block,
    },
    IfElseIf {
        cond: Expr,
        then: Block,
        else_: Box<IfStmt>,
    },
}

pub type IfStmt = Sourced<_IfStmt>;

#[derive(Debug, Clone, PartialEq)]
pub struct _WhileStmt {
    pub cond: Expr,
    pub body: Block,
}

pub type WhileStmt = Sourced<_WhileStmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum _Expr {
    Prim(PrimExpr),
    Unary(Unary),
    Binary(Binary),
}

pub type Expr = Sourced<_Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum _PrimExpr {
    Ident(String),
    Literal(Literal),
    Paren(Box<Expr>),
    Idx(Box<Expr>),
    Call(FuncArgs),
}

pub type PrimExpr = Sourced<_PrimExpr>;

#[derive(Debug, Clone, PartialEq)]
pub enum _UnaryOp {
    Pos,
    Neg,
    Not,
    BitNot,
    Deref,
    Ref,
    Cast(Type),
}

pub type UnaryOp = Sourced<_UnaryOp>;

#[derive(Debug, Clone, PartialEq)]
pub struct _Unary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

pub type Unary = Sourced<_Unary>;

#[derive(Debug, Clone, PartialEq)]
pub enum _BinaryOp {
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

pub type BinaryOp = Sourced<_BinaryOp>;

#[derive(Debug, Clone, PartialEq)]
pub struct _Binary {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

pub type Binary = Sourced<_Binary>;

#[derive(Debug, Clone, PartialEq)]
pub enum _LValue {
    Ident(String),
    ArrayAccess(Box<LValue>, Box<Expr>),
    PointerAccess(Box<LValue>),
}

pub type LValue = Sourced<_LValue>;

pub type _Block = Vec<Stmt>;
pub type Block = Sourced<_Block>;
