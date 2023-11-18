pub type Prog = Vec<CompileUnit>;

#[derive(Debug, Clone, PartialEq)]
pub enum CompileUnit {
    ConstDecl(ConstDecl),
    GlobalVarDecl(StaticVarDecl),
    ExternalFuncDecl(FuncDecl),
    FuncDecl(FuncDecl),
    FuncDef(FuncDef),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimType {
    Void,
    Bool,
    Char,
    Int,
    Float,
}

pub type BoolLiteral = bool;
pub type CharLiteral = u8;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;
pub type StringLiteral = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Null,
    Bool(BoolLiteral),
    Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    String(StringLiteral),
    List(Vec<Literal>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Prim(PrimType),
    Ptr(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub ty: Type,
    pub ident: String,
    pub dimension: Vec<usize>,
    pub init: Literal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StaticVarDecl {
    pub ty: Type,
    pub ident: String,
    pub dimension: Vec<usize>,
    pub init: Option<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub ty: Type,
    pub ident: String,
    pub dimension: Vec<usize>,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub ty: Type,
    pub ident: String,
    pub dimension: Vec<usize>,
}

pub type FuncParams = Vec<FuncParam>;
pub type FuncArgs = Vec<Expr>;
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub ty: Type,
    pub ident: String,
    pub params: FuncParams,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef {
    pub ty: Type,
    pub ident: String,
    pub params: FuncParams,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
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

#[derive(Debug, Clone, PartialEq)]
pub struct AssginStmt {
    lhs: LValue,
    rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfStmt {
    If(Expr, Block),
    IfElse(Expr, Block, Block),
    IfElseIf(Expr, Block, Box<IfStmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Prim(PrimExpr),
    Unary(Unary),
    Binary(Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimExpr {
    Ident(String),
    Literal(Literal),
    Paren(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
    BitNot,
    Deref,
    Ref,
    Idx(Box<Expr>),
    Call(FuncArgs),
    Cast(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Ident(String),
    ArrayAccess(Box<LValue>, Box<Expr>),
    PointerAccess(Box<LValue>),
}

pub type Block = Vec<Stmt>;
