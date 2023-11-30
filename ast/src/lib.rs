pub type Loc = usize;

pub type Sourced<T = ()> = (Loc, T);

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a>(Vec<Sourced<Unit<'a>>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Unit<'a> {
    ConstDecl(Sourced<ConstDecl<'a>>),
    GlobalVarDecl(Sourced<GlobalVarDecl<'a>>),
    ExternalFuncDecl(Sourced<FuncProto<'a>>),
    FuncDecl(Sourced<FuncProto<'a>>),
    FuncDef(Sourced<FuncDef<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimType {
    Void,
    Bool,
    // Char,
    Int,
    Float,
}

pub type BoolLiteral = bool;
// pub type CharLiteral = u8;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;
// pub type StringLiteral = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    // Null,
    Bool(BoolLiteral),
    // Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    // String(StringLiteral),
    List(Vec<Sourced<Literal>>),
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            // AstLiteral_::Null => "null".to_owned(),
            Literal::Bool(val) => match val {
                true => "true".to_owned(),
                false => "false".to_owned(),
            },
            // AstLiteral_::Char(ch) => format!("'{}'", (*ch as char).escape_default()),
            Literal::Int(num) => num.to_string(),
            Literal::Float(num) => format!("{num:.1}"),
            // AstLiteral_::String(s) => format!("\"{}\"", s.escape_default()),
            Literal::List(list) => {
                format!(
                    "{{{}}}",
                    list.iter()
                        .map(|item| item.1.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Prim(Sourced<PrimType>),
    // Ptr(Box<AstType>),
    // 多维数组, 对于参数中不声明第一维那么数组的首个元素用usize::MAX占位
    Array(Sourced<PrimType>, Sourced<Vec<usize>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Sourced<Literal>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVarDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Option<Sourced<Literal>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Option<Sourced<Expr<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub dimension: Vec<usize>,
}
pub type FuncParams<'a> = Vec<Sourced<FuncParam<'a>>>;
pub type FuncArgs<'a> = Vec<Sourced<Expr<'a>>>;
#[derive(Debug, Clone, PartialEq)]
pub struct FuncProto<'a> {
    pub ty: Sourced<PrimType>,
    pub ident: Sourced<&'a str>,
    pub params: Sourced<FuncParams<'a>>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef<'a> {
    pub proto: Sourced<FuncProto<'a>>,
    pub body: Sourced<Block<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    ConstDecl(Sourced<ConstDecl<'a>>),
    VarDecl(Sourced<VarDecl<'a>>),
    Assign(Sourced<AssginStmt<'a>>),
    If(Sourced<IfStmt<'a>>),
    While(Sourced<WhileStmt<'a>>),
    Break(Sourced),
    Continue(Sourced),
    Return(Sourced<Option<Sourced<Expr<'a>>>>),
    Expr(Sourced<Expr<'a>>),
    Block(Sourced<Block<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssginStmt<'a> {
    lhs: Sourced<LValue<'a>>,
    rhs: Sourced<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfStmt<'a> {
    If(Sourced<Expr<'a>>, Sourced<Block<'a>>),
    IfElse(Sourced<Expr<'a>>, Sourced<Block<'a>>, Sourced<Block<'a>>),
    IfElseIf(
        Sourced<Expr<'a>>,
        Sourced<Block<'a>>,
        Box<Sourced<IfStmt<'a>>>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt<'a> {
    pub cond: Sourced<Expr<'a>>,
    pub body: Sourced<Block<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Prim(Sourced<PrimExpr<'a>>),
    Unary(Sourced<Unary<'a>>),
    Binary(Sourced<Binary<'a>>),
}

impl ToString for Expr<'_> {
    fn to_string(&self) -> String {
        match self {
            Expr::Prim((_, prim)) => prim.to_string(),
            Expr::Unary((_, una)) => una.to_string(),
            Expr::Binary((_, bin)) => bin.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimExpr<'a> {
    LValue(Sourced<LValue<'a>>),
    Literal(Sourced<Literal>),
    Paren(Box<Sourced<Expr<'a>>>),
    Call(Sourced<&'a str>, Sourced<FuncArgs<'a>>),
}

impl<'a> ToString for PrimExpr<'a> {
    fn to_string(&self) -> String {
        match self {
            PrimExpr::LValue((_, lval)) => lval.to_string(),
            PrimExpr::Literal((_, literal)) => literal.to_string(),
            PrimExpr::Paren(expr) => expr.1.to_string(),
            PrimExpr::Call(func, sourced_args) => {
                let arg = sourced_args
                    .1
                    .iter()
                    .map(|arg| arg.1.to_string())
                    .collect::<Vec<String>>();
                format!("{}({})", func.1, arg.join(", "))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
    BitNot,
    // Ref,
    // Deref,
    // Cast(AstType),
}

impl UnaryOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            UnaryOp::Pos => "+",
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub op: Sourced<UnaryOp>,
    pub rhs: Box<Sourced<Expr<'a>>>,
}

impl<'a> ToString for Unary<'a> {
    fn to_string(&self) -> String {
        format!("({}{})", self.op.1.to_str(), self.rhs.1.to_string())
    }
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

impl BinaryOp {
    pub fn to_str(&self) -> &'static str {
        match self {
            BinaryOp::LogicalOr => "||",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::BitAnd => "&",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Le => "<=",
            BinaryOp::Ge => ">=",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub op: Sourced<BinaryOp>,
    pub lhs: Box<Sourced<Expr<'a>>>,
    pub rhs: Box<Sourced<Expr<'a>>>,
}

impl<'a> ToString for Binary<'a> {
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.lhs.1.to_string(),
            self.op.1.to_str(),
            self.rhs.1.to_string()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue<'a> {
    Ident(Sourced<&'a str>),
    Idx(Sourced<&'a str>, Vec<Sourced<Expr<'a>>>),
    // Deref(Box<AstExpr>),
    // Paren(Box<AstLValue>),
}

impl<'a> ToString for LValue<'a> {
    fn to_string(&self) -> String {
        match self {
            LValue::Ident(ident) => ident.1.to_string(),
            LValue::Idx(arr, idxs) => idxs.iter().fold(arr.1.to_string(), |mut acc, idx| {
                acc.push_str(&format!("[{}]", idx.1.to_string()));
                acc
            }),
        }
    }
}

pub type Block<'a> = Vec<Sourced<Stmt<'a>>>;
