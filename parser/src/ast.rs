pub type Loc = usize;

pub type Sourced<T> = (Loc, T);

#[derive(Debug, Clone, PartialEq)]
pub struct AstModule_(Vec<AstCompileUnit>);
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
    // Char,
    Int,
    Float,
}

pub type AstPrimType = Sourced<AstPrimType_>;

pub type BoolLiteral = bool;
// pub type CharLiteral = u8;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;
// pub type StringLiteral = String;

#[derive(Debug, Clone, PartialEq)]
pub enum AstLiteral_ {
    // Null,
    Bool(BoolLiteral),
    // Char(CharLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    // String(StringLiteral),
    List(Vec<AstLiteral>),
}

impl ToString for AstLiteral_ {
    fn to_string(&self) -> String {
        match self {
            // AstLiteral_::Null => "null".to_owned(),
            AstLiteral_::Bool(val) => match val {
                true => "true".to_owned(),
                false => "false".to_owned(),
            },
            // AstLiteral_::Char(ch) => format!("'{}'", (*ch as char).escape_default()),
            AstLiteral_::Int(num) => num.to_string(),
            AstLiteral_::Float(num) => num.to_string(),
            // AstLiteral_::String(s) => format!("\"{}\"", s.escape_default()),
            AstLiteral_::List(list) => {
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

pub type AstLiteral = Sourced<AstLiteral_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstType_ {
    Prim(AstPrimType),
    // Ptr(Box<AstType>),
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

impl ToString for AstExpr_ {
    fn to_string(&self) -> String {
        match self {
            AstExpr_::Prim((_, prim)) => prim.to_string(),
            AstExpr_::Unary((_, una)) => una.to_string(),
            AstExpr_::Binary((_, bin)) => bin.to_string(),
        }
    }
}

pub type AstExpr = Sourced<AstExpr_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstPrimExpr_ {
    LVal(AstLValue),
    Literal(AstLiteral),
    Paren(Box<AstExpr>),
    Call(String, AstFuncArgs),
}

impl ToString for AstPrimExpr_ {
    fn to_string(&self) -> String {
        match self {
            AstPrimExpr_::LVal((_, lval)) => lval.to_string(),
            AstPrimExpr_::Literal((_, literal)) => literal.to_string(),
            AstPrimExpr_::Paren(expr) => expr.1.to_string(),
            AstPrimExpr_::Call(func, sourced_args) => {
                let arg = sourced_args
                    .1
                    .iter()
                    .map(|arg| arg.1.to_string())
                    .collect::<Vec<String>>();
                format!("{}({})", func, arg.join(", "))
            }
        }
    }
}

pub type AstPrimExpr = Sourced<AstPrimExpr_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstUnaryOp_ {
    Pos,
    Neg,
    Not,
    BitNot,
    // Ref,
    // Deref,
    // Cast(AstType),
}

impl ToString for AstUnaryOp_ {
    fn to_string(&self) -> String {
        match self {
            AstUnaryOp_::Pos => "+".to_owned(),
            AstUnaryOp_::Neg => "-".to_owned(),
            AstUnaryOp_::Not => "!".to_owned(),
            AstUnaryOp_::BitNot => "~".to_owned(),
        }
    }
}

pub type AstUnaryOp = Sourced<AstUnaryOp_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstUnary_ {
    pub op: AstUnaryOp,
    pub expr: Box<AstExpr>,
}

impl ToString for AstUnary_ {
    fn to_string(&self) -> String {
        format!("({}{})", self.op.1.to_string(), self.expr.1.to_string())
    }
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

impl ToString for AstBinaryOp_ {
    fn to_string(&self) -> String {
        match self {
            AstBinaryOp_::LogicalOr => "||".to_owned(),
            AstBinaryOp_::LogicalAnd => "&&".to_owned(),
            AstBinaryOp_::BitOr => "|".to_owned(),
            AstBinaryOp_::BitXor => "^".to_owned(),
            AstBinaryOp_::BitAnd => "&".to_owned(),
            AstBinaryOp_::Eq => "==".to_owned(),
            AstBinaryOp_::Ne => "!=".to_owned(),
            AstBinaryOp_::Lt => "<".to_owned(),
            AstBinaryOp_::Gt => ">".to_owned(),
            AstBinaryOp_::Le => "<=".to_owned(),
            AstBinaryOp_::Ge => ">=".to_owned(),
            AstBinaryOp_::Shl => "<<".to_owned(),
            AstBinaryOp_::Shr => ">>".to_owned(),
            AstBinaryOp_::Add => "+".to_owned(),
            AstBinaryOp_::Sub => "-".to_owned(),
            AstBinaryOp_::Mul => "*".to_owned(),
            AstBinaryOp_::Div => "/".to_owned(),
            AstBinaryOp_::Mod => "%".to_owned(),
        }
    }
}

pub type AstBinaryOp = Sourced<AstBinaryOp_>;

#[derive(Debug, Clone, PartialEq)]
pub struct AstBinary_ {
    pub op: AstBinaryOp,
    pub lhs: Box<AstExpr>,
    pub rhs: Box<AstExpr>,
}

impl ToString for AstBinary_ {
    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.lhs.1.to_string(),
            self.op.1.to_string(),
            self.rhs.1.to_string()
        )
    }
}

pub type AstBinary = Sourced<AstBinary_>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstLValue_ {
    Ident(String),
    Idx(String, Vec<AstExpr>),
    // Deref(Box<AstExpr>),
    // Paren(Box<AstLValue>),
}

impl ToString for AstLValue_ {
    fn to_string(&self) -> String {
        match self {
            AstLValue_::Ident(ident) => ident.to_owned(),
            AstLValue_::Idx(arr, idxs) => idxs.iter().fold(arr.to_owned(), |mut acc, idx| {
                acc.push_str(&format!("[{}]", idx.1.to_string()));
                acc
            }),
            // AstLValue_::Deref(expr) => {
            //     format!("*{}", expr.1.to_string())
            // }
            // AstLValue_::Paren(inner) => {
            //     format!("({})", inner.1.to_string())
            // }
        }
    }
}

pub type AstLValue = Sourced<AstLValue_>;

pub type AstBlock_ = Vec<AstStmt>;
pub type AstBlock = Sourced<AstBlock_>;
