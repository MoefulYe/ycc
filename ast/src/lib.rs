use std::fmt::Display;

pub type Loc = usize;

pub type Sourced<T = ()> = (Loc, T);

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a>(pub Vec<Sourced<Unit<'a>>>);

impl<'a> Display for Module<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for unit in &self.0 {
            write!(f, "{}\n", unit.1)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unit<'a> {
    ConstDecl(Sourced<ConstDecl<'a>>),
    GlobalVarDecl(Sourced<GlobalVarDecl<'a>>),
    FuncDecl(Sourced<FuncProto<'a>>),
    FuncDef(Sourced<FuncDef<'a>>),
}

impl<'a> Display for Unit<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit::ConstDecl(unit) => write!(f, "{}", unit.1),
            Unit::GlobalVarDecl(unit) => write!(f, "{}", unit.1),
            Unit::FuncDecl(unit) => write!(f, "{};\n", unit.1),
            Unit::FuncDef(unit) => write!(f, "{}", unit.1),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimType {
    Void,
    Bool,
    Int,
    Float,
}

impl PrimType {
    pub fn as_str(&self) -> &'static str {
        match self {
            PrimType::Void => "void",
            PrimType::Bool => "bool",
            PrimType::Int => "int",
            PrimType::Float => "float",
        }
    }
}

impl Display for PrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PrimType::Void => "void",
                PrimType::Bool => "bool",
                PrimType::Int => "int",
                PrimType::Float => "float",
            }
        )
    }
}

pub type BoolLiteral = bool;
pub type IntLiteral = i32;
pub type FloatLiteral = f32;

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

impl Literal {
    pub fn type_str(&self) -> &'static str {
        match self {
            Literal::Bool(_) => "\"i1\"",
            Literal::Int(_) => "\"i32\"",
            Literal::Float(_) => "\"f32\"",
            Literal::List(_) => "\"arr\"",
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::Int(num) => write!(f, "{}", num),
            Literal::Float(num) => write!(f, "{num:.1}"),
            Literal::List(list) => {
                write!(
                    f,
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
    Array(Sourced<PrimType>, Sourced<Vec<i32>>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Prim(ty) => write!(f, "{}", ty.1),
            Type::Array(ty, dim) => {
                let dim = dim.1.iter().fold(String::new(), |mut acc, &item| {
                    if item == i32::MAX {
                        acc.push_str("[]");
                    } else {
                        acc.push_str(&format!("[{}]", item));
                    }
                    acc
                });
                write!(f, "{}{}", ty.1, dim)
            }
        }
    }
}

impl Type {
    pub fn as_str(&self) -> &'static str {
        match self {
            Type::Prim(ty) => ty.1.as_str(),
            Type::Array(ty, _) => match ty.1 {
                PrimType::Void => "arr of void",
                PrimType::Bool => "arr of i1",
                PrimType::Int => "arr of i32",
                PrimType::Float => "arr of f32",
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Sourced<Literal>,
}

impl<'a> Display for ConstDecl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty.1 {
            Type::Prim((_, ty)) => write!(f, "const {} {} = {};\n", ty, self.ident.1, self.init.1),
            Type::Array((_, ty), (_, dim)) => {
                let dim = dim.iter().fold(String::new(), |mut acc, &item| {
                    acc.push_str(&format!("[{}]", item));
                    acc
                });
                write!(
                    f,
                    "const {} {}{} = {};\n",
                    ty, self.ident.1, dim, self.init.1
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVarDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Option<Sourced<Literal>>,
}

impl<'a> Display for GlobalVarDecl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty.1 {
            Type::Prim((_, ty)) => {
                if let Some(init) = &self.init {
                    write!(f, "{} {} = {};\n", ty, self.ident.1, init.1)
                } else {
                    write!(f, "{} {};\n", ty, self.ident.1)
                }
            }
            Type::Array((_, ty), (_, dim)) => {
                let dim = dim.iter().fold(String::new(), |mut acc, item| {
                    acc.push_str(&format!("[{}]", item));
                    acc
                });
                if let Some(init) = &self.init {
                    write!(
                        f,
                        "{} {}{} = {};\n",
                        ty,
                        self.ident.1,
                        dim,
                        init.1.to_string()
                    )
                } else {
                    write!(f, "{} {}{};\n", ty, self.ident.1, dim)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
    pub init: Option<Sourced<Expr<'a>>>,
}

impl<'a> Display for VarDecl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty.1 {
            Type::Prim((_, ty)) => {
                if let Some(init) = &self.init {
                    write!(f, "{} {} = {};\n", ty, self.ident.1, init.1)
                } else {
                    write!(f, "{} {};\n", ty, self.ident.1)
                }
            }
            Type::Array((_, ty), (_, dim)) => {
                let dim = dim.iter().fold(String::new(), |mut acc, item| {
                    acc.push_str(&format!("[{}]", item));
                    acc
                });
                if let Some(init) = &self.init {
                    write!(
                        f,
                        "{} {}{} = {};\n",
                        ty,
                        self.ident.1,
                        dim,
                        init.1.to_string()
                    )
                } else {
                    write!(f, "{} {}{};\n", ty, self.ident.1, dim)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam<'a> {
    pub ty: Sourced<Type>,
    pub ident: Sourced<&'a str>,
}

impl<'a> Display for FuncParam<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty.1 {
            Type::Prim((_, ty)) => write!(f, "{} {}", ty, self.ident.1),
            Type::Array((_, ty), (_, dim)) => {
                let dim = dim.iter().fold(String::new(), |mut acc, &item| {
                    if item == i32::MAX {
                        acc.push_str("[]");
                    } else {
                        acc.push_str(&format!("[{}]", item));
                    }
                    acc
                });
                write!(f, "{} {}{}", ty, self.ident.1, dim)
            }
        }
    }
}
pub type FuncParams<'a> = Vec<Sourced<FuncParam<'a>>>;
pub type FuncArgs<'a> = Vec<Sourced<Expr<'a>>>;
#[derive(Debug, Clone, PartialEq)]
pub struct FuncProto<'a> {
    pub ty: Sourced<PrimType>,
    pub ident: Sourced<&'a str>,
    pub params: Sourced<FuncParams<'a>>,
}

impl<'a> Display for FuncProto<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .params
            .1
            .iter()
            .map(|param| param.1.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{} {}({})", self.ty.1, self.ident.1, params)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDef<'a> {
    pub proto: Sourced<FuncProto<'a>>,
    pub body: Sourced<Block<'a>>,
}

impl<'a> Display for FuncDef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{\n", self.proto.1)?;
        for stmt in &self.body.1 {
            write!(f, "{}", stmt.1)?;
        }
        write!(f, "}}\n")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    ConstDecl(Sourced<ConstDecl<'a>>),
    VarDecl(Sourced<VarDecl<'a>>),
    Assign(Sourced<AssignStmt<'a>>),
    If(Sourced<IfStmt<'a>>),
    While(Sourced<WhileStmt<'a>>),
    Break(Sourced),
    Continue(Sourced),
    Return(Sourced<Option<Sourced<Expr<'a>>>>),
    Expr(Sourced<Expr<'a>>),
    Block(Sourced<Block<'a>>),
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::ConstDecl(stmt) => write!(f, "{}", stmt.1),
            Stmt::VarDecl(stmt) => write!(f, "{}", stmt.1),
            Stmt::Assign(stmt) => write!(f, "{}", stmt.1),
            Stmt::If(stmt) => write!(f, "{}", stmt.1),
            Stmt::While(stmt) => write!(f, "{}", stmt.1),
            Stmt::Break(_) => write!(f, "break;\n"),
            Stmt::Continue(_) => write!(f, "continue;\n"),
            Stmt::Return(stmt) => {
                if let Some(expr) = &stmt.1 {
                    write!(f, "return {};\n", expr.1)
                } else {
                    write!(f, "return;\n")
                }
            }
            Stmt::Expr(stmt) => write!(f, "{};\n", stmt.1),
            Stmt::Block(stmt) => {
                write!(f, "{{\n")?;
                for stmt in &stmt.1 {
                    write!(f, "{}", stmt.1)?;
                }
                write!(f, "}}\n")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt<'a> {
    pub lhs: Sourced<LValue<'a>>,
    pub rhs: Sourced<Expr<'a>>,
}

impl<'a> Display for AssignStmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {};\n", self.lhs.1, self.rhs.1)
    }
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

impl<'a> Display for IfStmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IfStmt::If(cond, body) => {
                write!(f, "if ({}) {{\n", cond.1)?;
                for stmt in &body.1 {
                    write!(f, "{}", stmt.1)?;
                }
                write!(f, "}}\n")
            }
            IfStmt::IfElse(cond, body, else_body) => {
                write!(f, "if ({}) {{\n", cond.1)?;
                for stmt in &body.1 {
                    write!(f, "{}", stmt.1)?;
                }
                write!(f, "}} else {{\n")?;
                for stmt in &else_body.1 {
                    write!(f, "{}", stmt.1)?;
                }
                write!(f, "}}\n")
            }
            IfStmt::IfElseIf(cond, body, else_if) => {
                write!(f, "if ({}) {{\n", cond.1)?;
                for stmt in &body.1 {
                    write!(f, "{}", stmt.1)?;
                }
                write!(f, "}} else ")?;
                write!(f, "{}", else_if.1)?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt<'a> {
    pub cond: Sourced<Expr<'a>>,
    pub body: Sourced<Block<'a>>,
}

impl<'a> Display for WhileStmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {{\n", self.cond.1)?;
        for stmt in &self.body.1 {
            write!(f, "{}", stmt.1)?;
        }
        write!(f, "}}\n")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Prim(Sourced<PrimExpr<'a>>),
    Unary(Sourced<Unary<'a>>),
    Binary(Sourced<Binary<'a>>),
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Prim(expr) => write!(f, "{}", expr.1),
            Expr::Unary(expr) => write!(f, "{}", expr.1),
            Expr::Binary(expr) => write!(f, "{}", expr.1),
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

impl<'a> Display for PrimExpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimExpr::LValue(expr) => write!(f, "{}", expr.1),
            PrimExpr::Literal(expr) => write!(f, "{}", expr.1),
            PrimExpr::Paren(expr) => write!(f, "({})", expr.1),
            PrimExpr::Call(func, args) => {
                let args = args
                    .1
                    .iter()
                    .map(|arg| arg.1.to_string())
                    .collect::<Vec<String>>();
                write!(f, "{}({})", func.1, args.join(", "))
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

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Pos => write!(f, "+"),
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub op: Sourced<UnaryOp>,
    pub rhs: Box<Sourced<Expr<'a>>>,
}

impl<'a> Display for Unary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.op.1, self.rhs.1)
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Ne => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub op: Sourced<BinaryOp>,
    pub lhs: Box<Sourced<Expr<'a>>>,
    pub rhs: Box<Sourced<Expr<'a>>>,
}

impl<'a> Display for Binary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs.1, self.op.1, self.rhs.1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue<'a> {
    Ident(Sourced<&'a str>),
    Idx(Sourced<&'a str>, Vec<Sourced<Expr<'a>>>),
}

impl<'a> Display for LValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LValue::Ident(ident) => write!(f, "{}", ident.1),
            LValue::Idx(ident, idx) => {
                let idx = idx.iter().fold("".to_owned(), |mut acc, item| {
                    acc.push_str(&format!("[{}]", item.1));
                    acc
                });
                write!(f, "{}{}", ident.1, idx)
            }
        }
    }
}

pub type Block<'a> = Vec<Sourced<Stmt<'a>>>;
