use crate::utils::rand;
use graphviz_rust::dot_generator::{attr, edge, graph, id, node_id, stmt};
use graphviz_rust::dot_structures::{Attribute, Edge, EdgeTy, Id, Node, NodeId, Stmt, Vertex};
use graphviz_rust::{dot_generator::node, dot_structures::Graph};
use std::fmt::Display;

pub fn draw(module: &ast::Module) -> Graph {
    let mut g = graph!(strict di id!("module");
        attr!("rankdir", "LR")
    );
    module.draw(&mut g);
    g
}

trait Drawer {
    type Out;
    fn draw(&self, g: &mut Graph) -> Self::Out;
}

trait Leaf: Display {}

impl<T> Drawer for T
where
    T: Leaf,
{
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        this
    }
}

impl Drawer for ast::Module<'_> {
    type Out = ();

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        g.add_stmt(node!(this; attr!("label", "module")).into());
        for (_, unit) in &self.0 {
            let (ty, child) = unit.draw(g);
            g.add_stmt(edge!(node_id!(this) => node_id!(child); attr!("label", ty)).into());
        }
    }
}

impl Drawer for ast::Unit<'_> {
    type Out = (&'static str, String);

    fn draw(&self, g: &mut Graph) -> Self::Out {
        match self {
            ast::Unit::ConstDecl((_, unit)) => ("\"const-decl\"", unit.draw(g)),
            ast::Unit::GlobalVarDecl((_, unit)) => ("\"global-var-decl\"", unit.draw(g)),
            ast::Unit::FuncDecl((_, unit)) => ("\"func-decl\"", unit.draw(g)),
            ast::Unit::FuncDef((_, unit)) => ("\"func-def\"", unit.draw(g)),
        }
    }
}

impl Drawer for ast::ConstDecl<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        let ty = self.ty.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ty); attr!("label", "\"type\"")).into());
        let ident = self.ident.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ident); attr!("label", "\"ident\"")).into());
        let init = self.init.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(init); attr!("label", "\"init\"")).into());
        this
    }
}

impl Leaf for ast::PrimType {}
impl Leaf for ast::Type {}
impl Leaf for &'_ str {}

impl Drawer for ast::Literal {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = match self {
            ast::Literal::Bool(lit) => match lit {
                true => "\"true\"".to_owned(),
                false => "\"false\"".to_owned(),
            },
            ast::Literal::Int(lit) => format!("\"{}\"", lit),
            ast::Literal::Float(lit) => format!("\"{:.1}\"", lit),
            ast::Literal::List(list) => {
                list.iter().for_each(|(_, item)| {
                    let item = item.draw(g);
                    g.add_stmt(edge!(node_id!(this) => node_id!(item)).into());
                });
                "\"{...}\"".to_owned()
            }
        };
        g.add_stmt(node!(this; attr!("label", label)).into());
        this
    }
}

impl Drawer for ast::GlobalVarDecl<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        let ty = self.ty.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ty); attr!("label", "\"type\"")).into());
        let ident = self.ident.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ident); attr!("label", "\"ident\"")).into());
        if let Some((_, init)) = &self.init {
            let init = init.draw(g);
            g.add_stmt(edge!(node_id!(this) => node_id!(init); attr!("label", "\"init\"")).into());
        }
        this
    }
}

impl Drawer for ast::FuncProto<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{};\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        let ty = self.ty.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ty); attr!("label", "\"type\"")).into());
        let ident = self.ident.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ident); attr!("label", "\"ident\"")).into());
        self.params.1.iter().for_each(|(_, param)| {
            let param = param.draw(g);
            g.add_stmt(edge!(node_id!(this) => node_id!(param); attr!("label", "param")).into());
        });
        this
    }
}

impl Drawer for ast::FuncDef<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = self.proto.1.draw(g);
        let body = self.body.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(body); attr!("label", "\"body\"")).into());
        this
    }
}

impl Leaf for ast::FuncParam<'_> {}

impl Drawer for ast::Block<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = "\"{...}\"".to_owned();
        g.add_stmt(node!(this; attr!("label", label)).into());
        self.iter().for_each(|(_, stmt)| {
            let (ty, stmt) = stmt.draw(g);
            g.add_stmt(edge!(node_id!(this) => node_id!(stmt); attr!("label", ty)).into());
        });
        this
    }
}

impl Drawer for ast::Stmt<'_> {
    type Out = (&'static str, String);

    fn draw(&self, g: &mut Graph) -> Self::Out {
        match self {
            ast::Stmt::ConstDecl(stmt) => ("\"const-decl\"", stmt.1.draw(g)),
            ast::Stmt::VarDecl(stmt) => ("\"var-decl\"", stmt.1.draw(g)),
            ast::Stmt::Assign(stmt) => ("\"assign\"", stmt.1.draw(g)),
            ast::Stmt::If(stmt) => ("\"if\"", stmt.1.draw(g)),
            ast::Stmt::While(stmt) => ("\"while\"", stmt.1.draw(g)),
            ast::Stmt::Break(_) => {
                let this = rand::id();
                let label = "\"break\"".to_owned();
                g.add_stmt(node!(this; attr!("label", label)).into());
                ("\"break\"", this)
            }
            ast::Stmt::Continue(_) => {
                let this = rand::id();
                let label = "\"continue\"".to_owned();
                g.add_stmt(node!(this; attr!("label", label)).into());
                ("\"continue\"", this)
            }
            ast::Stmt::Return((_, may_null)) => match may_null {
                Some((_, expr)) => {
                    let this = rand::id();
                    let label = format!("\"return {};\"", expr.to_string().escape_default());
                    g.add_stmt(node!(this; attr!("label", label)).into());
                    let expr = expr.draw(g);
                    g.add_stmt(
                        edge!(node_id!(this) => node_id!(expr); attr!("label", "\"expr\"")).into(),
                    );
                    ("\"return;\"", this)
                }
                None => {
                    let this = rand::id();
                    let label = "\"return\"".to_owned();
                    g.add_stmt(node!(this; attr!("label", label)).into());
                    ("\"return\"", this)
                }
            },
            ast::Stmt::Expr((_, stmt)) => ("\"expr\"", stmt.draw(g)),
            ast::Stmt::Block((_, block)) => ("\"block\"", block.draw(g)),
        }
    }
}

impl Drawer for ast::VarDecl<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        let ty = self.ty.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ty); attr!("label", "\"type\"")).into());
        let ident = self.ident.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(ident); attr!("label", "\"ident\"")).into());
        if let Some((_, init)) = &self.init {
            let init = init.draw(g);
            g.add_stmt(edge!(node_id!(this) => node_id!(init); attr!("label", "\"init\"")).into());
        }
        this
    }
}

impl Drawer for ast::AssignStmt<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        g.add_stmt(node!(this; attr!("label", label)).into());
        let lhs = self.lhs.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(lhs); attr!("label", "\"lhs\"")).into());
        let rhs = self.rhs.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(rhs); attr!("label", "\"rhs\"")).into());
        this
    }
}

impl Drawer for ast::IfStmt<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = "\"if\"".to_owned();
        g.add_stmt(node!(this; attr!("label", label)).into());
        match self {
            ast::IfStmt::If((_, cond), (_, then)) => {
                let cond = cond.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(cond); attr!("label", "\"cond\"")).into(),
                );
                let then = then.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(then); attr!("label", "\"then\"")).into(),
                );
            }
            ast::IfStmt::IfElse((_, cond), (_, then), (_, alt)) => {
                let cond = cond.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(cond); attr!("label", "\"cond\"")).into(),
                );
                let then = then.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(then); attr!("label", "\"then\"")).into(),
                );
                let alt = alt.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(alt); attr!("label", "\"else\"")).into(),
                );
            }
            ast::IfStmt::IfElseIf((_, cond), (_, then), alt) => {
                let cond = cond.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(cond); attr!("label", "\"cond\"")).into(),
                );
                let then = then.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(then); attr!("label", "\"then\"")).into(),
                );
                let alt = alt.1.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(alt); attr!("label", "\"else\"")).into(),
                );
            }
        }
        this
    }
}

impl Drawer for ast::WhileStmt<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = "\"while\"".to_owned();
        g.add_stmt(node!(this; attr!("label", label)).into());
        let cond = self.cond.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(cond); attr!("label", "\"cond\"")).into());
        let body = self.body.1.draw(g);
        g.add_stmt(edge!(node_id!(this) => node_id!(body); attr!("label", "\"body\"")).into());
        this
    }
}

impl Drawer for ast::Expr<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        match self {
            ast::Expr::Prim((_, expr)) => expr.draw(g),
            ast::Expr::Unary((_, expr)) => {
                let this = rand::id();
                let label = format!("\"{}\"", expr.op.1);
                g.add_stmt(node!(this; attr!("label", label)).into());
                let rhs = expr.rhs.1.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(rhs); attr!("label", "\"rhs\"")).into(),
                );
                this
            }
            ast::Expr::Binary((_, expr)) => {
                let this = rand::id();
                let label = format!("\"{}\"", expr.op.1);
                g.add_stmt(node!(this; attr!("label", label)).into());
                let lhs = expr.lhs.1.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(lhs); attr!("label", "\"lhs\"")).into(),
                );
                let rhs = expr.rhs.1.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(rhs); attr!("label", "\"rhs\"")).into(),
                );
                this
            }
        }
    }
}

impl Leaf for ast::LValue<'_> {}

impl Drawer for ast::PrimExpr<'_> {
    type Out = String;

    fn draw(&self, g: &mut Graph) -> Self::Out {
        match self {
            ast::PrimExpr::LValue((_, expr)) => expr.draw(g),
            ast::PrimExpr::Literal((_, expr)) => expr.draw(g),
            ast::PrimExpr::Paren(expr) => {
                let this = rand::id();
                let label = "\"(...)\"".to_owned();
                g.add_stmt(node!(this; attr!("label", label)).into());
                let expr = expr.1.draw(g);
                g.add_stmt(
                    edge!(node_id!(this) => node_id!(expr); attr!("label", "\"expr\"")).into(),
                );
                this
            }
            ast::PrimExpr::Call((_, func), (_, args)) => {
                let this = rand::id();
                let label = format!("\"{}(...)\"", func);
                g.add_stmt(node!(this; attr!("label", label)).into());
                args.iter().for_each(|(_, arg)| {
                    let arg = arg.draw(g);
                    g.add_stmt(
                        edge!(node_id!(this) => node_id!(arg); attr!("label", "\"arg\"")).into(),
                    );
                });
                this
            }
        }
    }
}
