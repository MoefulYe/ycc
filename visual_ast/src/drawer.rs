use graphviz_rust::dot_generator::{attr, edge, graph, id, node_id};
use graphviz_rust::dot_structures::{Attribute, Edge, EdgeTy, Id, Node, NodeId, Vertex};
use graphviz_rust::{dot_generator::node, dot_structures::Graph};

use crate::utils::rand;

pub fn draw(module: &ast::Module) -> Graph {
    let mut g = graph!(strict di id!("module"));
    module.draw("module", &mut g);
    g
}

trait Drawer {
    type Out;
    fn draw(&self, parent: &str, g: &mut Graph) -> Self::Out;
}

impl Drawer for ast::Module<'_> {
    type Out = ();

    fn draw(&self, _: &str, g: &mut Graph) -> Self::Out {
        let id = rand::id();
        let node = node!(id; attr!("label", "module"));
        g.add_stmt(node.into());
        self.0.iter().for_each(|(_, unit)| unit.draw(&id, g));
    }
}

impl Drawer for ast::Unit<'_> {
    type Out = ();

    fn draw(&self, parent: &str, g: &mut Graph) -> Self::Out {
        let node = match self {
            ast::Unit::ConstDecl((_, const_decl)) => const_decl.draw(parent, g),
            ast::Unit::GlobalVarDecl((_, global_decl)) => global_decl.draw(parent, g),
            ast::Unit::FuncDecl((_, func_decl)) => func_decl.draw(parent, g),
            ast::Unit::FuncDef((_, func_def)) => func_def.draw(parent, g),
        };
        let this = node.id.clone();
        g.add_stmt(node.into());
        g.add_stmt(edge!(node_id!(parent) => this).into());
    }
}

impl Drawer for ast::ConstDecl<'_> {
    type Out = Node;

    fn draw(&self, _: &str, _: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        let node = node!(this; attr!("label", label));
        node
    }
}

impl Drawer for ast::GlobalVarDecl<'_> {
    type Out = Node;

    fn draw(&self, _: &str, _: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{}\"", self.to_string().escape_default());
        let node = node!(this; attr!("label", label));
        node
    }
}

impl Drawer for ast::FuncProto<'_> {
    type Out = Node;

    fn draw(&self, _: &str, _: &mut Graph) -> Self::Out {
        let this = rand::id();
        let label = format!("\"{};\"", self.to_string().escape_default());
        let node = node!(this; attr!("label", label));
        node
    }
}

impl Drawer for ast::FuncDef<'_> {
    type Out = Node;

    fn draw(&self, parent: &str, g: &mut Graph) -> Self::Out {
        let this = rand::id();
        let proto = &self.proto.1;
        let label = format!("\"{}\"", proto.to_string().escape_default());
        let node = node!(this; attr!("label", label));
        node
    }
}
