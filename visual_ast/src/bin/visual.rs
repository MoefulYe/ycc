use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::{
    attributes::*,
    cmd::{CommandArg, Format},
    exec, exec_dot, parse,
    printer::{DotPrinter, PrinterContext},
};

fn main() {
    let mut g = subgraph!("sb";
         edge!(node_id!("a") => subgraph!(;
            node!("n";
            NodeAttributes::color(color_name::black), NodeAttributes::shape(shape::egg))
        ))
    );
    let graph_svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()]).unwrap();
    println!("{graph_svg}");
}
