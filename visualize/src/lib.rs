mod drawer;
mod utils;

use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
pub fn visualize(ast: &ast::Module<'_>) -> Result<String, std::io::Error> {
    let g = drawer::draw(ast);
    exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()])
}
