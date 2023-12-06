use graphviz_rust::{cmd::Format, exec, printer::PrinterContext};
use parser::parse;
use std::{fs::File, io::Write};
use visual_ast::drawer::draw;
fn main() {
    let code = r##"
    const int a = 1;
    int b = 2;
    int foo(int a, int b[2]);
    int a() {
        return 1;
    }
    "##;
    let m = parse("test", code).unwrap();
    let g = draw(&m);
    let svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()]).unwrap();
    let mut f = File::options()
        .write(true)
        .create(true)
        .open("test.svg")
        .unwrap();
    f.write_all(svg.as_bytes()).unwrap();
}
