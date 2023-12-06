use graphviz_rust::{cmd::Format, exec, printer::DotPrinter, printer::PrinterContext};
use parser::parse;
use std::{fs::File, io::Write};
use visual_ast::drawer::draw;
fn main() {
    let code = r##"
    const int a[2][2]= {1,2};
    int a = 1;
    int a(int a, int b) {
        return;
        int c = 0;
    }
    "##;
    let m = parse("test", code).unwrap();
    let g = draw(&m);
    // let s = g.print(&mut PrinterContext::default());
    // println!("{s}");
    let svg = exec(g, &mut PrinterContext::default(), vec![Format::Svg.into()]).unwrap();
    let mut f = File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open("test.svg")
        .unwrap();
    f.write_all(svg.as_bytes()).unwrap();
}
