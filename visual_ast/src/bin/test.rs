use miette::Report;
use parser::parse;

fn main() {
    match parse(
        "test.c",
        r##"
            int main () {
                int a = 1 aasdask
            }
            "##,
    ) {
        Ok(ok) => println!("{ok}"),
        Err(err) => {
            let report: Report = err.into();
            println!("{:?}", report);
        }
    }
}
