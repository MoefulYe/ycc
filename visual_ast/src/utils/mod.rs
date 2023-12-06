pub mod rand;

pub fn report(err: impl Into<miette::Report>) {
    let report: miette::Report = err.into();
    println!("{:?}", report);
}
