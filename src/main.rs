mod cmd;
use clap::Parser;
use cmd::Cmd;

fn main() -> miette::Result<()> {
    Cmd::parse().exec()
}
