pub mod error;
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub syntax); // synthesized by LALRPOP

pub fn parse(
    code: &str,
) -> Result<
    ast::Module<'_>,
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, error::UserError<'_>>,
> {
    syntax::ModuleParser::new().parse(code, code)
}
