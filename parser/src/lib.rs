mod error;
use error::YccParseError;
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(syntax); // synthesized by LALRPOP

pub fn parse<'input>(code: &'input str) -> Result<ast::Module<'input>, miette::Report> {
    syntax::ModuleParser::new()
        .parse(code)
        .map_err(|err| YccParseError::from(err).into())
}
