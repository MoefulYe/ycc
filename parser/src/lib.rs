mod error;
use error::YccParseError;
use lalrpop_util::lalrpop_mod;
use miette::NamedSource;
lalrpop_mod!(syntax); // synthesized by LALRPOP

pub fn parse<'input>(
    name: &'input str,
    code: &'input str,
) -> Result<ast::Module<'input>, error::YccParseError<'input>> {
    syntax::ModuleParser::new()
        .parse(code)
        .map_err(|err| YccParseError::from((err, NamedSource::new(name, code.to_owned()))))
}
