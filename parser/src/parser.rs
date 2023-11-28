use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub syntax); // synthesized by LALRPOP

#[cfg(test)]
mod test {
    use crate::ast::_Literal;

    use super::syntax;

    #[test]
    fn null() {
        let (loc, _) = syntax::NullLiteralParser::new().parse("null").unwrap();
        assert!(loc == 0)
    }

    #[test]
    fn boolean() {
        let (_, parsed) = syntax::BoolLiteralParser::new().parse("true").unwrap();
        assert!(parsed == _Literal::Bool(true));
        let (_, parsed) = syntax::BoolLiteralParser::new().parse("false").unwrap();
        assert!(parsed == _Literal::Bool(false));
    }

    #[test]
    fn charactor() {
        let (_, parsed) = syntax::CharLiteralParser::new().parse("'a'").unwrap();
        assert!(parsed == _Literal::Char(b'a'));
        let (_, parsed) = syntax::CharLiteralParser::new().parse("'\\n'").unwrap();
        assert!(parsed == _Literal::Char(b'\n'));
        let (_, parsed) = syntax::CharLiteralParser::new().parse("'\\''").unwrap();
        assert!(parsed == _Literal::Char(b'\''));
        let (_, parsed) = syntax::CharLiteralParser::new().parse("'\\\\'").unwrap();
        assert!(parsed == _Literal::Char(b'\\'));
    }

    #[test]
    fn integer() {
        let (_, parsed) = syntax::IntLiteralParser::new().parse("0").unwrap();
        assert!(parsed == _Literal::Int(0));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("1").unwrap();
        assert!(parsed == _Literal::Int(1));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("123").unwrap();
        assert!(parsed == _Literal::Int(123));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("0x123").unwrap();
        assert!(parsed == _Literal::Int(0x123));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("0o123456").unwrap();
        assert!(parsed == _Literal::Int(0o123456));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("0b101010").unwrap();
        assert!(parsed == _Literal::Int(0b101010));
        let err = syntax::IntLiteralParser::new()
            .parse("0xfffffffffffffffffffffffffffffffffffffffffffffffff")
            .unwrap_err();
        assert!(err.to_string() == "fail to parse `0xfffffffffffffffffffffffffffffffffffffffffffffffff` to integer: number too large to fit in target type");
        let (_, parsed) = syntax::IntLiteralParser::new().parse("+1").unwrap();
        assert!(parsed == _Literal::Int(1));
        let (_, parsed) = syntax::IntLiteralParser::new().parse("-1").unwrap();
        assert!(parsed == _Literal::Int(-1));
    }
}
