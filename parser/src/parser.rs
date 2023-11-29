use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub syntax); // synthesized by LALRPOP

#[cfg(test)]
mod test {
    use crate::{
        ast::{AstLiteral_, AstPrimType_, AstType_},
        parser::syntax::{
            BoolLiteralParser, CharLiteralParser, ExprParser, FloatLiteralParser, IntLiteralParser,
            LiteralParser, NullLiteralParser, StringLiteralParser, TypeParser,
        },
    };

    #[test]
    fn null() {
        let (loc, _) = NullLiteralParser::new().parse("null").unwrap();
        assert!(loc == 0)
    }

    #[test]
    fn boolean() {
        let (_, parsed) = BoolLiteralParser::new().parse("true").unwrap();
        assert!(parsed == AstLiteral_::Bool(true));
        let (_, parsed) = BoolLiteralParser::new().parse("false").unwrap();
        assert!(parsed == AstLiteral_::Bool(false));
    }

    #[test]
    fn charactor() {
        let (_, parsed) = CharLiteralParser::new().parse("'a'").unwrap();
        assert!(parsed == AstLiteral_::Char(b'a'));
        let (_, parsed) = CharLiteralParser::new().parse("'\\n'").unwrap();
        assert!(parsed == AstLiteral_::Char(b'\n'));
        let (_, parsed) = CharLiteralParser::new().parse("'\\''").unwrap();
        assert!(parsed == AstLiteral_::Char(b'\''));
        let (_, parsed) = CharLiteralParser::new().parse("'\\\\'").unwrap();
        assert!(parsed == AstLiteral_::Char(b'\\'));
        let (_, parsed) = CharLiteralParser::new().parse("'\\0'").unwrap();
        assert!(parsed == AstLiteral_::Char(0u8));
    }

    #[test]
    fn integer() {
        let (_, parsed) = IntLiteralParser::new().parse("0").unwrap();
        assert!(parsed == AstLiteral_::Int(0));
        let (_, parsed) = IntLiteralParser::new().parse("1").unwrap();
        assert!(parsed == AstLiteral_::Int(1));
        let (_, parsed) = IntLiteralParser::new().parse("123").unwrap();
        assert!(parsed == AstLiteral_::Int(123));
        let (_, parsed) = IntLiteralParser::new().parse("0x123").unwrap();
        assert!(parsed == AstLiteral_::Int(0x123));
        let (_, parsed) = IntLiteralParser::new().parse("0o123456").unwrap();
        assert!(parsed == AstLiteral_::Int(0o123456));
        let (_, parsed) = IntLiteralParser::new().parse("0b101010").unwrap();
        assert!(parsed == AstLiteral_::Int(0b101010));
        let err = IntLiteralParser::new()
            .parse("0xfffffffffffffffffffffffffffffffffffffffffffffffff")
            .unwrap_err();
        assert!(err.to_string() == "fail to parse `0xfffffffffffffffffffffffffffffffffffffffffffffffff` to integer: number too large to fit in target type");
        let (_, parsed) = IntLiteralParser::new().parse("+1").unwrap();
        assert!(parsed == AstLiteral_::Int(1));
        let (_, parsed) = IntLiteralParser::new().parse("-1").unwrap();
        assert!(parsed == AstLiteral_::Int(-1));
    }

    #[test]
    fn float() {
        let (_, parsed) = FloatLiteralParser::new().parse("0.0").unwrap();
        assert!(parsed == AstLiteral_::Float(0.0));
        let (_, parsed) = FloatLiteralParser::new().parse("1.0").unwrap();
        assert!(parsed == AstLiteral_::Float(1.0));
        let (_, parsed) = FloatLiteralParser::new().parse("123.0").unwrap();
        assert!(parsed == AstLiteral_::Float(123.0));
        let (_, parsed) = FloatLiteralParser::new().parse("0.123").unwrap();
        assert!(parsed == AstLiteral_::Float(0.123));
        let (_, parsed) = FloatLiteralParser::new().parse("0.123e10").unwrap();
        assert!(parsed == AstLiteral_::Float(0.123e10));
        let (_, parsed) = FloatLiteralParser::new().parse("0.123e-10").unwrap();
        assert!(parsed == AstLiteral_::Float(0.123e-10));
        let (_, parsed) = FloatLiteralParser::new().parse("0.123e+10").unwrap();
        assert!(parsed == AstLiteral_::Float(0.123e+10));
        let (_, parsed) = FloatLiteralParser::new().parse("+1.0").unwrap();
        assert!(parsed == AstLiteral_::Float(1.0));
        let (_, parsed) = FloatLiteralParser::new().parse("-1.0").unwrap();
        assert!(parsed == AstLiteral_::Float(-1.0));
        let (_, parsed) = FloatLiteralParser::new().parse("1.0e10").unwrap();
        assert!(parsed == AstLiteral_::Float(1.0e10));
    }

    #[test]
    fn string() {
        let (_, parsed) = StringLiteralParser::new().parse(r###""""###).unwrap();
        assert!(parsed == AstLiteral_::String("".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""a""###).unwrap();
        assert!(parsed == AstLiteral_::String("a".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""\n""###).unwrap();
        assert!(parsed == AstLiteral_::String("\n".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""\t""###).unwrap();
        assert!(parsed == AstLiteral_::String("\t".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""\r""###).unwrap();
        assert!(parsed == AstLiteral_::String("\r".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""\0""###).unwrap();
        assert!(parsed == AstLiteral_::String("\0".to_string()));
        let (_, parsed) = StringLiteralParser::new().parse(r###""\\""###).unwrap();
        assert!(parsed == AstLiteral_::String("\\".to_string()));
    }

    #[test]
    fn array() {
        let (_, parsed) = LiteralParser::new().parse(r##"{}"##).unwrap();
        assert!(parsed == AstLiteral_::List(vec![]));
        let (_, parsed) = LiteralParser::new().parse(r##"{1}"##).unwrap();
        match parsed {
            AstLiteral_::List(list) => {
                assert!(list[0].1 == AstLiteral_::Int(1));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2}"##).unwrap();
        match parsed {
            AstLiteral_::List(list) => {
                assert!(list[0].1 == AstLiteral_::Int(1));
                assert!(list[1].1 == AstLiteral_::Int(2));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2, 3, }"##).unwrap();
        match parsed {
            AstLiteral_::List(list) => {
                assert!(list[0].1 == AstLiteral_::Int(1));
                assert!(list[1].1 == AstLiteral_::Int(2));
                assert!(list[2].1 == AstLiteral_::Int(3));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2, 3, 4,{} }"##).unwrap();
        match parsed {
            AstLiteral_::List(list) => {
                assert!(list[0].1 == AstLiteral_::Int(1));
                assert!(list[1].1 == AstLiteral_::Int(2));
                assert!(list[2].1 == AstLiteral_::Int(3));
                assert!(list[3].1 == AstLiteral_::Int(4));
                assert!(list[4].1 == AstLiteral_::List(vec![]));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new()
            .parse(r##"{1, 2, 3, 4, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}"##)
            .unwrap();
        match parsed {
            AstLiteral_::List(list) => {
                assert!(list[0].1 == AstLiteral_::Int(1));
                assert!(list[1].1 == AstLiteral_::Int(2));
                assert!(list[2].1 == AstLiteral_::Int(3));
                assert!(list[3].1 == AstLiteral_::Int(4));
                match &list[4].1 {
                    AstLiteral_::List(list) => {
                        assert!(list[0].1 == AstLiteral_::Int(1));
                        assert!(list[1].1 == AstLiteral_::Int(2));
                        assert!(list[2].1 == AstLiteral_::Int(3));
                        assert!(list[3].1 == AstLiteral_::Int(4));
                        assert!(list[4].1 == AstLiteral_::Int(5));
                        assert!(list[5].1 == AstLiteral_::Int(6));
                        assert!(list[6].1 == AstLiteral_::Int(7));
                        assert!(list[7].1 == AstLiteral_::Int(8));
                        assert!(list[8].1 == AstLiteral_::Int(9));
                        assert!(list[9].1 == AstLiteral_::Int(10));
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{{{{}}}}"##).unwrap();
        match parsed {
            AstLiteral_::List(list) => match &list[0].1 {
                AstLiteral_::List(list) => match &list[0].1 {
                    AstLiteral_::List(_) => {}
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    fn type_() {
        let (_, parsed) = TypeParser::new().parse("void").unwrap();
        match parsed {
            AstType_::Prim(prim) => {
                assert!(prim.1 == AstPrimType_::Void)
            }
            _ => panic!(),
        }
    }

    #[test]
    fn expr0() {
        let set = &["sum(1.1, 2)", "1", "*null", "arr[0]", "arr[0][1]"];
        for expect in set {
            let (_, parsed) = ExprParser::new().parse(expect).unwrap();
            let actual = parsed.to_string();
            assert_eq!(expect, &actual);
        }
    }
}
