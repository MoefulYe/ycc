use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub syntax); // synthesized by LALRPOP

pub use syntax::*;

#[cfg(test)]
mod test {
    use crate::parser::syntax::*;
    use ast::Literal;

    #[test]
    fn boolean() {
        let (_, parsed) = LiteralParser::new().parse("true").unwrap();
        assert!(parsed == Literal::Bool(true));
        let (_, parsed) = LiteralParser::new().parse("false").unwrap();
        assert!(parsed == Literal::Bool(false));
    }

    #[test]
    fn integer() {
        let (_, parsed) = LiteralParser::new().parse("0").unwrap();
        assert!(parsed == Literal::Int(0));
        let (_, parsed) = LiteralParser::new().parse("1").unwrap();
        assert!(parsed == Literal::Int(1));
        let (_, parsed) = LiteralParser::new().parse("123").unwrap();
        assert!(parsed == Literal::Int(123));
        let (_, parsed) = LiteralParser::new().parse("0x123").unwrap();
        assert!(parsed == Literal::Int(0x123));
        let (_, parsed) = LiteralParser::new().parse("0o123456").unwrap();
        assert!(parsed == Literal::Int(0o123456));
        let (_, parsed) = LiteralParser::new().parse("0b101010").unwrap();
        assert!(parsed == Literal::Int(0b101010));
        let err = LiteralParser::new()
            .parse("0xfffffffffffffffffffffffffffffffffffffffffffffffff")
            .unwrap_err();
        assert!(err.to_string() == "fail to parse `0xfffffffffffffffffffffffffffffffffffffffffffffffff` to integer: number too large to fit in target type");
        let (_, parsed) = LiteralParser::new().parse("+1").unwrap();
        assert!(parsed == Literal::Int(1));
        let (_, parsed) = LiteralParser::new().parse("-1").unwrap();
        assert!(parsed == Literal::Int(-1));
    }

    #[test]
    fn array() {
        let (_, parsed) = LiteralParser::new().parse(r##"{}"##).unwrap();
        assert!(parsed == Literal::List(vec![]));
        let (_, parsed) = LiteralParser::new().parse(r##"{1}"##).unwrap();
        match parsed {
            Literal::List(list) => {
                assert!(list[0].1 == Literal::Int(1));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2}"##).unwrap();
        match parsed {
            Literal::List(list) => {
                assert!(list[0].1 == Literal::Int(1));
                assert!(list[1].1 == Literal::Int(2));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2, 3, }"##).unwrap();
        match parsed {
            Literal::List(list) => {
                assert!(list[0].1 == Literal::Int(1));
                assert!(list[1].1 == Literal::Int(2));
                assert!(list[2].1 == Literal::Int(3));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{1, 2, 3, 4,{} }"##).unwrap();
        match parsed {
            Literal::List(list) => {
                assert!(list[0].1 == Literal::Int(1));
                assert!(list[1].1 == Literal::Int(2));
                assert!(list[2].1 == Literal::Int(3));
                assert!(list[3].1 == Literal::Int(4));
                assert!(list[4].1 == Literal::List(vec![]));
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new()
            .parse(r##"{1, 2, 3, 4, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}"##)
            .unwrap();
        match parsed {
            Literal::List(list) => {
                assert!(list[0].1 == Literal::Int(1));
                assert!(list[1].1 == Literal::Int(2));
                assert!(list[2].1 == Literal::Int(3));
                assert!(list[3].1 == Literal::Int(4));
                match &list[4].1 {
                    Literal::List(list) => {
                        assert!(list[0].1 == Literal::Int(1));
                        assert!(list[1].1 == Literal::Int(2));
                        assert!(list[2].1 == Literal::Int(3));
                        assert!(list[3].1 == Literal::Int(4));
                        assert!(list[4].1 == Literal::Int(5));
                        assert!(list[5].1 == Literal::Int(6));
                        assert!(list[6].1 == Literal::Int(7));
                        assert!(list[7].1 == Literal::Int(8));
                        assert!(list[8].1 == Literal::Int(9));
                        assert!(list[9].1 == Literal::Int(10));
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
        let (_, parsed) = LiteralParser::new().parse(r##"{{{{}}}}"##).unwrap();
        match parsed {
            Literal::List(list) => match &list[0].1 {
                Literal::List(list) => match &list[0].1 {
                    Literal::List(_) => {}
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    //
    // #[test]
    // fn type_() {
    //     let (_, parsed) = TypeParser::new().parse("void").unwrap();
    //     match parsed {
    //         Type::Prim(prim) => {
    //             assert!(prim.1 == Prim::Void)
    //         }
    //         _ => panic!(),
    //     }
    // }
    //
    #[test]
    fn expr0() {
        let set = &["sum(1.11, 2)", "1", "arr[0]", "arr[0][1]", "1 + 1 * 9 != 3"];
        for expect in set {
            let (_, parsed) = ExprParser::new().parse(expect).unwrap();
            println!("{}", parsed.to_string())
        }
    }
}
