use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;

macro_rules! test {
    ($name:ident, $script:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let tokens = tokenize($script);
            let root = parse(tokens);

            assert_eq!(root.evaluate(), $expected);
        }
    };
}

test!(integration_parse_addition, "20 + 22", 42);
test!(integration_parse_subtraction, "64-22", 42);
test!(integration_parse_multiplication, "6 * 7", 42);
test!(integration_parse_division, "84 / 2", 42);
test!(integration_parse_parentheses1, "( 2 + 2 ) * 10", 40);
test!(integration_parse_parentheses2, "10 * (2 + 2)", 40);
test!(integration_follows_pemdas_mult_after, "2 + 2 * 10", 22);
test!(integration_follows_pemdas_mult_before, "10 * 2 + 2", 22);
test!(integration_parse_multiple_ops, "10 + 10 + 5 + 2", 27);
