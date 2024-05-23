use std::collections::HashMap;

use crate::ast::*;
use crate::types::*;

impl AstNode {
    pub fn simplify(self) -> AstNode {
        match self.node_type {
            Token::Term(_) => self,
            Token::Number(_) => self,
            Token::Operator(_) => simplify_operator(self),
            Token::LParen => self.children[0].clone().simplify(),
            _ => panic!("Not implemented for {:?}", self.node_type),
        }
    }
}

fn simplify_operator(ast: AstNode) -> AstNode {
    match ast.node_type.clone() {
        Token::Operator(op @ Operator::Add)
        | Token::Operator(op @ Operator::Subtract)
        | Token::Operator(op @ Operator::Multiply) => simplify_add_sub_mult(ast, op),
        Token::Operator(Operator::Divide) => simplify_divide(ast),
        _ => {
            let children = ast
                .children
                .into_iter()
                .map(|child| child.simplify())
                .collect();
            AstNode {
                node_type: ast.node_type,
                children,
            }
        }
    }
}

fn simplify_add_sub_mult(ast: AstNode, op: Operator) -> AstNode {
    let left = ast.children[0].clone().simplify();
    let right = ast.children[1].clone().simplify();

    // If they are evaluatable, and already simplified, then they are numbers
    if left.is_evaluatable() && right.is_evaluatable() {
        let left_val = left.evaluate();
        let right_val = right.evaluate();

        let result = match op {
            Operator::Add => left_val + right_val,
            Operator::Subtract => left_val - right_val,
            Operator::Multiply => left_val * right_val,
            _ => panic!("Expected add or subtract operator"),
        };

        return AstNode::new_number(result);
    }

    // If they are the same variable, combine them
    if left.is_term() && right.is_term() {
        if let Some(try_combine) = combine_terms(&ast, &op) {
            return try_combine;
        }
    }

    if right.is_term() && op == Operator::Subtract {
        // Negate the coefficient of the term
        let right_coef = right.get_term_coef();
        let right_term = right.get_term_name();

        return AstNode {
            node_type: Token::Operator(Operator::Add),
            children: vec![
                left,
                AstNode {
                    node_type: Token::Term((-right_coef, right_term)),
                    children: vec![],
                },
            ],
        };
    }

    AstNode {
        node_type: ast.node_type,
        children: vec![left, right],
    }
}

fn combine_terms(ast: &AstNode, op: &Operator) -> Option<AstNode> {
    let left = ast.children[0].clone();
    let right = ast.children[1].clone();

    let left_term = left.get_term_name();
    let right_term = right.get_term_name();

    if left_term == right_term {
        match op {
            Operator::Add | Operator::Subtract => {
                let left_coef = left.get_term_coef();
                let right_coef = right.get_term_coef();

                let coef = match op {
                    Operator::Add => left_coef + right_coef,
                    Operator::Subtract => left_coef - right_coef,
                    _ => panic!("Expected add or subtract operator"),
                };

                if coef == 0 {
                    return Some(AstNode {
                        node_type: Token::Number(0),
                        children: vec![],
                    });
                }

                return Some(AstNode {
                    node_type: Token::Term((coef, left_term.clone())),
                    children: vec![],
                });
            }

            _ => panic!("CLT is not implemented for multiply or divide"),
        }
    }

    None
}

fn simplify_divide(ast: AstNode) -> AstNode {
    let mut left = ast.children[0].clone();
    let mut right = ast.children[1].clone();

    if left.is_evaluatable() && right.is_evaluatable() {
        let mut left_val = left.evaluate();
        let mut right_val = right.evaluate();
        if right_val == 0 {
            panic!("Division by zero");
        }

        if left_val % right_val == 0 {
            return AstNode {
                node_type: Token::Number(left_val / right_val),
                children: vec![],
            };
        }

        // Leave as simplified fraction
        let gcd = gcd(left_val, right_val);
        left_val /= gcd;
        right_val /= gcd;
        return AstNode {
            node_type: Token::Operator(Operator::Divide),
            children: vec![
                AstNode {
                    node_type: Token::Number(left_val),
                    children: vec![],
                },
                AstNode {
                    node_type: Token::Number(right_val),
                    children: vec![],
                },
            ],
        };
    }

    left = left.simplify();
    right = right.simplify();

    if right.node_type == Token::Number(1) {
        return left;
    }

    AstNode {
        node_type: ast.node_type,
        children: vec![left, right],
    }
}

pub fn simplify(ast: &AstNode) {
    println!("Original: {:#?}", ast);
    println!("Simplified: {:#?}", ast.clone().simplify().to_string());
}

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

#[cfg(test)]
mod test {
    use crate::test;

    macro_rules! test_simplify {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let tokens = crate::lexer::tokenize($input);
                let ast = crate::parser::parse(tokens);
                let simplified = ast.simplify();
                assert_eq!(simplified.to_string(), $expected);
            }
        };
    }

    test_simplify!(simplify_add, "1 + 2", "3");
    test_simplify!(simplify_subtract, "1 - 2", "-1");
    test_simplify!(simplify_multiply, "2 * 3", "6");
    test_simplify!(simplify_divide, "6 / 2", "3");
    test_simplify!(simplify_divide_fraction, "1 / 3", "1 / 3");
    test_simplify!(simplify_divide_fraction_simplified, "2 / 4", "1 / 2");
    test_simplify!(simplify_parentheses, "(1 + 2) * 3", "9");
    test_simplify!(simplify_parentheses_nested, "(1 + (2 * 3))", "7");
    test_simplify!(simplify_parentheses_denominator, "1 / (2 * 3)", "1 / 6");
    test_simplify!(simplify_combine_one_like_term, "x + x", "2x");
    test_simplify!(simplify_combine_like_terms, "x + x + x", "3x");
    test_simplify!(simplify_combine_like_terms_subtract, "x - x", "0");
    test_simplify!(
        simplify_combine_multiple_like_terms,
        "x + x + y + y",
        "2x + 2y"
    );
    test_simplify!(simplify_combine_like_terms_far, "x + 2 * 2 + x", "2x + 4");
    // test_simplify!(simplify_combine_like_terms_multiply, "x * x", "x ^ 2");
}
