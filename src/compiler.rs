use crate::ast::*;
use crate::types::*;

impl AstNode {
    pub fn simplify(self) -> AstNode {
        match self.node_type {
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
    let left = ast.children[0].clone();
    let right = ast.children[1].clone();

    if left.is_evaluatable() && right.is_evaluatable() {
        let left_val = left.clone().simplify();
        let right_val = right.clone().simplify();

        if variant_eq(&left_val.node_type, &Token::Number(0))
            && variant_eq(&right_val.node_type, &Token::Number(0))
        {
            let left_val = left_val.evaluate();
            let right_val = right_val.evaluate();

            let result = match op {
                Operator::Add => left_val + right_val,
                Operator::Subtract => left_val - right_val,
                Operator::Multiply => left_val * right_val,
                _ => panic!("Expected add or subtract operator"),
            };

            return AstNode {
                node_type: Token::Number(result),
                children: vec![],
            };
        }
    }

    AstNode {
        node_type: ast.node_type,
        children: vec![left.simplify(), right.simplify()],
    }
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
}
