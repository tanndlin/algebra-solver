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
        Token::Operator(op @ Operator::Add) | Token::Operator(op @ Operator::Subtract) => {
            simplify_add_sub(ast, op)
        }
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

fn simplify_add_sub(ast: AstNode, op: Operator) -> AstNode {
    let left = ast.children[0].clone();
    let right = ast.children[1].clone();

    if left.is_evaluatable() && right.is_evaluatable() {
        let left_val = left.evaluate();
        let right_val = right.evaluate();

        let result = match op {
            Operator::Add => left_val + right_val,
            Operator::Subtract => left_val - right_val,
            _ => panic!("Expected add or subtract operator"),
        };

        AstNode {
            node_type: Token::Number(result),
            children: vec![],
        }
    } else {
        AstNode {
            node_type: ast.node_type,
            children: vec![left.simplify(), right.simplify()],
        }
    }
}

pub fn simplify(ast: &AstNode) {
    println!("Simplified: {:#?}", ast.clone().simplify().to_string());
}

pub fn is_equal(ast: &AstNode) -> bool {
    assert!(ast.node_type == Token::Eq);
    let left = &ast.children[0];
    let right = &ast.children[1];

    assert!(left.is_evaluatable());
    assert!(right.is_evaluatable());

    let left_val = left.evaluate();
    let right_val = right.evaluate();

    left_val == right_val
}
