use crate::types::{Operator, Token};

#[derive(Debug)]
pub struct AstNode {
    pub node_type: Token,
    pub children: Vec<AstNode>,
}

impl AstNode {
    pub fn evaluate(&self) -> i32 {
        evaluate_node(self)
    }
}

fn evaluate_node(node: &AstNode) -> i32 {
    macro_rules! eval_operator {
        ($op:tt) => {
            evaluate_node(&node.children[0]) $op evaluate_node(&node.children[1])
        };
    }

    match node.node_type {
        Token::Number(n) => n,
        Token::Operator(Operator::Add) => eval_operator!(+),
        Token::Operator(Operator::Subtract) => eval_operator!(-),
        Token::Operator(Operator::Multiply) => eval_operator!(*),
        Token::Operator(Operator::Divide) => eval_operator!(/),
        Token::Operator(Operator::Mod) => eval_operator!(%),
        Token::LParen => evaluate_node(&node.children[0]),
        _ => panic!("Not implemented for {:?}", node.node_type),
    }
}
