use crate::types::Operator;

#[derive(Debug, PartialEq)]
pub enum NodeType {
    // Factors
    Number,
    Identifier,

    // Expressions
    Operator(Operator),
    LParen,
    Eq,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

pub fn operator_to_node_type(op: Operator) -> NodeType {
    NodeType::Operator(op)
}

#[derive(Debug)]
pub struct AstNode {
    pub node_type: NodeType,
    pub children: Vec<AstNode>,
    pub value: Option<String>,
}

impl AstNode {
    pub fn evaluate(&self) -> i32 {
        evaluate_node(self).unwrap()
    }
}

fn evaluate_node(node: &AstNode) -> Option<i32> {
    macro_rules! eval_operator {
        ($op:tt) => {
            match (evaluate_node(&node.children[0]), evaluate_node(&node.children[1])) {
                (Some(l), Some(r)) => Some(l $op r),
                _ => None,
            }
        };
    }

    match node.node_type {
        NodeType::Number => node.value.clone().unwrap().parse::<i32>().ok(),
        NodeType::Operator(Operator::Add) => eval_operator!(+),
        NodeType::Operator(Operator::Subtract) => eval_operator!(-),
        NodeType::Operator(Operator::Multiply) => eval_operator!(*),
        NodeType::Operator(Operator::Divide) => eval_operator!(/),
        NodeType::Operator(Operator::Mod) => eval_operator!(%),
        NodeType::LParen => evaluate_node(&node.children[0]),
        _ => panic!("Not implemented for {:?}", node.node_type),
    }
}
