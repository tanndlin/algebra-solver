use crate::types::{Operator, Token};

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
    pub node_type: Token,
    pub children: Vec<AstNode>,
}

impl AstNode {
    pub fn new_number(n: i32) -> Self {
        AstNode {
            node_type: Token::Number(n),
            children: vec![],
        }
    }

    pub fn is_term(&self) -> bool {
        matches!(self.node_type, Token::Term(_))
    }

    pub fn get_term_name(&self) -> String {
        match &self.node_type {
            Token::Term((_, s)) => s.clone(),
            _ => panic!("Expected term, got {:?}", self.node_type),
        }
    }

    pub fn get_term_coef(&self) -> i32 {
        match &self.node_type {
            Token::Term((coef, _)) => *coef,
            _ => panic!("Expected term, got {:?}", self.node_type),
        }
    }

    #[allow(dead_code)]
    pub fn to_string(&self) -> String {
        match &self.node_type {
            Token::Number(n) => n.to_string(),
            Token::Operator(op) => {
                let op = match op {
                    Operator::Add => "+",
                    Operator::Subtract => "-",
                    Operator::Multiply => "*",
                    Operator::Divide => "/",
                };

                let left = self.children[0].to_string();
                let right = self.children[1].to_string();
                format!("{} {} {}", left, op, right)
            }
            Token::LParen => format!("({})", self.children[0].to_string()),
            Token::Term((coef, s)) => {
                let coef_str = if *coef == 1 {
                    "".to_string()
                } else {
                    coef.to_string()
                };
                format!("{}{}", coef_str, s)
            }
            _ => panic!("Not implemented for {:?}", self.node_type),
        }
    }

    pub fn evaluate(&self) -> i32 {
        assert!(self.is_evaluatable());
        evaluate_node(self)
    }

    pub fn is_evaluatable(&self) -> bool {
        match self.node_type {
            Token::Number(_) => true,
            Token::Operator(_) => self.children.iter().all(|child| child.is_evaluatable()),
            Token::LParen => self.children[0].is_evaluatable(),
            _ => false,
        }
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
        Token::LParen => evaluate_node(&node.children[0]),
        _ => panic!("Not implemented for {:?}", node.node_type),
    }
}
