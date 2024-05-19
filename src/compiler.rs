use crate::ast::*;
use crate::types::*;

pub fn simplify(ast: &AstNode) {
    is_equal(ast);
}

pub fn is_equal(ast: &AstNode) {
    assert!(ast.node_type == Token::Eq);
}
