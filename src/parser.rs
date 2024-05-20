use crate::ast::*;
use crate::lexer::LexerToken;
use crate::types::*;

struct Parser {
    tokens: Vec<LexerToken>,
    position: usize,
}

impl Parser {
    fn get_current_token(&self) -> &LexerToken {
        self.tokens.get(self.position).unwrap()
    }

    fn get_next(&self, n: usize) -> Option<&LexerToken> {
        self.tokens.get(self.position + n)
    }

    fn is_end(&self) -> bool {
        self.position >= self.tokens.len()
    }
}

pub fn parse(tokens: Vec<LexerToken>) -> AstNode {
    let mut parser = Parser {
        tokens,
        position: 0,
    };

    parse_expression(&mut parser)
}

fn parse_parentheses(parser: &mut Parser) -> AstNode {
    consume_token(parser, Token::LParen);
    let ast = AstNode {
        node_type: Token::LParen,
        children: vec![parse_expression(parser)],
    };

    consume_token(parser, Token::RParen);
    ast
}

fn parse_expression(parser: &mut Parser) -> AstNode {
    parse_equality(parser)
}

fn parse_mul_div(parser: &mut Parser) -> AstNode {
    let left = parse_factor(parser);
    if parser.is_end() {
        return left;
    }

    match parser.get_current_token().token.clone() {
        Token::Operator(op @ Operator::Multiply) | Token::Operator(op @ Operator::Divide) => {
            parser.position += 1;
            AstNode {
                node_type: Token::Operator(op),
                children: vec![left, parse_mul_div(parser)],
            }
        }
        _ => left,
    }
}

fn parse_add_sub(parser: &mut Parser) -> AstNode {
    let left = parse_mul_div(parser);
    if parser.is_end() {
        return left;
    }

    match parser.get_current_token().token.clone() {
        Token::Operator(op @ Operator::Add) | Token::Operator(op @ Operator::Subtract) => {
            parser.position += 1;
            AstNode {
                node_type: Token::Operator(op),
                children: vec![left, parse_add_sub(parser)],
            }
        }
        _ => left,
    }
}

fn parse_relational(parser: &mut Parser) -> AstNode {
    let left = parse_add_sub(parser);
    if parser.is_end() {
        return left;
    }

    match parser.get_current_token().token {
        Token::LessThan | Token::GreaterThan | Token::Leq | Token::Geq => {
            parser.position += 1;
            AstNode {
                node_type: match parser.get_current_token().token {
                    Token::LessThan => Token::LessThan,
                    Token::GreaterThan => Token::GreaterThan,
                    Token::Leq => Token::Leq,
                    Token::Geq => Token::Geq,
                    _ => panic!("Expected relational operator"),
                },
                children: vec![left, parse_relational(parser)],
            }
        }
        _ => left,
    }
}

fn parse_equality(parser: &mut Parser) -> AstNode {
    let left = parse_relational(parser);
    if parser.is_end() {
        return left;
    }

    match parser.get_current_token().token {
        Token::Eq => {
            parser.position += 1;
            AstNode {
                node_type: Token::Eq,
                children: vec![left, parse_equality(parser)],
            }
        }
        _ => left,
    }
}

fn parse_number(parser: &mut Parser) -> AstNode {
    let token = consume_token(parser, Token::Number(0));

    AstNode {
        node_type: token.token,
        children: vec![],
    }
}

fn parse_identifier(parser: &mut Parser) -> AstNode {
    let tok = consume_token(parser, Token::Term((1, String::new())));

    AstNode {
        node_type: tok.token,
        children: vec![],
    }
}

fn parse_factor(parser: &mut Parser) -> AstNode {
    let token = &parser.get_current_token();

    match token.token {
        Token::Number(_) => parse_number(parser),
        Token::Term(_) => parse_identifier(parser),
        Token::LParen => parse_parentheses(parser),
        _ => panic!("Expected number or identifier"),
    }
}

fn consume_token(parser: &mut Parser, expected_token: Token) -> LexerToken {
    if parser.is_end() {
        panic!("Expected token {:?}, got end of input", expected_token);
    }

    let token = parser.get_current_token().clone();
    if !variant_eq(&expected_token, &token.token) {
        panic!("Expected token {:?}, got {:?}", expected_token, token.token);
    }

    parser.position += 1;
    token.clone()
}

#[allow(dead_code)]
fn consume_one_of(parser: &mut Parser, expected_tokens: Vec<Token>) -> LexerToken {
    let token = parser.get_current_token().clone();
    if !expected_tokens.contains(&token.token) {
        panic!(
            "Expected one of {:?}, got {:?}",
            expected_tokens, token.token
        );
    }

    parser.position += 1;
    token.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn parser_parse_addition() {
        let script = "1 + 2";
        let tokens = tokenize(script);
        let add_ast = parse(tokens);

        assert_eq!(add_ast.node_type, Token::Operator(Operator::Add));
        assert_eq!(add_ast.children.len(), 2);

        assert_eq!(add_ast.children[0].node_type, Token::Number(1));
        assert_eq!(add_ast.children[1].node_type, Token::Number(2));
    }
}
