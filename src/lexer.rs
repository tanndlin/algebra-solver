use crate::types::*;

#[derive(Debug, Clone)]
pub struct LexerToken {
    pub token: Token,
    pub position: usize,
    pub line: usize,
}

struct Lexer {
    script: String,
    position: usize,
    line_number: usize,
    tokens: Vec<LexerToken>,
}

impl Lexer {
    fn is_end(&self) -> bool {
        self.position >= self.script.len()
    }

    fn has_two_more_chars(&self) -> bool {
        self.position + 1 < self.script.len()
    }

    fn create_token(&mut self, token: Token) {
        self.tokens.push(LexerToken {
            position: self.position,
            token,
            line: self.line_number,
        });
    }

    fn cur_char(&self) -> char {
        self.script.chars().nth(self.position).unwrap()
    }
}

pub fn tokenize(script: &str) -> Vec<LexerToken> {
    let mut lexer = Lexer {
        script: script.to_string(),
        position: 0,
        line_number: 1,
        tokens: vec![],
    };

    while !lexer.is_end() {
        if is_whitespace(lexer.cur_char()) {
            lexer.position += 1;
            continue;
        }

        let next_token = next_token(&mut lexer);
        lexer.create_token(next_token);
        if !lexer.is_end() && is_newline(lexer.cur_char()) {
            lexer.line_number += 1;
        }
    }

    lexer.tokens
}

fn next_token(lexer: &mut Lexer) -> Token {
    let c = lexer.cur_char();
    if c.is_ascii_digit() {
        return lex_number(lexer);
    }

    if let Some(token) = match_operator(lexer) {
        return token;
    }

    if !c.is_alphanumeric() {
        panic!("Unexpected character: {}", c);
    }

    Token::Term((1, lex_identifier(lexer).to_string()))
}

fn lex_identifier(lexer: &mut Lexer) -> String {
    let mut ident = String::new();
    while !lexer.is_end() {
        let c = lexer.cur_char();
        if c.is_alphanumeric() {
            ident.push(c);
            lexer.position += 1;
        } else {
            break;
        }
    }

    ident
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || is_newline(c)
}

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

fn match_operator(lexer: &mut Lexer) -> Option<Token> {
    // Try double char operators first
    if let Some(token) = match_double_char_op(lexer) {
        return Some(token);
    }

    let ret = match lexer.cur_char() {
        '+' => Some(Token::Operator(Operator::Add)),
        '-' => Some(Token::Operator(Operator::Subtract)),
        '*' => Some(Token::Operator(Operator::Multiply)),
        '/' => Some(Token::Operator(Operator::Divide)),
        '<' => Some(Token::LessThan),
        '>' => Some(Token::GreaterThan),
        '(' => Some(Token::LParen),
        ')' => Some(Token::RParen),
        _ => None,
    };

    if ret.is_some() {
        lexer.position += 1;
    }

    ret
}

fn match_double_char_op(lexer: &mut Lexer) -> Option<Token> {
    if !lexer.has_two_more_chars() {
        return None;
    }

    let c = lexer.cur_char();
    let next_c = lexer.script.chars().nth(lexer.position + 1).unwrap();
    let matched = match (c, next_c) {
        ('=', '=') => Some(Token::Eq),
        ('<', '=') => Some(Token::Leq),
        ('>', '=') => Some(Token::Geq),
        _ => None,
    };

    if matched.is_some() {
        lexer.position += 2;
    }

    matched
}

fn lex_number(lexer: &mut Lexer) -> Token {
    let mut coef = String::new();

    while !lexer.is_end() {
        let next_c = lexer.cur_char();
        if next_c.is_ascii_digit() {
            coef.push(next_c);
            lexer.position += 1;
        } else {
            break;
        }
    }

    let coef = coef.parse::<i32>().unwrap();

    if !lexer.is_end() && lexer.cur_char().is_alphabetic() {
        let term_name = lex_identifier(lexer);
        return Token::Term((coef, term_name));
    }

    Token::Number(coef)
}

// Tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_whitespace() {
        assert!(is_whitespace(' '));
        assert!(is_whitespace('\n'));
        assert!(is_whitespace('\t'));
        assert!(!is_whitespace('a'));
    }

    #[test]
    fn lex_simple_expression() {
        let tokens = tokenize("1 + 2");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token, Token::Number(1));
        assert_eq!(tokens[1].token, Token::Operator(Operator::Add));
        assert_eq!(tokens[2].token, Token::Number(2));
    }

    #[test]
    fn lex_all_operators() {
        let tokens = tokenize("+-*/<><=>===");
        assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[0].token, Token::Operator(Operator::Add));
        assert_eq!(tokens[1].token, Token::Operator(Operator::Subtract));
        assert_eq!(tokens[2].token, Token::Operator(Operator::Multiply));
        assert_eq!(tokens[3].token, Token::Operator(Operator::Divide));
        assert_eq!(tokens[4].token, Token::LessThan);
        assert_eq!(tokens[5].token, Token::GreaterThan);
        assert_eq!(tokens[6].token, Token::Leq);
        assert_eq!(tokens[7].token, Token::Geq);
        assert_eq!(tokens[8].token, Token::Eq);
    }

    #[test]
    fn parse_terms() {
        let tokens = tokenize("2x + 3y");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token, Token::Term((2, "x".to_string())));
        assert_eq!(tokens[1].token, Token::Operator(Operator::Add));
        assert_eq!(tokens[2].token, Token::Term((3, "y".to_string())));
    }
}
