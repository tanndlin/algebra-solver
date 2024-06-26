mod ast;
mod lexer;
mod parser;
// mod simplifier;
mod test;
mod types;

fn main() {
    let content = std::fs::read_to_string("equation.txt").expect("Could not read file");
    let tokens: Vec<lexer::LexerToken> = lexer::tokenize(&content);
    for token in tokens.iter() {
        println!("{:?}", token);
    }

    let ast = parser::parse(tokens);
    // simplifier::simplify(&ast);
}
