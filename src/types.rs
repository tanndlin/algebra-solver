#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Term((i32, String)),
    Number(i32),
    Operator(Operator),
    LParen,
    RParen,
    Eq,
    LessThan,
    GreaterThan,
    Leq,
    Geq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}
