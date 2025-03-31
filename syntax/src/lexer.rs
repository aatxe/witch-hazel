use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'source> {
    // identifiers and whitespace
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*", |lex| lex.slice())]
    Identifier(&'source str),
    #[regex(r#"\s+"#, |lex| lex.slice())]
    Whitespace(&'source str),

    // literals
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    BooleanLiteral(bool),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    NumberLiteral(f64),

    // operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Divide,
    #[token("*")]
    Multiply,
    #[token("//")]
    FloorDivide,
    #[token("==")]
    DoubleEqual,
    #[token("~=")]
    NotEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("<=")]
    LessThanEqual,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,

    // reserved words
    #[token("and")]
    ReservedAnd,
    #[token("break")]
    ReservedBreak,
    #[token("case")]
    ReservedCase,
    #[token("continue")]
    ReservedContinue,
    #[token("do")]
    ReservedDo,
    #[token("else")]
    ReservedElse,
    #[token("elseif")]
    ReservedElseif,
    #[token("end")]
    ReservedEnd,
    #[token("export")]
    ReservedExport,
    #[token("for")]
    ReservedFor,
    #[token("function")]
    ReservedFunction,
    #[token("fn")]
    ReservedFn,
    #[token("if")]
    ReservedIf,
    #[token("import")]
    ReservedImport,
    #[token("in")]
    ReservedIn,
    #[token("local")]
    ReservedLocal,
    #[token("match")]
    ReservedMatch,
    #[token("module")]
    ReservedModule,
    #[token("nil")]
    ReservedNil,
    #[token("not")]
    ReservedNot,
    #[token("or")]
    ReservedOr,
    #[token("repeat")]
    ReservedRepeat,
    #[token("return")]
    ReservedReturn,
    #[token("then")]
    ReservedThen,
    #[token("type")]
    ReservedType,
    #[token("until")]
    ReservedUntil,
    #[token("while")]
    ReservedWhile,

    // punctuation and symbols
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("=")]
    Equal,
    #[token("@")]
    AtSign,
    #[token("--")]
    Comment,
    #[token("->")]
    SkinnyArrow,
    #[token("=>")]
    ThickArrow,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    SemiColon,
}

pub fn lex(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_two_numbers() {
        let result = lex("2 + 2").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::NumberLiteral(2.0)),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::NumberLiteral(2.0)),
            ]
        );
    }

    #[test]
    fn add_identifier_to_number() {
        let result = lex("foo + 2").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::Identifier("foo")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::NumberLiteral(2.0)),
            ]
        );
    }

    #[test]
    fn add_two_identifiers() {
        let result = lex("foo + bar").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::Identifier("foo")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("bar")),
            ]
        );
    }

    #[test]
    fn grouped_add_of_two_identifiers() {
        let result = lex("(foo + bar)").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::ParenOpen),
                Ok(Token::Identifier("foo")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("bar")),
                Ok(Token::ParenClose),
            ]
        );
    }

    #[test]
    fn lex_a_function_definition() {
        let result = lex("function name(a, b) return a + b end").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::ReservedFunction),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("name")),
                Ok(Token::ParenOpen),
                Ok(Token::Identifier("a")),
                Ok(Token::Comma),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::ParenClose),
                Ok(Token::Whitespace(" ")),
                Ok(Token::ReservedReturn),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("a")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::ReservedEnd),
            ]
        )
    }

    #[test]
    fn lex_a_multiline_function_definition() {
        let result = lex(r#"
        function name(a, b)
            return a + b
        end"#)
        .collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::Whitespace("\n        ")),
                Ok(Token::ReservedFunction),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("name")),
                Ok(Token::ParenOpen),
                Ok(Token::Identifier("a")),
                Ok(Token::Comma),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::ParenClose),
                Ok(Token::Whitespace("\n            ")),
                Ok(Token::ReservedReturn),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("a")),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::Whitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::Whitespace("\n        ")),
                Ok(Token::ReservedEnd),
            ]
        )
    }
}
