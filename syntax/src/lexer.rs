use logos::{Lexer, Logos};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct LexError;

pub type LexResult<'source> = Result<Token<'source>, LexError>;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexError)]
pub enum Token<'source> {
    // identifiers and whitespace
    #[regex(r"\p{XID_Start}[\p{XID_Continue}-]*", |lex| lex.slice())]
    Identifier(&'source str),
    #[regex(r#"[[:blank:]]+"#, |lex| lex.slice())]
    HorizontalWhitespace(&'source str),
    #[regex(r#"[\n\v\f\r\x85\u2028\u2029]"#, |lex| lex.slice())]
    VerticalWhitespace(&'source str),

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
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
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
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
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
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
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
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("bar")),
                Ok(Token::ParenClose),
            ]
        );
    }

    #[test]
    fn lex_kebab_case_identifier() {
        let result = lex("foo-bar").collect::<Vec<_>>();
        assert_eq!(result, vec![Ok(Token::Identifier("foo-bar")),]);
    }

    #[test]
    fn lex_greek_identifier() {
        let result = lex("αβγ").collect::<Vec<_>>();
        assert_eq!(result, vec![Ok(Token::Identifier("αβγ")),]);
    }

    #[test]
    fn lex_chinese_identifier() {
        let result = lex("你好").collect::<Vec<_>>();
        assert_eq!(result, vec![Ok(Token::Identifier("你好")),]);
    }

    #[test]
    fn lex_thai_identifier() {
        let result = lex("สวัสดี").collect::<Vec<_>>();
        assert_eq!(result, vec![Ok(Token::Identifier("สวัสดี")),]);
    }

    #[test]
    fn lex_arabic_identifier() {
        let result = lex("مرحبا").collect::<Vec<_>>();
        assert_eq!(result, vec![Ok(Token::Identifier("مرحبا")),]);
    }

    #[test]
    fn spaces_around_minus_are_subtraction() {
        let result = lex("foo - bar").collect::<Vec<_>>();
        assert_eq!(
            result,
            vec![
                Ok(Token::Identifier("foo")),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Minus),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("bar")),
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
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("name")),
                Ok(Token::ParenOpen),
                Ok(Token::Identifier("a")),
                Ok(Token::Comma),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::ParenClose),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::ReservedReturn),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("a")),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::HorizontalWhitespace(" ")),
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
                Ok(Token::VerticalWhitespace("\n")),
                Ok(Token::HorizontalWhitespace("        ")),
                Ok(Token::ReservedFunction),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("name")),
                Ok(Token::ParenOpen),
                Ok(Token::Identifier("a")),
                Ok(Token::Comma),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::ParenClose),
                Ok(Token::VerticalWhitespace("\n")),
                Ok(Token::HorizontalWhitespace("            ")),
                Ok(Token::ReservedReturn),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("a")),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Plus),
                Ok(Token::HorizontalWhitespace(" ")),
                Ok(Token::Identifier("b")),
                Ok(Token::VerticalWhitespace("\n")),
                Ok(Token::HorizontalWhitespace("        ")),
                Ok(Token::ReservedEnd),
            ]
        )
    }
}
