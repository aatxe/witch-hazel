use super::*;

#[test]
fn parse_identifier() {
    let source = "foo";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_identifier();
    assert!(parser.no_errors());
    assert_eq!(
        result,
        Expression::Identifier {
            value: String::from_str_in("foo", &allocator)
        }
    );
}

#[test]
fn parse_nil_literal() {
    let source = "nil";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    assert!(parser.no_errors());
    assert_eq!(result, Expression::NilLiteral);
}

#[test]
fn parse_boolean_literal() {
    let source = "true";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    assert!(parser.no_errors());
    assert_eq!(result, Expression::BooleanLiteral { value: true });

    let source = "false";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    assert!(parser.no_errors());
    assert_eq!(result, Expression::BooleanLiteral { value: false });
}

#[test]
fn parse_number_literal() {
    let source = "42";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    assert!(parser.no_errors());
    assert_eq!(result, Expression::NumberLiteral { value: 42.0 });
}
