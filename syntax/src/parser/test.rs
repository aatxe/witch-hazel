use super::*;

#[test]
fn parse_identifier() {
    let source = "foo";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_identifier();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
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
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
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
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(result, Expression::BooleanLiteral { value: false });
}

#[test]
fn parse_number_literal() {
    let source = "42";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(result, Expression::NumberLiteral { value: 42.0 });
}

#[test]
fn parse_function_expression() {
    let source = "function foo() end";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let result = parser.parse_simple_expression();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(
        result,
        Expression::Function {
            name: Some(String::from_str_in("foo", &allocator)),
            parameters: Vec::new_in(&allocator),
            body: Block {
                expressions: Vec::new_in(&allocator)
            }
        }
    );
}
#[test]
fn parse_multiline_function() {
    let source = "function random()\n\t4\nend";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let mut body = Vec::new_in(&allocator);
    body.push(Expression::NumberLiteral { value: 4f64 });

    let result = parser.parse_simple_expression();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(
        result,
        Expression::Function {
            name: Some(String::from_str_in("random", &allocator)),
            parameters: Vec::new_in(&allocator),
            body: Block { expressions: body }
        }
    );
}

#[test]
fn parse_function_with_arguments() {
    let source = "function foo(a, b) end";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let mut parameters = Vec::new_in(&allocator);
    parameters.push(Binding {
        name: String::from_str_in("a", &allocator),
        ty: None,
    });
    parameters.push(Binding {
        name: String::from_str_in("b", &allocator),
        ty: None,
    });

    let result = parser.parse_simple_expression();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(
        result,
        Expression::Function {
            name: Some(String::from_str_in("foo", &allocator)),
            parameters,
            body: Block {
                expressions: Vec::new_in(&allocator)
            }
        }
    );
}

#[test]
fn parse_multiline_function_with_arguments() {
    let source = "function foo(a, b)\n\t4\nend";
    let lexer = Lexer::new(source);
    let allocator = Bump::new();
    let parser = Parser::new(lexer, &allocator);

    let mut parameters = Vec::new_in(&allocator);
    parameters.push(Binding {
        name: String::from_str_in("a", &allocator),
        ty: None,
    });
    parameters.push(Binding {
        name: String::from_str_in("b", &allocator),
        ty: None,
    });

    let mut body = Vec::new_in(&allocator);
    body.push(Expression::NumberLiteral { value: 4f64 });

    let result = parser.parse_simple_expression();
    let errors = parser.errors.into_inner();

    assert_eq!(errors, Vec::new_in(&allocator));
    assert_eq!(
        result,
        Expression::Function {
            name: Some(String::from_str_in("foo", &allocator)),
            parameters,
            body: Block { expressions: body }
        }
    );
}
