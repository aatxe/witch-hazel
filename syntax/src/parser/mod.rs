use crate::{
    ast::*,
    lexer::{LexResult, Token},
};
use bumpalo::{
    boxed::Box,
    collections::{String, Vec},
    Bump,
};
use logos::Lexer;
use std::cell::RefCell;
use std::ops::Range;

type Allocator = Bump;
type Span = Range<usize>;

#[cfg(test)]
mod test;

#[derive(Debug, PartialEq)]
pub enum ParseError<'source> {
    UnexpectedEof,
    IllegalToken(Span),
    ExpectedIdentifier(LexResult<'source>, Span),
    ExpectedNewLineOrSemicolon(LexResult<'source>, Span),
    UnexpectedToken(LexResult<'source>, Span),

    ExpectedClosingBracket {
        expected: Token<'source>,
        opening_span: Span,
        found: LexResult<'source>,
        span: Span,
    },
}

pub type ParseResult<'source, T> = Result<T, ParseError<'source>>;

pub struct Parser<'alloc, 'source> {
    allocator: &'alloc Allocator,
    source: &'source str,
    tokens: Vec<'alloc, (LexResult<'source>, Span)>,
    errors: RefCell<Vec<'alloc, ParseError<'source>>>,
    cursor: RefCell<usize>,
}

impl<'alloc, 'source> Parser<'alloc, 'source> {
    pub fn new(lexer: Lexer<'source, Token<'source>>, allocator: &'alloc Allocator) -> Self {
        let source = lexer.source();
        let tokens = Vec::from_iter_in(lexer.spanned(), allocator);

        Parser {
            allocator,
            source,
            tokens,
            errors: RefCell::new(Vec::new_in(allocator)),
            cursor: RefCell::new(0),
        }
    }

    pub fn no_errors(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    // mostly here to make the linter leave me alone, may replace it with something more useful later
    pub fn slice_source(&self, span: Span) -> &'source str {
        &self.source[span]
    }

    fn peek(&self) -> Option<(LexResult<'source>, Span)> {
        // tokens are intentionally cheap to clone
        self.tokens.get(*self.cursor.borrow()).cloned()
    }

    fn advance(&self) {
        let mut cursor = self.cursor.borrow_mut();
        *cursor += 1;
    }

    fn next(&self) -> Option<(LexResult<'source>, Span)> {
        let next = self.peek();
        self.advance();
        next
    }

    fn skip_whitespace(&self) {
        while let Some((Ok(Token::Whitespace(_) | Token::NewLine), _)) = self.peek() {
            self.advance();
        }
    }

    fn skip_horizontal_whitespace(&self) {
        while let Some((Ok(Token::Whitespace(_)), _)) = self.peek() {
            self.advance();
        }
    }

    pub fn parse_identifier(&self) -> Expression<'alloc> {
        // skip leading whitespace
        self.skip_whitespace();

        match self.next() {
            Some((Ok(Token::Identifier(ident)), _)) => Expression::Identifier {
                value: String::from_str_in(ident, self.allocator),
            },

            Some((Ok(token), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::ExpectedIdentifier(Ok(token), span));

                Expression::Error
            }

            Some((Err(_), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::IllegalToken(span));

                Expression::Error
            }

            None => {
                self.errors.borrow_mut().push(ParseError::UnexpectedEof);

                Expression::Error
            }
        }
    }

    pub fn parse_parameter_list(&self) -> Bindings<'alloc> {
        self.skip_whitespace();

        let mut bindings = Vec::new_in(self.allocator);

        match self.next() {
            Some((Ok(Token::ParenOpen), opening_span)) => {
                self.skip_whitespace();

                if let Some((Ok(Token::ParenClose), _)) = self.peek() {
                    self.advance();
                    return bindings;
                }

                while let Some((Ok(Token::Identifier(ident)), _)) = self.next() {
                    let name = String::from_str_in(ident, self.allocator);
                    bindings.push(Binding { name, ty: None });
                    self.skip_whitespace();

                    match self.peek() {
                        Some((Ok(Token::Comma), _)) => {
                            self.advance();
                            self.skip_whitespace();
                        }

                        Some((Ok(Token::ParenClose), _)) => {
                            self.advance();
                            break;
                        }

                        Some((Ok(token), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::ExpectedClosingBracket {
                                    expected: Token::ParenClose,
                                    opening_span,
                                    found: Ok(token),
                                    span,
                                });
                            break;
                        }
                        Some((Err(_), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::IllegalToken(span));
                            break;
                        }
                        None => {
                            self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                            break;
                        }
                    }
                }
            }

            Some((Ok(token), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::UnexpectedToken(Ok(token), span));
            }

            Some((Err(_), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::IllegalToken(span));
            }

            None => {
                self.errors.borrow_mut().push(ParseError::UnexpectedEof);
            }
        }

        bindings
    }

    fn should_end_block(&self) -> bool {
        let Some((Ok(lexeme), _)) = self.peek() else {
            return true;
        };

        lexeme == Token::ReservedEnd
            || lexeme == Token::ReservedElse
            || lexeme == Token::ReservedElseif
            || lexeme == Token::ReservedUntil
    }

    pub fn parse_block(&self) -> Block<'alloc> {
        self.skip_whitespace();

        let mut expressions = Vec::new_in(self.allocator);

        if self.should_end_block() {
            return Block { expressions };
        }

        expressions.push(self.parse_expression());
        self.skip_whitespace();

        while !self.should_end_block() {
            expressions.push(self.parse_expression());
            self.skip_horizontal_whitespace();

            match self.peek() {
                Some((Ok(Token::NewLine | Token::SemiColon), _)) => {
                    self.advance();
                }

                Some((
                    Ok(
                        Token::ReservedEnd
                        | Token::ReservedElseif
                        | Token::ReservedElse
                        | Token::ReservedUntil,
                    ),
                    _,
                )) => {
                    self.advance();
                    break;
                }

                Some((Ok(token), span)) => {
                    self.errors
                        .borrow_mut()
                        .push(ParseError::ExpectedNewLineOrSemicolon(Ok(token), span));
                    break;
                }

                Some((Err(_), span)) => {
                    self.errors
                        .borrow_mut()
                        .push(ParseError::IllegalToken(span));
                    break;
                }

                None => {
                    self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                    break;
                }
            }

            self.skip_horizontal_whitespace();
        }

        Block { expressions }
    }

    // argument_list ::= '(' expression { ',' expression } ')'
    pub fn parse_argument_list(&self) -> Expressions<'alloc> {
        self.skip_whitespace();

        let mut arguments = Vec::new_in(self.allocator);

        match self.next() {
            Some((Ok(Token::ParenOpen), _)) => {
                self.skip_whitespace();

                if let Some((Ok(Token::ParenClose), _)) = self.peek() {
                    self.advance();
                    return arguments;
                }

                while let Some((Ok(token), _)) = self.peek() {
                    if token == Token::ParenClose {
                        self.advance();
                        return arguments;
                    }

                    if token == Token::Comma {
                        self.advance();
                        continue;
                    }

                    let expression = self.parse_expression();
                    arguments.push(expression);
                    self.skip_whitespace();
                }
            }

            Some((Ok(token), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::UnexpectedToken(Ok(token), span));
            }

            Some((Err(_), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::IllegalToken(span));
            }

            None => {
                self.errors.borrow_mut().push(ParseError::UnexpectedEof);
            }
        }

        arguments
    }

    pub fn parse_expression(&self) -> Expression<'alloc> {
        self.parse_simple_expression()
    }

    // prefix_expression ::= NAME | '(' expression ')'
    pub fn parse_prefix_expression(&self) -> Expression<'alloc> {
        self.skip_whitespace();

        if let Some((Ok(Token::ParenOpen), opening_span)) = self.peek() {
            self.advance();

            self.skip_whitespace();
            let expression = self.parse_expression();
            self.skip_whitespace();

            return match self.next() {
                Some((Ok(Token::ParenClose), _)) => expression,

                Some((Ok(token), span)) => {
                    self.errors
                        .borrow_mut()
                        .push(ParseError::ExpectedClosingBracket {
                            expected: Token::ParenClose,
                            opening_span,
                            found: Ok(token),
                            span,
                        });
                    Expression::Error
                }

                Some((Err(_), span)) => {
                    self.errors
                        .borrow_mut()
                        .push(ParseError::IllegalToken(span));
                    Expression::Error
                }

                None => {
                    self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                    Expression::Error
                }
            };
        }

        self.parse_identifier()
    }

    // primary_expression ::= prefix_expression { '.' NAME | '[' expression ']' | ':' NAME parameter_list | parameter_list }
    pub fn parse_primary_expression(&self) -> Expression<'alloc> {
        let mut expression = self.parse_prefix_expression();
        self.skip_whitespace();

        loop {
            match self.peek() {
                Some((Ok(Token::Dot), _)) => {
                    self.advance();
                    self.skip_whitespace();

                    match self.next() {
                        Some((Ok(Token::Identifier(name)), _)) => {
                            let name = String::from_str_in(name, self.allocator);
                            expression = Expression::IndexName {
                                operator: IndexOperator::Dot,
                                expression: Box::new_in(expression, self.allocator),
                                name,
                            };
                        }

                        Some((Ok(token), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::ExpectedIdentifier(Ok(token), span));
                        }

                        Some((Err(_), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::IllegalToken(span));
                        }

                        None => {
                            self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                        }
                    }
                }

                Some((Ok(Token::Colon), _)) => {
                    self.advance();
                    self.skip_whitespace();

                    match self.next() {
                        Some((Ok(Token::Identifier(name)), _)) => {
                            let name = String::from_str_in(name, self.allocator);
                            expression = Expression::IndexName {
                                operator: IndexOperator::Colon,
                                expression: Box::new_in(expression, self.allocator),
                                name,
                            };

                            let arguments = self.parse_argument_list();
                            expression = Expression::Call {
                                function: Box::new_in(expression, self.allocator),
                                arguments,
                            };
                        }

                        Some((Ok(token), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::ExpectedIdentifier(Ok(token), span));
                        }

                        Some((Err(_), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::IllegalToken(span));
                        }

                        None => {
                            self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                        }
                    }
                }

                Some((Ok(Token::BracketOpen), opening_span)) => {
                    self.advance();
                    let index = self.parse_expression();
                    self.skip_whitespace();

                    match self.next() {
                        Some((Ok(Token::BracketClose), _)) => {
                            expression = Expression::IndexExpression {
                                expression: Box::new_in(expression, self.allocator),
                                index: Box::new_in(index, self.allocator),
                            };
                        }

                        Some((Ok(token), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::ExpectedClosingBracket {
                                    expected: Token::BracketClose,
                                    opening_span,
                                    found: Ok(token),
                                    span,
                                });
                        }

                        Some((Err(_), span)) => {
                            self.errors
                                .borrow_mut()
                                .push(ParseError::IllegalToken(span));
                        }

                        None => {
                            self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                        }
                    }
                }

                Some((Ok(Token::ParenOpen), _)) => {
                    let arguments = self.parse_argument_list();
                    expression = Expression::Call {
                        function: Box::new_in(expression, self.allocator),
                        arguments,
                    };
                }

                Some((Ok(_), _)) => {
                    break;
                }

                Some((Err(_), span)) => {
                    self.errors
                        .borrow_mut()
                        .push(ParseError::IllegalToken(span));
                    break;
                }

                None => {
                    break;
                }
            }
        }

        expression
    }

    // function_expression ::= ('function' | 'fn') [NAME] '(' parameter_list ')' block
    // simple_expression ::= NUMBER | 'true' | 'false' | 'nil' | function_expression | primary_expression
    pub fn parse_simple_expression(&self) -> Expression<'alloc> {
        // skip leading whitespace
        self.skip_whitespace();

        match self.peek() {
            // literal numbers
            Some((Ok(Token::NumberLiteral(value)), _)) => {
                self.advance();
                Expression::NumberLiteral { value }
            }

            // `true` or `false`
            Some((Ok(Token::BooleanLiteral(value)), _)) => {
                self.advance();
                Expression::BooleanLiteral { value }
            }

            // `nil`
            Some((Ok(Token::ReservedNil), _)) => {
                self.advance();
                Expression::NilLiteral
            }

            // function expression
            Some((Ok(Token::ReservedFunction | Token::ReservedFn), _)) => {
                self.advance();
                self.skip_whitespace();

                let name = match self.peek() {
                    Some((Ok(Token::Identifier(ident)), _)) => {
                        self.advance();
                        Some(String::from_str_in(ident, self.allocator))
                    }
                    _ => None,
                };

                let parameters = self.parse_parameter_list();
                let body = self.parse_block();

                Expression::Function {
                    name,
                    parameters,
                    body,
                }
            }

            Some((Ok(_), _)) => self.parse_primary_expression(),

            // illegal tokens become error expressions
            Some((Err(_), span)) => {
                self.errors
                    .borrow_mut()
                    .push(ParseError::IllegalToken(span));
                Expression::Error
            }

            // unexpected EOF
            None => {
                self.errors.borrow_mut().push(ParseError::UnexpectedEof);
                Expression::Error
            }
        }
    }
}
