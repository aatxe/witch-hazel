use crate::{
    ast::*,
    lexer::{LexResult, Token},
};
use bumpalo::{
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
    UnexpectedToken(LexResult<'source>, Span),
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
        panic!("unimplemented")
    }

    pub fn parse_block(&self) -> Block<'alloc> {
        panic!("unimplemented")
    }

    pub fn parse_primary_expression(&self) -> Expression<'alloc> {
        panic!("unimplemented")
    }

    pub fn parse_simple_expression(&self) -> Expression<'alloc> {
        // skip leading whitespace
        self.skip_whitespace();

        match self.next() {
            // literal numbers
            Some((Ok(Token::NumberLiteral(value)), _)) => Expression::NumberLiteral { value },

            // `true` or `false`
            Some((Ok(Token::BooleanLiteral(value)), _)) => Expression::BooleanLiteral { value },

            // `nil`
            Some((Ok(Token::ReservedNil), _)) => Expression::NilLiteral,

            // function expression
            Some((Ok(Token::ReservedFunction | Token::ReservedFn), _)) => {
                let parameters = self.parse_parameter_list();
                let body = self.parse_block();

                Expression::Function { parameters, body }
            }

            Some((Ok(_), _)) => self.parse_primary_expression(),

            // illegal tokens become error expressions, should probably surface some error as well
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
