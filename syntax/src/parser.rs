use crate::lexer::{LexResult, Token};
use bumpalo::{collections::Vec, Bump};
use logos::Lexer;
use std::ops::Range;

type Allocator = Bump;
type Span = Range<usize>;

pub struct Parser<'alloc, 'source> {
    allocator: &'alloc Allocator,
    source: &'source str,
    tokens: Vec<'alloc, (LexResult<'source>, Span)>,
    cursor: usize,
}

impl<'alloc, 'source> Parser<'alloc, 'source> {
    pub fn new(lexer: Lexer<'source, Token<'source>>, allocator: &'alloc Allocator) -> Self {
        let source = lexer.source();
        let tokens = Vec::from_iter_in(lexer.spanned(), allocator);
        Parser {
            allocator,
            source,
            tokens,
            cursor: 0,
        }
    }

    fn peek(&self) -> Option<(LexResult<'source>, Span)> {
        // tokens are intentionally cheap to clone
        self.tokens.get(self.cursor).cloned()
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn next(&mut self) -> Option<(LexResult<'source>, Span)> {
        let next = self.peek();
        self.advance();
        next
    }

    fn skip_whitespace(&mut self) {
        while let Some((Ok(Token::Whitespace(_)), _)) = self.peek() {
            self.advance();
        }
    }
}
