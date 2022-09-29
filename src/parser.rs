use std::vec;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

#[derive(Debug, Clone)]
pub enum AST {
    Num(i32),
    Add(Box<AST>, Box<AST>),
    Id(String),
    Let(Box<AST>, Box<AST>),
}

#[derive(Debug)]
pub enum ParseError {
    TokenMismatch(Token, Token),
    Unexpected(Token),
    NoMoreTokens,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    fn cur(&self) -> Option<Token> {
        self.tokens.get(self.index).map(|val| val.clone())
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.index + 1).map(|val| val.clone())
    }

    fn consume(&mut self, cond_fn: fn(Token) -> Option<AST>) -> Result<AST, ParseError> {
        let token = self.cur().ok_or(ParseError::NoMoreTokens)?;
        if let Some(result) = cond_fn(token.clone()) {
            self.index += 1;
            Ok(result)
        } else {
            println!("Unexpected");
            Err(ParseError::Unexpected(token))
        }
    }

    fn consume_and_ignore(&mut self, cond_fn: fn(Token) -> bool) -> Result<(), ParseError> {
        let token = self.cur().ok_or(ParseError::NoMoreTokens)?;
        if cond_fn(token.clone()) {
            self.index += 1;
            Ok(())
        } else {
            println!("Unexpected ignore");
            Err(ParseError::Unexpected(token))
        }
    }

    fn consume_plus_operand(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Num(num) => Some(AST::Num(num)),
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })
    }

    fn consume_plus(&mut self) -> Result<(), ParseError> {
        self.consume_and_ignore(|t| match t {
            Token::Plus => true,
            _ => false,
        })
    }

    fn parse_plus(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_plus_operand()?;
        self.consume_plus()?;
        let rhs = self.consume_plus_operand()?;
        Ok(AST::Add(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_let(&mut self) -> Result<AST, ParseError> {
        self.consume_and_ignore(|t| match t {
            Token::Let => true,
            _ => false,
        })?;
        let name = self.consume(|t| match t {
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })?;
        self.consume_and_ignore(|t| match t {
            Token::Eq => true,
            _ => false,
        })?;
        let rhs = self.parse_node()?;
        Ok(AST::Let(Box::new(name), Box::new(rhs)))
    }

    fn parse_node(&mut self) -> Result<AST, ParseError> {
        match (self.cur(), self.peek()) {
            (_, Some(Token::Plus)) => self.parse_plus(),
            (Some(Token::Let), Some(Token::Id(_))) => self.parse_let(),
            (Some(Token::Num(_)), _) => self.consume(|t| match t {
                Token::Num(num) => Some(AST::Num(num)),
                _ => None,
            }),
            (None, _) => Err(ParseError::NoMoreTokens),
            _ => {
                println!("Unimplemented token");
                Err(ParseError::Unexpected(self.cur().unwrap()))
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>, ParseError> {
        let mut ast: Vec<AST> = vec![];
        while self.index < self.tokens.len() {
            ast.push(self.parse_node()?);
        }
        return Ok(ast);
    }
}
