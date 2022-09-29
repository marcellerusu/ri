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
    Div(Box<AST>, Box<AST>),
    Mul(Box<AST>, Box<AST>),
    Minus(Box<AST>, Box<AST>),
    Id(String),
    Let(Box<AST>, Box<AST>),
    Def(String, Vec<String>, Box<AST>),
    FnCall(String, Vec<AST>),
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

    fn consume<T>(&mut self, cond_fn: fn(Token) -> Option<T>) -> Result<T, ParseError> {
        let token = self.cur().ok_or(ParseError::NoMoreTokens)?;
        if let Some(result) = cond_fn(token.clone()) {
            self.index += 1;
            Ok(result)
        } else {
            println!("Unexpected");
            Err(ParseError::Unexpected(token))
        }
    }

    fn consume_math_operand(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Num(num) => Some(AST::Num(num)),
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })
    }

    fn parse_plus(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| match t {
            Token::Plus => Some(()),
            _ => None,
        })?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Add(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_mul(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| match t {
            Token::Times => Some(()),
            _ => None,
        })?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Mul(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_div(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| match t {
            Token::Div => Some(()),
            _ => None,
        })?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Div(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_minus(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| match t {
            Token::Minus => Some(()),
            _ => None,
        })?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Minus(Box::new(lhs), Box::new(rhs)))
    }
    fn parse_let(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Let => Some(()),
            _ => None,
        })?;
        let name = self.consume(|t| match t {
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })?;
        self.consume(|t| match t {
            Token::Eq => Some(()),
            _ => None,
        })?;
        let rhs = self.parse_node()?;
        Ok(AST::Let(Box::new(name), Box::new(rhs)))
    }

    fn parse_def(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Fn => Some(()),
            _ => None,
        })?;
        let name = self.consume(|t| match t {
            Token::Id(name) => Some(name),
            _ => None,
        })?;
        self.consume(|t| match t {
            Token::OpenParen => Some(()),
            _ => None,
        })?;
        let mut args: Vec<String> = vec![];
        loop {
            let arg_name = self.consume(|t| match t {
                Token::Id(name) => Some(name),
                _ => None,
            })?;
            args.push(arg_name);
            match self.cur() {
                Some(Token::CloseParen) => {
                    self.index += 1;
                    break;
                }
                Some(Token::Comma) => {
                    self.index += 1;
                }
                _ => continue,
            }
        }

        self.consume(|t| match t {
            Token::Eq => Some(()),
            _ => None,
        })?;

        let expr = self.parse_node()?;

        Ok(AST::Def(name, args, Box::new(expr)))
    }

    fn parse_fn_call(&mut self) -> Result<AST, ParseError> {
        let name = self.consume(|t| match t {
            Token::Id(fn_name) => Some(fn_name),
            _ => None,
        })?;
        self.consume(|t| match t {
            Token::OpenParen => Some(()),
            _ => None,
        })?;

        let mut args: Vec<AST> = vec![];
        loop {
            args.push(self.parse_node()?);
            match self.cur() {
                Some(Token::CloseParen) => {
                    self.index += 1;
                    break;
                }
                Some(Token::Comma) => {
                    self.index += 1;
                }
                _ => continue,
            }
        }

        Ok(AST::FnCall(name, args))
    }

    fn parse_node(&mut self) -> Result<AST, ParseError> {
        match (self.cur(), self.peek()) {
            (_, Some(Token::Plus)) => self.parse_plus(),
            (_, Some(Token::Minus)) => self.parse_minus(),
            (_, Some(Token::Div)) => self.parse_div(),
            (_, Some(Token::Times)) => self.parse_mul(),
            (Some(Token::Id(_)), Some(Token::OpenParen)) => self.parse_fn_call(),
            (Some(Token::Fn), _) => self.parse_def(),
            (Some(Token::Let), Some(Token::Id(_))) => self.parse_let(),
            (Some(Token::Num(_)), _) => self.consume(|t| match t {
                Token::Num(num) => Some(AST::Num(num)),
                _ => None,
            }),
            (Some(Token::Id(_)), _) => self.consume(|t| match t {
                Token::Id(name) => Some(AST::Id(name)),
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
