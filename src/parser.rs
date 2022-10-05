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
    Str(String),
    Array(Vec<AST>),
    Dot(Box<AST>, String),
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
        self.tokens.get(self.index).cloned()
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.index + 1).cloned()
    }

    fn consume<T>(&mut self, cond_fn: fn(Token) -> Option<T>) -> Result<T, ParseError> {
        let token = self.cur().ok_or(ParseError::NoMoreTokens)?;
        if let Some(result) = cond_fn(token) {
            self.index += 1;
            Ok(result)
        } else {
            panic!("Unexpected {:?}", &self.tokens.as_slice()[self.index..]);
        }
    }

    fn consume_math_operand(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Num(num) => Some(AST::Num(num)),
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })
    }

    fn consume_plus_operand(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| match t {
            Token::Num(num) => Some(AST::Num(num)),
            Token::Str(string) => Some(AST::Str(string)),
            Token::Id(name) => Some(AST::Id(name)),
            _ => None,
        })
    }

    fn parse_plus(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_plus_operand()?;
        self.consume(|t| t.as_plus())?;
        let rhs = self.parse_node()?;
        Ok(AST::Add(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_mul(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| t.as_times())?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Mul(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_div(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| t.as_div())?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Div(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_minus(&mut self) -> Result<AST, ParseError> {
        let lhs = self.consume_math_operand()?;
        self.consume(|t| t.as_minus())?;
        let rhs = self.consume_math_operand()?;
        Ok(AST::Minus(Box::new(lhs), Box::new(rhs)))
    }

    fn parse_let(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| t.as_let())?;
        let name = self.consume(|t| t.as_id())?;
        self.consume(|t| t.as_eq())?;
        let rhs = self.parse_node()?;
        Ok(AST::Let(Box::new(AST::Id(name)), Box::new(rhs)))
    }

    fn parse_comma_separated_ids(&mut self) -> Result<Vec<String>, ParseError> {
        let mut names: Vec<String> = vec![];
        loop {
            names.push(self.consume(|t| t.as_id())?);
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
        Ok(names)
    }

    fn parse_comma_separated_exprs(
        &mut self,
        closing_token: Token,
    ) -> Result<Vec<AST>, ParseError> {
        let mut exprs: Vec<AST> = vec![];
        loop {
            exprs.push(self.parse_node()?);
            if let Some(token) = self.cur() {
                if token == closing_token {
                    self.index += 1;
                    break;
                } else if matches!(token, Token::Comma) {
                    self.index += 1;
                }
            }
        }
        Ok(exprs)
    }

    fn parse_def(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| t.as_fn())?;
        let name = self.consume(|t| t.as_id())?;
        self.consume(|t| t.as_open_paren())?;
        let args = self.parse_comma_separated_ids()?;
        self.consume(|t| t.as_eq())?;
        let expr = self.parse_node()?;
        Ok(AST::Def(name, args, Box::new(expr)))
    }

    fn parse_fn_call(&mut self) -> Result<AST, ParseError> {
        let name = self.consume(|t| t.as_id())?;
        self.consume(|t| t.as_open_paren())?;
        let args = self.parse_comma_separated_exprs(Token::CloseParen)?;
        Ok(AST::FnCall(name, args))
    }

    fn parse_array(&mut self) -> Result<AST, ParseError> {
        self.consume(|t| t.as_open_sq())?;
        let array = self.parse_comma_separated_exprs(Token::CloseSq)?;
        Ok(AST::Array(array))
    }

    fn parse_dot(&mut self) -> Result<AST, ParseError> {
        let lhs_name = self.consume(|t| t.as_id())?;
        self.consume(|t| t.as_dot())?;
        let property = self.consume(|t| t.as_id())?;
        Ok(AST::Dot(Box::new(AST::Id(lhs_name)), property))
    }

    fn parse_node(&mut self) -> Result<AST, ParseError> {
        match (self.cur(), self.peek()) {
            (_, Some(Token::Plus)) => self.parse_plus(),
            (_, Some(Token::Minus)) => self.parse_minus(),
            (_, Some(Token::Div)) => self.parse_div(),
            (_, Some(Token::Times)) => self.parse_mul(),
            (Some(Token::Id(_)), Some(Token::Dot)) => self.parse_dot(),
            (Some(Token::Id(_)), Some(Token::OpenParen)) => self.parse_fn_call(),
            (Some(Token::Num(_)), _) => self.consume(|t| t.as_num().map(AST::Num)),
            (Some(Token::Str(_)), _) => self.consume(|t| t.as_str().map(AST::Str)),
            (Some(Token::Id(_)), _) => self.consume(|t| t.as_id().map(AST::Id)),
            (Some(Token::Fn), _) => self.parse_def(),
            (Some(Token::Let), Some(Token::Id(_))) => self.parse_let(),
            (Some(Token::OpenSq), _) => self.parse_array(),
            (None, _) => Err(ParseError::NoMoreTokens),
            _ => {
                panic!("Unimplemented token {:?} {:?}", self.cur(), self.peek())
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AST>, ParseError> {
        let mut ast: Vec<AST> = vec![];
        while self.index < self.tokens.len() {
            ast.push(self.parse_node()?);
        }
        Ok(ast)
    }
}
