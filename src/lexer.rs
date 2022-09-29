use regex::Regex;

#[derive(Debug)]
pub struct Lexer {
    program: String,
    index: usize,
}

fn re(regex_str: &str) -> Regex {
    Regex::new(&regex_str).unwrap()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Id(String),
    Fn,
    OpenParen,
    CloseParen,
    Comma,
    Plus,
    Minus,
    Times,
    Div,
    Let,
    Eq,
}

#[derive(Debug)]
pub enum LexError {}

impl Lexer {
    pub fn new(program: String) -> Lexer {
        Lexer { program, index: 0 }
    }

    fn rest_of_string(&self) -> String {
        String::from(&self.program[self.index..])
    }

    fn scan(&mut self, regex: Regex) -> Option<String> {
        let rest_of_string = self.rest_of_string();
        let result = regex.find(&rest_of_string)?;
        if result.start() != 0 {
            return None;
        }
        self.index += result.end();
        Some(String::from(result.as_str()))
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = vec![];
        while self.index < self.program.len() {
            // println!("{:?}", self.index);
            if let Some(_) = self.scan(re(r"\s+")) {
                continue;
            } else if let Some(digit) = self.scan(re(r"\d+")) {
                tokens.push(Token::Num(digit.parse::<i32>().unwrap()))
            } else if let Some(_) = self.scan(re(r"\+")) {
                tokens.push(Token::Plus)
            } else if let Some(_) = self.scan(re(r"\*")) {
                tokens.push(Token::Times)
            } else if let Some(_) = self.scan(re(r"/")) {
                tokens.push(Token::Div)
            } else if let Some(_) = self.scan(re(r"\-")) {
                tokens.push(Token::Minus)
            } else if let Some(_) = self.scan(re(r"let\b")) {
                tokens.push(Token::Let)
            } else if let Some(_) = self.scan(re(r"=")) {
                tokens.push(Token::Eq)
            } else if let Some(_) = self.scan(re(r"fn\b")) {
                tokens.push(Token::Fn)
            } else if let Some(_) = self.scan(re(r"\(")) {
                tokens.push(Token::OpenParen)
            } else if let Some(_) = self.scan(re(r"\)")) {
                tokens.push(Token::CloseParen)
            } else if let Some(name) = self.scan(re(r"[a-zA-Z]+")) {
                tokens.push(Token::Id(name))
            } else if let Some(_) = self.scan(re(r",")) {
                tokens.push(Token::Comma)
            } else {
                panic!("Couldn't find token [{:?}]", self.rest_of_string())
            }
        }

        return Ok(tokens);
    }
}
