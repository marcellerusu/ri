use regex::Regex;

#[derive(Debug)]
pub struct Lexer {
    program: String,
    index: usize,
}

fn re(regex_str: &str) -> Regex {
    Regex::new(regex_str).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Num(i32),
    Str(String),
    Id(String),
    Fn,
    OpenParen,
    CloseParen,
    Comma,
    Plus,
    Minus,
    Times,
    OpenSq,
    CloseSq,
    Div,
    Let,
    Eq,
    Dot,
}

impl Token {
    pub fn as_num(&self) -> Option<i32> {
        match self {
            Token::Num(num) => Some(*num),
            _ => None,
        }
    }
    pub fn as_str(&self) -> Option<String> {
        match self {
            Token::Str(string) => Some(string.clone()),
            _ => None,
        }
    }
    pub fn as_id(&self) -> Option<String> {
        match self {
            Token::Id(name) => Some(name.clone()),
            _ => None,
        }
    }
    pub fn as_fn(&self) -> Option<()> {
        match self {
            Token::Fn => Some(()),
            _ => None,
        }
    }
    pub fn as_open_paren(&self) -> Option<()> {
        match self {
            Token::OpenParen => Some(()),
            _ => None,
        }
    }
    pub fn as_close_paren(&self) -> Option<()> {
        match self {
            Token::CloseParen => Some(()),
            _ => None,
        }
    }
    pub fn as_comma(&self) -> Option<()> {
        match self {
            Token::Comma => Some(()),
            _ => None,
        }
    }
    pub fn as_plus(&self) -> Option<()> {
        match self {
            Token::Plus => Some(()),
            _ => None,
        }
    }
    pub fn as_minus(&self) -> Option<()> {
        match self {
            Token::Minus => Some(()),
            _ => None,
        }
    }
    pub fn as_times(&self) -> Option<()> {
        match self {
            Token::Times => Some(()),
            _ => None,
        }
    }
    pub fn as_open_sq(&self) -> Option<()> {
        match self {
            Token::OpenSq => Some(()),
            _ => None,
        }
    }
    pub fn as_close_sq(&self) -> Option<()> {
        match self {
            Token::CloseSq => Some(()),
            _ => None,
        }
    }
    pub fn as_div(&self) -> Option<()> {
        match self {
            Token::Div => Some(()),
            _ => None,
        }
    }
    pub fn as_let(&self) -> Option<()> {
        match self {
            Token::Let => Some(()),
            _ => None,
        }
    }
    pub fn as_eq(&self) -> Option<()> {
        match self {
            Token::Eq => Some(()),
            _ => None,
        }
    }
    pub fn as_dot(&self) -> Option<()> {
        match self {
            Token::Dot => Some(()),
            _ => None,
        }
    }
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
            if self.scan(re(r"\s+")).is_some() {
                continue;
            } else if let Some(digit) = self.scan(re(r"\d+")) {
                tokens.push(Token::Num(digit.parse::<i32>().unwrap()))
            } else if self.scan(re(r"\+")).is_some() {
                tokens.push(Token::Plus)
            } else if self.scan(re(r"\*")).is_some() {
                tokens.push(Token::Times)
            } else if self.scan(re(r"\[")).is_some() {
                tokens.push(Token::OpenSq)
            } else if self.scan(re(r"\]")).is_some() {
                tokens.push(Token::CloseSq)
            } else if self.scan(re(r"\.")).is_some() {
                tokens.push(Token::Dot)
            } else if self.scan(re(r"/")).is_some() {
                tokens.push(Token::Div)
            } else if self.scan(re(r"\-")).is_some() {
                tokens.push(Token::Minus)
            } else if self.scan(re(r"let\b")).is_some() {
                tokens.push(Token::Let)
            } else if self.scan(re(r"=")).is_some() {
                tokens.push(Token::Eq)
            } else if self.scan(re("\"")).is_some() {
                let mut string = "".to_string();
                for c in self.rest_of_string().chars() {
                    self.index += 1;
                    if c == '"' {
                        break;
                    }
                    string.push(c);
                }
                tokens.push(Token::Str(string))
            } else if self.scan(re(r"fn\b")).is_some() {
                tokens.push(Token::Fn)
            } else if self.scan(re(r"\(")).is_some() {
                tokens.push(Token::OpenParen)
            } else if self.scan(re(r"\)")).is_some() {
                tokens.push(Token::CloseParen)
            } else if let Some(name) = self.scan(re(r"[a-zA-Z_]+")) {
                tokens.push(Token::Id(name))
            } else if self.scan(re(r",")).is_some() {
                tokens.push(Token::Comma)
            } else {
                panic!("Couldn't find token [{:?}]", self.rest_of_string())
            }
        }

        Ok(tokens)
    }
}
