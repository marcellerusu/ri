use parser::ParseError;

pub mod interpreter;
pub mod lexer;
pub mod parser;

fn eval(file_contents: String) -> Result<(), ParseError> {
    let mut lex = lexer::Lexer::new(file_contents);
    match lex.tokenize() {
        Ok(tokens) => {
            let mut parser = parser::Parser::new(tokens);
            let ast = parser.parse()?;
            let mut interpreter = interpreter::Interpreter::new(ast);
            let output = interpreter.eval();
            println!("{:?}", output);
        }
        Err(err) => {
            println!("failed to tokenize {:?}", err);
        }
    }
    Ok(())
}

fn main() {
    let filename = std::env::args().skip(1).next();
    match filename {
        Some(filename) => {
            let result = std::fs::read_to_string(&filename).unwrap();
            if let Err(err) = eval(result) {
                println!("Error {:?}", err);
            }
        }
        None => {
            println!("Please input a filename")
        }
    }
    ()
}
