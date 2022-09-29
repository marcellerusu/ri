use crate::parser::AST;

pub struct Interpreter {
    ast: Vec<AST>,
}

impl Interpreter {
    pub fn new(ast: Vec<AST>) -> Interpreter {
        Interpreter { ast }
    }

    fn add(&self, lhs: &AST, rhs: &AST) -> i32 {
        match [lhs, rhs] {
            [AST::Num(lhs), AST::Num(rhs)] => lhs + rhs,
            _ => panic!("expected nums"),
        }
    }

    pub fn eval(&self) -> i32 {
        let node = self.ast.first();
        match &node {
            Some(AST::Add(lhs, rhs)) => self.add(lhs.as_ref(), rhs.as_ref()),
            _ => todo!(),
        }
    }
}
