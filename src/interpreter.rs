use std::collections::HashMap;

use crate::parser::AST;

pub struct Interpreter {
    ast: Vec<AST>,
    vars: HashMap<String, i32>,
}

impl Interpreter {
    pub fn new(ast: Vec<AST>) -> Interpreter {
        Interpreter {
            ast,
            vars: HashMap::new(),
        }
    }

    fn add(&mut self, lhs: &AST, rhs: &AST) -> i32 {
        match [lhs, rhs] {
            [AST::Num(lhs), AST::Num(rhs)] => lhs + rhs,
            [AST::Id(name), AST::Num(rhs)] => match self.vars.get(name) {
                Some(val) => val + rhs,
                None => panic!("Couldn't find value for var: {:?}", name),
            },
            _ => panic!("expected numbers"),
        }
    }

    fn assign(&mut self, id: &AST, num: &AST) {
        match (id, num) {
            (AST::Id(id), AST::Num(num)) => {
                self.vars.insert(id.clone(), num.clone());
                ()
            }
            _ => panic!("umm"),
        }
    }

    pub fn eval(&mut self) -> i32 {
        let mut result = 0;
        for node in self.ast.clone() {
            match node {
                AST::Add(lhs, rhs) => {
                    result = self.add(lhs.as_ref(), rhs.as_ref());
                    ()
                }
                AST::Let(id, num) => {
                    self.assign(id.as_ref(), num.as_ref());
                    ()
                }
                _ => panic!("unexpected {:?}", node),
            }
        }
        result
    }
}
