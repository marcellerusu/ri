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

    fn to_i(&self, node: &AST) -> i32 {
        match node.clone() {
            AST::Num(num) => num,
            AST::Id(name) => match self.vars.get(&name) {
                Some(val) => val.clone(),
                None => panic!("Couldn't find value for var: {:?}", name),
            },
            _ => panic!("expected numbers"),
        }
    }

    fn add(&mut self, lhs: &AST, rhs: &AST) -> i32 {
        let lhs = self.to_i(lhs);
        let rhs = self.to_i(rhs);
        lhs + rhs
    }

    fn minus(&mut self, lhs: &AST, rhs: &AST) -> i32 {
        let lhs = self.to_i(lhs);
        let rhs = self.to_i(rhs);
        lhs - rhs
    }

    fn div(&mut self, lhs: &AST, rhs: &AST) -> i32 {
        let lhs = self.to_i(lhs);
        let rhs = self.to_i(rhs);
        lhs / rhs
    }

    fn mul(&mut self, lhs: &AST, rhs: &AST) -> i32 {
        let lhs = self.to_i(lhs);
        let rhs = self.to_i(rhs);
        lhs * rhs
    }

    fn assign(&mut self, id: &AST, rhs: &AST) {
        match id {
            AST::Id(id) => {
                let rhs = self.eval_expr(rhs.clone());
                self.vars.insert(id.clone(), rhs);
                ()
            }
            _ => panic!("umm"),
        }
    }

    fn eval_expr(&mut self, expr: AST) -> i32 {
        match expr {
            AST::Add(lhs, rhs) => self.add(lhs.as_ref(), rhs.as_ref()),
            AST::Div(lhs, rhs) => self.div(lhs.as_ref(), rhs.as_ref()),
            AST::Mul(lhs, rhs) => self.mul(lhs.as_ref(), rhs.as_ref()),
            AST::Minus(lhs, rhs) => self.minus(lhs.as_ref(), rhs.as_ref()),
            AST::Num(num) => num,
            _ => panic!("unexpected {:?}", expr),
        }
    }

    pub fn eval(&mut self) -> i32 {
        let mut result = 0;
        for node in self.ast.clone() {
            match node {
                AST::Let(id, expr) => {
                    self.assign(id.as_ref(), expr.as_ref());
                }
                _ => result = self.eval_expr(node),
            }
        }
        result
    }
}
