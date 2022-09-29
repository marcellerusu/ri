use std::collections::HashMap;

use crate::parser::AST;
#[derive(Clone)]
struct Fn {
    args: Vec<String>,
    expr: AST,
}

#[derive(Clone)]
pub struct Interpreter {
    ast: Vec<AST>,
    vars: HashMap<String, i32>,
    fns: HashMap<String, Fn>,
}

impl Interpreter {
    pub fn new(ast: Vec<AST>) -> Interpreter {
        Interpreter {
            ast,
            vars: HashMap::new(),
            fns: HashMap::new(),
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

    fn define(&mut self, name: String, args: Vec<String>, expr: &AST) {
        self.fns.insert(
            name,
            Fn {
                args,
                expr: expr.clone(),
            },
        );
    }

    fn fn_call(&mut self, name: String, args: Vec<AST>) -> i32 {
        match self.clone().fns.get(&name) {
            Some(Fn {
                args: fn_args,
                expr,
            }) => {
                for (arg_name, arg_expr) in fn_args.iter().zip(args) {
                    let expr = self.eval_expr(arg_expr);
                    self.vars.insert(arg_name.clone(), expr);
                }
                let result = self.eval_expr(expr.clone());
                for arg_name in fn_args.iter() {
                    self.vars.remove(arg_name);
                }
                result
            }
            None => {
                panic!("Function not found {:?}", name)
            }
        }
    }

    fn eval_expr(&mut self, expr: AST) -> i32 {
        match expr {
            AST::Add(lhs, rhs) => self.add(lhs.as_ref(), rhs.as_ref()),
            AST::Div(lhs, rhs) => self.div(lhs.as_ref(), rhs.as_ref()),
            AST::Mul(lhs, rhs) => self.mul(lhs.as_ref(), rhs.as_ref()),
            AST::Minus(lhs, rhs) => self.minus(lhs.as_ref(), rhs.as_ref()),
            AST::Num(num) => num,
            AST::Id(name) => self.vars.get(&name).unwrap().clone(),
            AST::FnCall(name, args) => self.fn_call(name, args),
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
                AST::Def(name, args, expr) => {
                    self.define(name, args, expr.as_ref());
                }
                _ => result = self.eval_expr(node),
            }
        }
        result
    }
}
