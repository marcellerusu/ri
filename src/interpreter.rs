use std::collections::HashMap;

use crate::parser::AST;
#[derive(Clone)]
struct Fn {
    args: Vec<String>,
    expr: AST,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    Str(String),
    Array(Vec<Value>),
    Nil,
}

#[derive(Clone)]
pub struct Interpreter {
    ast: Vec<AST>,
    vars: HashMap<String, Value>,
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

    fn new_with_context(
        ast: Vec<AST>,
        vars: HashMap<String, Value>,
        fns: HashMap<String, Fn>,
    ) -> Interpreter {
        Interpreter { ast, vars, fns }
    }

    fn to_i(&self, node: &AST) -> i32 {
        match node.clone() {
            AST::Num(num) => num,
            AST::Id(name) => match self.vars.get(&name) {
                Some(val) => match val {
                    Value::Int(num) => *num,
                    non_int => panic!("Can't convert {:?} to an integer", non_int),
                },
                None => panic!("Couldn't find value for var: {:?}", name),
            },
            _ => panic!("expected numbers"),
        }
    }

    fn add_i32(&self, lhs: i32, rhs: &AST) -> Value {
        if let AST::Num(rhs) = rhs {
            Value::Int(lhs + rhs)
        } else {
            panic!("Expected {:?} to be an int", rhs)
        }
    }

    fn add_str(&self, lhs: String, rhs: &AST) -> Value {
        if let AST::Str(rhs) = rhs {
            Value::Str(lhs + rhs)
        } else if let AST::Id(name) = rhs {
            if let Some(Value::Str(rhs)) = self.vars.get(name) {
                Value::Str(lhs + rhs)
            } else {
                panic!("Expected {:?} to be an str {:?}", rhs, self.vars)
            }
        } else {
            panic!("Expected {:?} to be an str {:?}", rhs, self.vars)
        }
    }

    fn add_value(&self, lhs: Value, rhs: Value) -> Value {
        match (lhs, rhs) {
            (Value::Str(lhs), Value::Str(rhs)) => Value::Str(lhs + &rhs),
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            _ => panic!("invalid add"),
        }
    }

    fn add(&mut self, lhs: &AST, rhs: &AST) -> Value {
        match lhs {
            AST::Num(lhs) => self.add_i32(*lhs, rhs),
            AST::Id(lhs_name) => match self.vars.get(lhs_name) {
                Some(Value::Int(lhs)) => self.add_i32(*lhs, rhs),
                Some(Value::Str(lhs)) => self.add_str(lhs.clone(), rhs),
                Some(Value::Array(_)) => todo!("Array + not implemented yet"),
                Some(Value::Nil) => panic!("Can not add {:?} to nil", lhs),
                None => panic!("Could not find variable {}", lhs_name),
            },
            AST::Str(lhs) => self.add_str(lhs.clone(), rhs),
            _ => panic!("[{:?} + {:?}] not supported", lhs, rhs),
        }
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

    fn assign(&mut self, id: &AST, rhs: &AST) -> Value {
        match id {
            AST::Id(id) => {
                let rhs = self.eval_expr(rhs.clone());
                self.vars.insert(id.clone(), rhs);
                Value::Nil
            }
            _ => panic!("umm"),
        }
    }

    fn define(&mut self, name: String, args: Vec<String>, expr: &AST) -> Value {
        self.fns.insert(
            name,
            Fn {
                args,
                expr: expr.clone(),
            },
        );
        Value::Nil
    }

    fn fn_call(&mut self, name: String, args: Vec<AST>) -> Value {
        match self.clone().fns.get(&name) {
            Some(Fn {
                args: fn_args,
                expr,
            }) => {
                let mut context = self.vars.clone();
                for (arg_name, arg_expr) in fn_args.iter().zip(args) {
                    let expr = self.eval_expr(arg_expr);
                    context.remove_entry(arg_name);
                    context.insert(arg_name.clone(), expr);
                }

                Interpreter::new_with_context(vec![expr.clone()], context, self.fns.clone()).eval()
            }
            None => {
                panic!("Function not found {:?}", name)
            }
        }
    }

    fn eval_expr(&mut self, expr: AST) -> Value {
        match expr {
            AST::Add(lhs, rhs) => self.add(lhs.as_ref(), rhs.as_ref()),
            AST::Div(lhs, rhs) => Value::Int(self.div(lhs.as_ref(), rhs.as_ref())),
            AST::Mul(lhs, rhs) => Value::Int(self.mul(lhs.as_ref(), rhs.as_ref())),
            AST::Minus(lhs, rhs) => Value::Int(self.minus(lhs.as_ref(), rhs.as_ref())),
            AST::Num(num) => Value::Int(num),
            AST::Id(name) => match self.vars.get(&name) {
                Some(value) => value.clone(),
                None => {
                    panic!("Var not found {}", name)
                }
            },
            AST::FnCall(name, args) => self.fn_call(name, args),
            AST::Str(string) => Value::Str(string),
            AST::Array(array) => {
                let value = Vec::from_iter(array.iter().map(|node| self.eval_expr(node.clone())));
                Value::Array(value)
            }
            AST::Dot(lhs, rhs) => {
                if let Value::Array(array) = self.eval_expr(*lhs.clone()) {
                    match rhs.as_str() {
                        "length" => Value::Int(array.len() as i32),
                        _ => panic!("unknown method {} for {:?}", rhs, lhs),
                    }
                } else {
                    panic!("expected lhs of {:?}.{:?} to be an id", lhs, rhs);
                }
            }

            AST::Let(_, _) => todo!("let is not valid expr"),
            AST::Def(_, _, _) => todo!("def is not a valid expr"),
        }
    }

    pub fn eval(&mut self) -> Value {
        let mut result = Value::Nil;
        for node in self.ast.clone() {
            result = match node {
                AST::Let(id, expr) => self.assign(id.as_ref(), expr.as_ref()),
                AST::Def(name, args, expr) => self.define(name, args, expr.as_ref()),
                _ => self.eval_expr(node),
            }
        }
        result
    }
}
