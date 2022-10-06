use crate::parser::{ClassEntry, AST};
use core::panic;
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct Fn {
    args: Vec<String>,
    body: Vec<AST>,
}

#[derive(Clone, Debug)]
struct ClassDef {
    arg_names: Vec<String>,
    methods: Vec<Fn>,
}

#[derive(Clone, Debug)]
pub struct Object {
    vars: HashMap<String, Value>,
    def: ClassDef,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    Str(String),
    Sym(String),
    Array(Vec<Value>),
    Object(Object),
    Nil,
}

#[derive(Clone)]
pub struct Interpreter {
    ast: Vec<AST>,
    vars: HashMap<String, Value>,
    fns: HashMap<String, Fn>,
    classes: HashMap<String, ClassDef>,
}

impl Interpreter {
    pub fn new(ast: Vec<AST>) -> Interpreter {
        Interpreter {
            ast,
            vars: HashMap::new(),
            fns: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    fn new_with_context(
        ast: Vec<AST>,
        vars: HashMap<String, Value>,
        fns: HashMap<String, Fn>,
        classes: HashMap<String, ClassDef>,
    ) -> Interpreter {
        Interpreter {
            ast,
            vars,
            fns,
            classes,
        }
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

    fn add_i32(&mut self, lhs: i32, rhs: &AST) -> Value {
        let rhs = self.eval_expr(rhs.clone());
        if let Value::Int(rhs) = rhs {
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

    fn add(&mut self, lhs: &AST, rhs: &AST) -> Value {
        match lhs {
            AST::Num(lhs) => self.add_i32(*lhs, rhs),
            AST::Id(lhs_name) => match self.vars.get(lhs_name) {
                Some(Value::Int(lhs)) => self.add_i32(*lhs, rhs),
                Some(Value::Str(lhs)) => self.add_str(lhs.clone(), rhs),
                Some(Value::Array(_)) => todo!("Array + not implemented yet"),
                Some(Value::Sym(name)) => todo!("cannot add {:?} to {}", lhs, name),
                Some(Value::Nil) => panic!("cannot add {:?} to nil", lhs),
                Some(Value::Object(object)) => {
                    panic!("cannot add {:?} to object {:?}", lhs, object)
                }
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

    fn define_fn(&mut self, name: String, args: Vec<String>, body: Vec<AST>) -> Value {
        self.fns.insert(name, Fn { args, body });
        Value::Nil
    }

    fn define_class(
        &mut self,
        name: String,
        arg_names: Vec<String>,
        _methods: Vec<ClassEntry>,
    ) -> Value {
        self.classes.insert(
            name,
            ClassDef {
                arg_names,
                methods: vec![],
            },
        );
        Value::Nil
    }

    fn new_class(&mut self, class_name: String, args: Vec<AST>) -> Value {
        if let Some(ClassDef { arg_names, methods }) = self.classes.get(&class_name) {
            let mut vars: HashMap<String, Value> = HashMap::new();
            for (expr, name) in args.iter().zip(arg_names) {
                vars.insert(name.clone(), self.clone().eval_expr(expr.clone()));
            }
            Value::Object(Object {
                vars,
                def: ClassDef {
                    arg_names: arg_names.clone(),
                    methods: methods.clone(),
                },
            })
        } else {
            panic!("new");
        }
    }

    fn fn_call_from_id(&mut self, name: String, args: Vec<AST>) -> Value {
        if name == "print" {
            println!(
                "{}",
                args.iter()
                    .map(|expr| format!("{:?}", self.eval_expr(expr.clone())))
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            Value::Nil
        } else if let Some(Fn {
            args: fn_args,
            body,
        }) = self.fns.get(&name)
        {
            let mut context = self.vars.clone();
            if args.iter().all(|arg| matches!(arg, AST::NamedArg(_, _))) {
                let mut names = args.iter().map(|arg| match arg {
                    AST::NamedArg(name, _) => name,
                    _ => panic!("unknown"),
                });
                assert!(names.len() == fn_args.len() && names.all(|name| fn_args.contains(name)));
                let args = args.iter().map(|arg| match arg {
                    AST::NamedArg(name, expr) => {
                        (name, self.clone().eval_expr(expr.as_ref().clone()))
                    }
                    _ => panic!("wtf"),
                });
                for (name, expr) in args {
                    context.remove(name);
                    context.insert(name.clone(), expr);
                }
            } else {
                for (arg_name, arg_expr) in fn_args.iter().zip(args) {
                    let expr = self.clone().eval_expr(arg_expr);
                    context.remove_entry(arg_name);
                    context.insert(arg_name.clone(), expr.clone());
                }
            }
            // println!("calling function with {:?}", context);

            Interpreter::new_with_context(
                body.to_vec(),
                context,
                self.fns.clone(),
                self.classes.clone(),
            )
            .eval()
        } else {
            panic!("Function not found {}", name);
        }
    }

    fn call_array_method(&self, method_name: String, array: &[Value], args: Vec<AST>) -> Value {
        match method_name.as_str() {
            "join" => {
                assert!(args.len() == 1);
                if let Some(AST::Str(join_str)) = args.first() {
                    let str_arr: Vec<String> =
                        Vec::from_iter(array.iter().map(|node| match node {
                            Value::Int(int) => int.to_string(),
                            Value::Str(string) => string.to_string(),
                            _ => todo!("join on non str | int"),
                        }));
                    Value::Str(str_arr.join(join_str))
                } else {
                    panic!("Expected a string for join")
                }
            }
            name => panic!("Unknown array method {}", name),
        }
    }

    fn fn_call_from_dot(&mut self, lhs: AST, method_name: String, args: Vec<AST>) -> Value {
        let lhs = self.eval_expr(lhs);
        if let Value::Array(array) = lhs {
            self.call_array_method(method_name, &array, args)
        } else {
            panic!("Unsupported dot fn call - lhs: {:?}", lhs);
        }
    }

    fn fn_call(&mut self, lhs: AST, args: Vec<AST>) -> Value {
        match lhs {
            AST::Id(name) => self.fn_call_from_id(name, args),
            AST::Dot(lhs, method_name) => self.fn_call_from_dot(*lhs, method_name, args),
            _ => panic!("Unknown function expression {:?}", lhs),
        }
    }

    fn eval_expr(&mut self, expr: AST) -> Value {
        match expr {
            AST::Add(lhs, rhs) => self.add(lhs.as_ref(), rhs.as_ref()),
            AST::Div(lhs, rhs) => Value::Int(self.div(lhs.as_ref(), rhs.as_ref())),
            AST::Mul(lhs, rhs) => Value::Int(self.mul(lhs.as_ref(), rhs.as_ref())),
            AST::Minus(lhs, rhs) => Value::Int(self.minus(lhs.as_ref(), rhs.as_ref())),
            AST::Nil => Value::Nil,
            AST::Num(num) => Value::Int(num),
            AST::Id(name) => match self.vars.get(&name) {
                Some(value) => value.clone(),
                None => panic!("Var not found {}", name),
            },
            AST::Sym(name) => Value::Sym(name),
            AST::FnCall(lhs, args) => self.fn_call(*lhs, args),
            AST::Str(string) => Value::Str(string),
            AST::Array(array) => {
                let value = Vec::from_iter(array.iter().map(|node| self.eval_expr(node.clone())));
                Value::Array(value)
            }
            AST::Dot(lhs, rhs) => {
                let lhs = self.eval_expr((*lhs).clone());
                if let Value::Array(array) = lhs {
                    match rhs.as_str() {
                        "length" => Value::Int(array.len() as i32),
                        _ => panic!("unknown method {} for {:?}", rhs, array),
                    }
                } else if let Value::Str(string) = lhs {
                    match rhs.as_str() {
                        "length" => Value::Int(string.len() as i32),
                        _ => panic!("unknown method {} for {:?}", rhs, string),
                    }
                } else {
                    panic!("expected lhs of {:?}.{:?} to be an id", lhs, rhs);
                }
            }
            AST::Class(name, args, entries) => self.define_class(name, args, entries),
            AST::New(class_name, args) => self.new_class(class_name, args),

            AST::Let(_, _) => todo!("let is not valid expr"),
            AST::Def(_, _, _) => todo!("def is not a valid expr"),
            AST::NamedArg(_, _) => todo!("named arg is not a valid expr"),
        }
    }

    pub fn eval(&mut self) -> Value {
        let mut result = Value::Nil;
        for node in self.ast.clone() {
            result = match node {
                AST::Let(id, expr) => self.assign(id.as_ref(), expr.as_ref()),
                AST::Def(name, args, body) => self.define_fn(name, args, body),
                _ => self.eval_expr(node),
            }
        }
        result
    }
}
