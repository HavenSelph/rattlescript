use crate::ast::AST;
use crate::token::Location;
use crate::utils::error;
use crate::builtins;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};


#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Value>,
    parent: Option<Ref<Scope>>,
    in_function: bool,
}

impl Scope {
    fn insert(&mut self, name: String, value: Value, update: bool) {
        if !update {
            self.vars.insert(name, value);
        } else {
            if self.vars.contains_key(&name) {
                self.vars.insert(name, value);
            } else {
                match &self.parent {
                    Some(parent) => parent.lock().unwrap().insert(name, value, update),
                    None => error!("Variable {} not found, couldn't update", name),
                }
            }
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if self.vars.contains_key(name) {
            self.vars.get(name).map(|v| v.clone())
        } else {
            match &self.parent {
                Some(parent) => parent.lock().unwrap().get(name),
                None => None,
            }
        }
    }
}

type BuiltInFunctionType = fn(&Location, Vec<Value>) -> Value;
type Ref<T> = Arc<Mutex<T>>;

enum ControlFlowDecision {
    None,
    Return(Value),
}

pub struct Interpreter {
    builtins: HashMap<String, BuiltInFunctionType>,
    control_flow: ControlFlowDecision,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut builtins = HashMap::new();
        builtins.insert("print".to_string(), builtins::print as BuiltInFunctionType);
        builtins.insert("len".to_string(), builtins::len as BuiltInFunctionType);

        Interpreter {
            builtins,
            control_flow: ControlFlowDecision::None,
        }
    }

    pub fn execute(&mut self, ast: &Arc<AST>) -> Value {
        let mut scope = Arc::new(Mutex::new(Scope {
            vars: HashMap::new(),
            parent: None,
            in_function: false,
        }));
        self.run(ast, &mut scope)
    }

    fn run(&mut self, ast: &Arc<AST>, scope: &mut Ref<Scope>) -> Value {
        match ast.as_ref() {
            AST::Block(_, stmts) => {
                let mut last = Value::Nothing;
                for stmt in stmts {
                    last = self.run(stmt, scope);
                    match self.control_flow {
                        ControlFlowDecision::None => {},
                        _ => break,
                    }
                }
                last
            },
            AST::Call(loc, func, args) => {
                self.handle_call(scope, loc, func, args)
            },
            AST::If(loc, cond, body, else_body) => {
                let cond = self.run(cond, scope);
                match cond {
                    Value::Boolean(true) => self.run(body, scope),
                    Value::Boolean(false) => {
                        match else_body {
                            Some(else_body) => self.run(else_body, scope),
                            None => Value::Nothing,
                        }
                    },
                    _ => error!("{loc}: If condition must be a boolean")
                }
            },
            AST::BooleanLiteral(_, value) => Value::Boolean(*value),
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(string.clone()),
            AST::Nothing(_) => Value::Nothing,
            AST::VarDeclaration(_, name, value) => {
                if scope.lock().unwrap().vars.contains_key(name) {
                    error!("Variable {} already exists in scope", name)
                }
                if self.builtins.contains_key(name) {
                    error!("`{}` is a built-in function, can't be used as a variable", name)
                }
                let value = self.run(value, scope);
                scope.lock().unwrap().insert(name.clone(), value.clone(), false);
                value
            },
            AST::Assignment(loc, lhs, value) => {
                let value = self.run(value, scope);
                match lhs.as_ref() {
                    AST::Variable(loc, name) => {
                        if let None = scope.lock().unwrap().get(name) {
                            error!("{loc}: Variable {} doesn't exist", name)
                        }
                        if self.builtins.contains_key(name) {
                            error!("{loc}: `{}` is a built-in function, can't override it", name)
                        }
                        scope.lock().unwrap().insert(name.clone(), value.clone(), true);
                        value
                    },
                    _ => error!("{loc}: Can't assign to {:?}", lhs)
                }
            },
            AST::Not(loc, expr) => {
                Value::Boolean(!self.get_boolean_value(scope, loc, expr))
            }
            AST::And(loc, left, right) => {
                let left = self.get_boolean_value(scope, loc, left);
                let right = self.get_boolean_value(scope, loc, right);
                Value::Boolean(left && right)
            }
            AST::Or(loc, left, right) => {
                let left = self.get_boolean_value(scope, loc, left);
                let right = self.get_boolean_value(scope, loc, right);
                Value::Boolean(left || right)
            }
            AST::Index(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (&left, &right) {
                    (Value::String(left), Value::Integer(right)) => {
                        match left.chars().nth(*right as usize) {
                            Some(c) => Value::String(c.to_string()),
                            None => error!("{loc}: Index out of bounds")
                        }
                    },
                    _ => error!("{loc}: Can't index {:?} with {:?}", left, right)
                }
            },
            AST::Variable(loc, name) => {
                if let Some(_) = self.builtins.get(name) {
                    return Value::BuiltInFunction(name.clone())
                }
                if let Some(value) = scope.lock().unwrap().get(name) {
                    return value.clone()
                }
                error!("{loc}: Variable {} not found", name)
            },
            AST::Plus(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left + right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 + right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left + right as f64),
                    (Value::String(left), Value::String(right)) => Value::String(left + &right),
                    _ => error!("{loc}: Invalid types for addition")
                }
            },
            AST::Minus(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left - right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 - right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left - right as f64),
                    _ => error!("{loc}: Invalid types for subtraction")
                }
            },
            AST::Multiply(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left * right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 * right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left * right as f64),
                    (Value::String(left), Value::Integer(right)) => {
                        if right < 0 { error!("{loc}: {right} is not a positive integer.") }
                        Value::String(left.repeat(right as usize))
                    }
                    _ => error!("{loc}: Invalid types for multiplication")
                }
            },
            AST::Divide(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left / right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 / right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left / right as f64),
                    _ => error!("{loc}: Invalid types for division")
                }
            },
            AST::Slice {loc, lhs, start, end, step} => {
                let lhs = self.run(lhs, scope);
                match lhs {
                    Value::String(s) => {
                        let start = if let Some(start) = start { self.run(start, scope) } else { Value::Integer(0) };
                        let end = if let Some(end) = end { self.run(end, scope) } else { Value::Integer(s.len() as i64) };
                        let step = if let Some(step) = step { self.run(step, scope) } else { Value::Integer(1) };
                        match (start, end, step) {
                            (Value::Integer(start), Value::Integer(end), Value::Integer(step)) => {
                                if step == 0 { error!("{loc}: Step cannot be 0") }
                                let mut result = String::new();
                                let mut i = start;
                                while i < end {
                                    result.push(s.chars().nth(i as usize).unwrap());
                                    i += step;
                                }
                                Value::String(result)
                            },
                            _ => error!("{loc}: Invalid types for slice")
                        }
                    },
                    _ => error!("{loc}: Can only slice strings")
                }
            }
            AST::Function { name, args, body, .. } => {
                let func = Value::Function{
                    args: args.clone(),
                    body: body.clone(),
                    scope: scope.clone()
                };
                match name {
                    Some(name) => {
                        scope.lock().unwrap().insert(name.clone(), func.clone(), false);
                    },
                    None => {}
                }
                func
            },
            AST::Return(loc, val) => {
                if !scope.lock().unwrap().in_function {
                    error!("{loc}: Return statement outside of function")
                }
                self.control_flow = ControlFlowDecision::Return(self.run(val, scope));
                Value::Nothing
            },
        }
    }

    fn handle_call(&mut self, scope: &mut Ref<Scope>, loc: &Location, func: &Arc<AST>, args: &Vec<Arc<AST>>) -> Value {
        let func = self.run(func, scope);
        let args: Vec<_> = args.iter().map(|arg| self.run(arg, scope)).collect();

        match &func {
            Value::BuiltInFunction(func) => {
                match self.builtins.get(func) {
                    Some(func) => func(loc, args),
                    None => unreachable!("{loc}: Built-in function {:?} not found", func)
                }
            }
            Value::Function{ body, args: func_args, scope: closure_scope } => {
                let mut new_scope = Arc::new(Mutex::new(Scope {
                    vars: HashMap::new(),
                    parent: Some(closure_scope.clone()),
                    in_function: true,
                }));
                if args.len() != func_args.len() {
                    error!("{loc}: Expected {} arguments, got {}", func_args.len(), args.len())
                }
                for (arg, value) in func_args.iter().zip(args) {
                    new_scope.lock().unwrap().insert(arg.clone(), value, false);
                }
                self.run(body, &mut new_scope);
                let value = if let ControlFlowDecision::Return(value) = &self.control_flow {
                    value.clone()
                } else {
                    Value::Nothing
                };
                self.control_flow = ControlFlowDecision::None;
                value
            }
            _ => error!("{loc}: Can't call object {:?}", func)
        }
    }

    fn get_boolean_value(&mut self, scope: &mut Ref<Scope>, loc: &Location, val: &Arc<AST>) -> bool {
        match self.run(val, scope) {
            Value::Boolean(b) => b,
            _ => error!("{loc}: Expected boolean value")
        }
    }
}
