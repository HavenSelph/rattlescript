use crate::ast::AST;
use crate::token::Location;
use crate::utils::error;
use crate::builtins;
use crate::value::Value;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};


#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Value>,
    parent: Option<Ref<Scope>>,
    in_function: bool,
}

impl Scope {
    fn insert(&mut self, name: String, value: Value, update: bool, loc: &Location) {
        if !update {
            self.vars.insert(name, value);
        } else {
            if self.vars.contains_key(&name) {
                self.vars.insert(name, value);
            } else {
                match &self.parent {
                    Some(parent) => parent.lock().unwrap().insert(name, value, update, loc),
                    None => error!(loc, "Variable {} not found, couldn't update", name),
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
pub type Ref<T> = Arc<Mutex<T>>;

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

        macro_rules! dispatch_op {
            ($loc:expr, $op:path, $left:expr, $right:expr) => {
                {
                    let left = self.run($left, scope);
                    let right = self.run($right, scope);
                    $op(left, right, $loc)
                }
            };

            ($loc:expr, $op:path, $val:expr) => {
                {
                    let val = self.run($val, scope);
                    $op(val, $loc)
                }
            };
        }

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
                    _ => error!(loc, "If condition must be a boolean")
                }
            },
            AST::BooleanLiteral(_, value) => Value::Boolean(*value),
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(string.clone()),
            AST::Nothing(_) => Value::Nothing,
            AST::VarDeclaration(loc, name, value) => {
                if scope.lock().unwrap().vars.contains_key(name) {
                    error!(loc, "Variable {} already exists in scope", name)
                }
                if self.builtins.contains_key(name) {
                    error!(loc, "`{}` is a built-in function, can't be used as a variable", name)
                }
                let value = self.run(value, scope);
                scope.lock().unwrap().insert(name.clone(), value.clone(), false, loc);
                value
            },
            AST::Assignment(loc, lhs, value) => {
                let value = self.run(value, scope);
                match lhs.as_ref() {
                    AST::Variable(loc, name) => {
                        if let None = scope.lock().unwrap().get(name) {
                            error!(loc, "Variable {} doesn't exist", name)
                        }
                        if self.builtins.contains_key(name) {
                            error!(loc, "`{}` is a built-in function, can't override it", name)
                        }
                        scope.lock().unwrap().insert(name.clone(), value.clone(), true, loc);
                        value
                    },
                    _ => error!(loc, "Can't assign to {:?}", lhs)
                }
            },
            AST::Index(loc, left, right) => {
                let left = self.run(left, scope);
                let right = self.run(right, scope);
                match (&left, &right) {
                    (Value::String(left), Value::Integer(right)) => {
                        match left.chars().nth(*right as usize) {
                            Some(c) => Value::String(c.to_string()),
                            None => error!(loc, "Index out of bounds")
                        }
                    },
                    _ => error!(loc, "Can't index {:?} with {:?}", left, right)
                }
            },
            AST::Variable(loc, name) => {
                if let Some(_) = self.builtins.get(name) {
                    return Value::BuiltInFunction(name.clone())
                }
                if let Some(value) = scope.lock().unwrap().get(name) {
                    return value.clone()
                }
                error!(loc, "Variable {} not found", name)
            },

            AST::Plus(loc, left, right) => dispatch_op!(loc, Value::plus, left, right),
            AST::Minus(loc, left, right) => dispatch_op!(loc, Value::minus, left, right),
            AST::Multiply(loc, left, right) => dispatch_op!(loc, Value::multiply, left, right),
            AST::Divide(loc, left, right) => dispatch_op!(loc, Value::divide, left, right),

            AST::Not(loc, expr) => dispatch_op!(&loc, Value::not, expr),
            AST::And(loc, left, right) => dispatch_op!(loc, Value::and, left, right),
            AST::Or(loc, left, right) => dispatch_op!(loc, Value::or, left, right),

            AST::Equals(loc, left, right) => dispatch_op!(loc, Value::equals, left, right),
            AST::NotEquals(loc, left, right) => dispatch_op!(loc, Value::not_equals, left, right),
            AST::LessThan(loc, left, right) => dispatch_op!(loc, Value::less_than, left, right),
            AST::GreaterThan(loc, left, right) => dispatch_op!(loc, Value::greater_than, left, right),
            AST::LessThanEquals(loc, left, right) => dispatch_op!(loc, Value::less_than_equals, left, right),
            AST::GreaterThanEquals(loc, left, right) => dispatch_op!(loc, Value::greater_than_equals, left, right),

            AST::Slice {loc, lhs, start, end, step} => {
                let lhs = self.run(lhs, scope);
                let start = start.clone().map(|start| self.run(&start, scope));
                let end = end.clone().map(|end| self.run(&end, scope));
                let step = step.clone().map(|step| self.run(&step, scope));
                lhs.slice(start, end, step, loc)
            }
            AST::Function { name, args, body, loc, .. } => {
                let func = Value::Function{
                    args: args.clone(),
                    body: body.clone(),
                    scope: scope.clone()
                };
                match name {
                    Some(name) => {
                        scope.lock().unwrap().insert(name.clone(), func.clone(), false, loc);
                    },
                    None => {}
                }
                func
            },
            AST::Return(loc, val) => {
                if !scope.lock().unwrap().in_function {
                    error!(loc, "Return statement outside of function")
                }
                self.control_flow = ControlFlowDecision::Return(self.run(val, scope));
                Value::Nothing
            },

            AST::Assert(loc, cond) => {
                let cond = self.run(cond, scope);
                match cond {
                    Value::Boolean(true) => {},
                    Value::Boolean(false) => error!(loc, "Assertion failed"),
                    _ => error!(loc, "Assertion condition must be a boolean")
                }
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
            Value::Function{ body, args: func_args, scope: closure_scope, .. } => {
                let mut new_scope = Arc::new(Mutex::new(Scope {
                    vars: HashMap::new(),
                    parent: Some(closure_scope.clone()),
                    in_function: true,
                }));
                if args.len() != func_args.len() {
                    error!(loc, "Expected {} arguments, got {}", func_args.len(), args.len())
                }
                for (arg, value) in func_args.iter().zip(args) {
                    new_scope.lock().unwrap().insert(arg.clone(), value, false, loc);
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
            _ => error!(loc, "Can't call object {:?}", func)
        }
    }
}
