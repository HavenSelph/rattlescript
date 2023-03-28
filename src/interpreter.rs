use crate::ast::AST;
use crate::builtin;
use crate::error::{runtime_error as error, Result};
use crate::token::Location;
use crate::value::{IteratorValue, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Scope {
    pub vars: HashMap<String, Value>,
    pub parent: Option<Ref<Scope>>,
    pub in_function: bool,
}

impl Scope {
    fn insert(&mut self, name: String, value: Value, update: bool, loc: &Location) -> Result<()> {
        if !update || self.vars.contains_key(&name) {
            self.vars.insert(name, value);
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().insert(name, value, update, loc)?,
                None => error!(loc, "Variable {} not found, couldn't update", name),
            }
        }
        Ok(())
    }

    fn get(&self, name: &str) -> Option<Value> {
        if self.vars.contains_key(name) {
            self.vars.get(name).cloned()
        } else {
            match &self.parent {
                Some(parent) => parent.borrow_mut().get(name),
                None => None,
            }
        }
    }
}

type BuiltInFunctionType = fn(&Location, Vec<Value>) -> Result<Value>;
pub type Ref<T> = Rc<RefCell<T>>;

enum ControlFlow {
    None,
    Continue,
    Break,
    Return(Value),
}

pub struct Interpreter {
    builtins: HashMap<&'static str, BuiltInFunctionType>,
    control_flow: ControlFlow,
}

macro_rules! builtins {
    ($($name:ident),+ $(,)?) => {
        HashMap::from([$(
            (stringify!($name), builtin::$name as BuiltInFunctionType),
        )+])
    };
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let builtins = builtins!(print, len, exit, input);
        Interpreter {
            builtins,
            control_flow: ControlFlow::None,
        }
    }

    pub fn execute(&mut self, ast: &Rc<AST>) -> Result<Value> {
        let scope = Rc::new(RefCell::new(Scope {
            vars: HashMap::new(),
            parent: None,
            in_function: false,
        }));
        self.run(ast, scope)
    }

    pub fn run_block_without_scope(&mut self, ast: &Rc<AST>, scope: Ref<Scope>) -> Result<Value> {
        let mut last = Value::Nothing;
        match ast.as_ref() {
            AST::Block(_, stmts) => {
                for stmt in stmts {
                    last = self.run(stmt, scope.clone())?;
                    match self.control_flow {
                        ControlFlow::None => {}
                        _ => break,
                    }
                }
                Ok(last)
            }
            _ => unreachable!("run_block_without_scope called on non-block"),
        }
    }


    pub fn run(&mut self, ast: &Rc<AST>, scope: Ref<Scope>) -> Result<Value> {
        macro_rules! dispatch_op {
            ($loc:expr, $op:path, $left:expr, $right:expr) => {{
                let left = self.run($left, scope.clone())?;
                let right = self.run($right, scope.clone())?;
                $op(left, right, $loc)?
            }};

            ($loc:expr, $op:path, $val:expr) => {{
                let val = self.run($val, scope.clone())?;
                $op(val, $loc)?
            }};
        }

        Ok(match ast.as_ref() {
            AST::Block(..) => {
                let block_scope = Rc::new(RefCell::new(Scope {
                    vars: HashMap::new(),
                    parent: Some(scope.clone()),
                    in_function: scope.borrow_mut().in_function,
                }));
                self.run_block_without_scope(ast, block_scope)?
            }
            AST::Call(loc, func, args) => self.handle_call(scope, loc, func, args)?,
            AST::If(loc, cond, body, else_body) => {
                let cond = self.run(cond, scope.clone())?;
                match cond {
                    Value::Boolean(true) => self.run(body, scope)?,
                    Value::Boolean(false) => match else_body {
                        Some(else_body) => self.run(else_body, scope)?,
                        None => Value::Nothing,
                    },
                    _ => error!(loc, "If condition must be a boolean"),
                }
            }
            AST::While(loc, cond, body) => {
                loop {
                    let cond = self.run(cond, scope.clone())?;
                    match cond {
                        Value::Boolean(true) => {
                            self.run(body, scope.clone())?;
                            match self.control_flow {
                                ControlFlow::None => {}
                                ControlFlow::Continue => self.control_flow = ControlFlow::None,
                                ControlFlow::Break => {
                                    self.control_flow = ControlFlow::None;
                                    break;
                                }
                                ControlFlow::Return(_) => break,
                            }
                        }
                        Value::Boolean(false) => break,
                        _ => error!(loc, "While condition must be a boolean"),
                    }
                }
                Value::Nothing
            }
            AST::For(loc, loop_var, iter, body) => {
                let iter = self.run(iter, scope.clone())?.iterator(loc);
                match iter {
                    Value::Iterator(IteratorValue(iter)) => {
                        let iter = &mut *(*iter).borrow_mut();
                        for val in iter {
                            let mut loop_scope = Scope {
                                vars: HashMap::new(),
                                parent: Some(scope.clone()),
                                in_function: scope.borrow_mut().in_function,
                            };
                            loop_scope.insert(loop_var.clone(), val.clone(), false, loc)?;
                            self.run(body, Rc::new(RefCell::new(loop_scope)))?;
                            match self.control_flow {
                                ControlFlow::None => {}
                                ControlFlow::Continue => self.control_flow = ControlFlow::None,
                                ControlFlow::Break => {
                                    self.control_flow = ControlFlow::None;
                                    break;
                                }
                                ControlFlow::Return(_) => break,
                            }
                        }
                    }
                    _ => error!(loc, "For loop must iterate over an iterable"),
                }
                Value::Nothing
            }
            AST::BooleanLiteral(_, value) => Value::Boolean(*value),
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(string.clone()),
            AST::Nothing(_) => Value::Nothing,
            AST::VarDeclaration(loc, name, value) => {
                if scope.borrow_mut().vars.contains_key(name) {
                    error!(loc, "Variable {} already exists in scope", name)
                }
                if self.builtins.contains_key(name.as_str()) {
                    error!(
                        loc,
                        "`{}` is a built-in function, can't be used as a variable", name
                    )
                }
                let value = self.run(value, scope.clone())?;
                scope
                    .borrow_mut()
                    .insert(name.clone(), value.clone(), false, loc)?;
                value
            }
            AST::Assignment(loc, lhs, value) => {
                let value = self.run(value, scope.clone())?;
                match lhs.as_ref() {
                    AST::Variable(loc, name) => {
                        if scope.borrow_mut().get(name).is_none() {
                            error!(loc, "Variable {} doesn't exist", name)
                        }
                        if self.builtins.contains_key(name.as_str()) {
                            error!(loc, "`{}` is a built-in function, can't override it", name)
                        }
                        scope
                            .borrow_mut()
                            .insert(name.clone(), value.clone(), true, loc)?;
                        value
                    }
                    _ => error!(loc, "Can't assign to {:?}", lhs),
                }
            }
            AST::Index(loc, left, right) => {
                let left = self.run(left, scope.clone())?;
                let right = self.run(right, scope)?;
                match (&left, &right) {
                    (Value::String(left), Value::Integer(right)) => {
                        match left.chars().nth(*right as usize) {
                            Some(c) => Value::String(c.to_string()),
                            None => error!(loc, "Index out of bounds"),
                        }
                    }
                    _ => error!(loc, "Can't index {:?} with {:?}", left, right),
                }
            }
            AST::Variable(loc, name) => {
                if self.builtins.get(name.as_str()).is_some() {
                    Value::BuiltInFunction(name.clone())
                } else if let Some(value) = scope.borrow_mut().get(name) {
                    value
                } else {
                    error!(loc, "Variable {} not found", name)
                }
            }

            AST::Plus(loc, left, right) => dispatch_op!(loc, Value::plus, left, right),
            AST::Minus(loc, left, right) => dispatch_op!(loc, Value::minus, left, right),
            AST::Multiply(loc, left, right) => dispatch_op!(loc, Value::multiply, left, right),
            AST::Divide(loc, left, right) => dispatch_op!(loc, Value::divide, left, right),

            AST::Not(loc, expr) => dispatch_op!(loc, Value::not, expr),
            AST::And(loc, left, right) => dispatch_op!(loc, Value::and, left, right),
            AST::Or(loc, left, right) => dispatch_op!(loc, Value::or, left, right),

            AST::Equals(loc, left, right) => dispatch_op!(loc, Value::equals, left, right),
            AST::NotEquals(loc, left, right) => dispatch_op!(loc, Value::not_equals, left, right),
            AST::LessThan(loc, left, right) => dispatch_op!(loc, Value::less_than, left, right),
            AST::GreaterThan(loc, left, right) => {
                dispatch_op!(loc, Value::greater_than, left, right)
            }
            AST::LessThanEquals(loc, left, right) => {
                dispatch_op!(loc, Value::less_than_equals, left, right)
            }
            AST::GreaterThanEquals(loc, left, right) => {
                dispatch_op!(loc, Value::greater_than_equals, left, right)
            }

            AST::Slice {
                loc,
                lhs,
                start,
                end,
                step,
            } => {
                let lhs = self.run(lhs, scope.clone())?;
                let start = start
                    .clone()
                    .map(|start| self.run(&start, scope.clone()))
                    .transpose()?;
                let end = end
                    .clone()
                    .map(|end| self.run(&end, scope.clone()))
                    .transpose()?;
                let step = step
                    .clone()
                    .map(|step| self.run(&step, scope.clone()))
                    .transpose()?;
                lhs.slice(start, end, step, loc)?
            }
            AST::Function {
                name,
                args,
                body,
                loc,
                ..
            } => {
                let func = Value::Function {
                    args: args.clone(),
                    body: body.clone(),
                    scope: scope.clone(),
                };
                match name {
                    Some(name) => {
                        scope
                            .borrow_mut()
                            .insert(name.clone(), func.clone(), false, loc)?
                    }
                    None => {}
                }
                func
            }
            AST::Return(loc, val) => {
                if !scope.borrow_mut().in_function {
                    error!(loc, "Return statement outside of function")
                }
                self.control_flow = ControlFlow::Return(self.run(val, scope)?);
                Value::Nothing
            }
            AST::Break(_loc) => {
                self.control_flow = ControlFlow::Break;
                Value::Nothing
            }
            AST::Continue(_loc) => {
                self.control_flow = ControlFlow::Continue;
                Value::Nothing
            }
            AST::Assert(loc, cond) => {
                let cond = self.run(cond, scope)?;
                match cond {
                    Value::Boolean(true) => {}
                    Value::Boolean(false) => error!(loc, "Assertion failed"),
                    _ => error!(loc, "Assertion condition must be a boolean"),
                }
                Value::Nothing
            }
            AST::Range(loc, start, end) => {
                let start = self.run(start, scope.clone())?;
                let end = self.run(end, scope)?;
                match (start, end) {
                    (Value::Integer(start), Value::Integer(end)) => Value::Range(start, end),
                    _ => error!(loc, "Range must be between integers"),
                }
            }
        })
    }

    fn handle_call(
        &mut self,
        scope: Ref<Scope>,
        loc: &Location,
        func: &Rc<AST>,
        args: &[Rc<AST>],
    ) -> Result<Value> {
        let func = self.run(func, scope.clone())?;
        let args = args
            .iter()
            .map(|arg| self.run(arg, scope.clone()))
            .collect::<Result<Vec<_>>>()?;

        Ok(match &func {
            Value::BuiltInFunction(func) => match self.builtins.get(func.as_str()) {
                Some(func) => func(loc, args)?,
                None => error!(loc, "Built-in function {:?} not found", func),
            },
            Value::Function {
                body,
                args: func_args,
                scope: closure_scope,
                ..
            } => {
                let new_scope = Rc::new(RefCell::new(Scope {
                    vars: HashMap::new(),
                    parent: Some(closure_scope.clone()),
                    in_function: true,
                }));
                if args.len() != func_args.len() {
                    error!(
                        loc,
                        "Expected {} arguments, got {}",
                        func_args.len(),
                        args.len()
                    )
                }
                for (arg, value) in func_args.iter().zip(args) {
                    new_scope
                        .borrow_mut()
                        .insert(arg.clone(), value, false, loc)?;
                }
                self.run(body, new_scope)?;
                let value = if let ControlFlow::Return(value) = &self.control_flow {
                    value.clone()
                } else {
                    Value::Nothing
                };
                self.control_flow = ControlFlow::None;
                value
            }
            _ => error!(loc, "Can't call object {:?}", func),
        })
    }
}
