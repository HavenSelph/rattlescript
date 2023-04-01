use crate::ast::AST;
use crate::common::{make, Ref, Span};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::{Class, ClassInstance, Function, IteratorValue, Value};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

mod builtin;
pub mod value;

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, Value>,
    pub parent: Option<Ref<Scope>>,
    pub in_function: bool,
}

impl Scope {
    pub fn new(parent: Option<Ref<Scope>>, in_function: bool) -> Ref<Scope> {
        make!(Scope {
            vars: HashMap::new(),
            parent,
            in_function,
        })
    }

    fn insert(&mut self, name: &str, value: Value, update: bool, loc: &Span) -> Result<()> {
        if !update || self.vars.contains_key(name) {
            self.vars.insert(name.to_string(), value);
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

#[derive(Debug)]
enum ControlFlow {
    None,
    Continue,
    Break,
    Return(Value),
}

type BuiltInFunctionType = fn(&Span, Vec<Value>) -> Result<Value>;

pub struct Interpreter {
    builtins: HashMap<&'static str, BuiltInFunctionType>,
    control_flow: ControlFlow,
}

macro_rules! builtins {
    ($($name:ident),+ $(,)?) => {
        HashMap::from([$(
            (
                stringify!($name),
                builtin::$name as BuiltInFunctionType,
            ),
        )+])
    };
}

impl Interpreter {
    pub fn new() -> Self {
        let builtins = builtins!(print, repr, len, exit, input);
        Self {
            builtins,
            control_flow: ControlFlow::None,
        }
    }

    pub fn execute(&mut self, ast: &Rc<AST>) -> Result<Value> {
        let scope = Scope::new(None, false);
        self.run(ast, scope)
    }

    pub fn run_block_without_new_scope(
        &mut self,
        ast: &Rc<AST>,
        scope: Ref<Scope>,
    ) -> Result<Value> {
        match ast.as_ref() {
            AST::Block(_, stmts) => {
                let mut last = None;
                for stmt in stmts {
                    last = Some(self.run(stmt, scope.clone())?);
                }
                Ok(last.unwrap_or_else(|| Value::Nothing))
            }
            _ => unreachable!("run_block_without_scope called on non-block"),
        }
    }

    fn run(&mut self, ast: &Rc<AST>, scope: Ref<Scope>) -> Result<Value> {
        macro_rules! dispatch_op {
            ($span:expr, $op:path, $left:expr, $right:expr) => {{
                let left = self.run($left, scope.clone())?;
                let right = self.run($right, scope.clone())?;
                $op(&left, &right, $span)?
            }};

            ($span:expr, $op:path, $val:expr) => {{
                let val = self.run($val, scope.clone())?;
                $op(&val, $span)?
            }};
        }
        Ok(match ast.as_ref() {
            // Literals
            AST::BooleanLiteral(_, value) => Value::Boolean(*value),
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(make!(string.clone())),
            AST::Nothing(_) => Value::Nothing,

            AST::Plus(span, left, right) => dispatch_op!(span, Value::plus, left, right),
            AST::Minus(span, left, right) => dispatch_op!(span, Value::minus, left, right),
            AST::Multiply(loc, left, right) => dispatch_op!(loc, Value::multiply, left, right),
            AST::Power(loc, left, right) => dispatch_op!(loc, Value::power, left, right),
            AST::Divide(loc, left, right) => dispatch_op!(loc, Value::divide, left, right),
            AST::Modulo(loc, left, right) => dispatch_op!(loc, Value::modulo, left, right),
            AST::FloorDivide(loc, left, right) => {
                dispatch_op!(loc, Value::floor_divide, left, right)
            }

            AST::Not(loc, expr) => dispatch_op!(loc, Value::not, expr),
            AST::And(loc, left, right) => dispatch_op!(loc, Value::and, left, right),
            AST::Or(loc, left, right) => dispatch_op!(loc, Value::or, left, right),

            AST::Equals(loc, left, right) => dispatch_op!(loc, Value::equals, left, right),
            AST::NotEquals(loc, left, right) => dispatch_op!(loc, Value::not_equals, left, right),
            AST::LessThan(loc, left, right) => dispatch_op!(loc, Value::less_than, left, right),

            AST::GreaterThan(loc, left, right) => {
                dispatch_op!(loc, Value::greater_than, left, right)
            }
            AST::LessEquals(loc, left, right) => {
                dispatch_op!(loc, Value::less_equals, left, right)
            }
            AST::GreaterEquals(loc, left, right) => {
                dispatch_op!(loc, Value::greater_equals, left, right)
            }

            AST::Call(span, func, args) => self.handle_call(scope, span, func, args)?,

            AST::Function {
                name,
                args,
                body,
                span,
                ..
            } => {
                let func = Value::Function(make!(Function {
                    span: *span,
                    name: name.clone().unwrap_or_else(|| "<anon>".to_string()),
                    args: args
                        .iter()
                        .map(|(name, def)| (
                            name.clone(),
                            def.as_ref()
                                .map(|def| self.run(def, scope.clone()).unwrap())
                        ))
                        .collect(),
                    body: body.clone(),
                    scope: scope.clone(),
                }));
                match name {
                    Some(name) => scope.borrow_mut().insert(name, func.clone(), false, span)?,
                    None => {}
                }
                func
            }
            AST::FieldAccess(span, obj, field) => {
                let obj = self.run(obj, scope)?;
                match obj {
                    Value::ClassInstance(instance) => match instance.borrow().fields.get(field) {
                        Some(val) => val.clone(),
                        None => {
                            error!(span, "Field '{}' not found on class instance", field);
                        }
                    },
                    _ => {
                        error!(
                            span,
                            "Cannot access field '{}' on non-class instance", field
                        );
                    }
                }
            }
            AST::Class {
                span,
                name,
                fields,
                methods,
            } => {
                let class = Value::Class(make!(Class {
                    span: *span,
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, def)| (
                            name.clone(),
                            def.as_ref()
                                .map(|def| self.run(def, scope.clone()).unwrap())
                        ))
                        .collect(),
                    methods: methods
                        .iter()
                        .map(|(name, func)| {
                            let func = self.run(func, scope.clone()).expect("function");
                            (name.clone(), func)
                        })
                        .collect()
                }));
                scope
                    .borrow_mut()
                    .insert(name, class.clone(), false, span)?;
                class
            }
            AST::Slice {
                span,
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
                lhs.slice(start, end, step, span)?
            }

            AST::Block(..) => {
                let block_scope = Scope::new(Some(scope.clone()), scope.borrow().in_function);
                self.run_block_without_new_scope(ast, block_scope)?
            }
            AST::Variable(span, name) => {
                if self.builtins.get(name.as_str()).is_some() {
                    Value::BuiltInFunction(make!(name.clone()))
                } else if let Some(value) = scope.borrow_mut().get(name) {
                    value
                } else {
                    error!(span, "Variable {} not found", name)
                }
            }
            AST::Return(span, val) => {
                if !scope.borrow_mut().in_function {
                    error!(span, "Return statement outside of function")
                }
                self.control_flow = ControlFlow::Return(self.run(val, scope)?);
                Value::Nothing
            }
            AST::Assignment(span, lhs, value) => {
                let value = self.run(value, scope.clone())?;
                self.handle_assign(scope, span, lhs, value.clone())?;
                value
            }
            AST::VarDeclaration(span, name, value) => {
                if self.builtins.contains_key(name.as_str()) {
                    error!(
                        span,
                        "`{}` is a built-in function, can't be used as a variable", name
                    )
                }
                let value = self.run(value, scope.clone())?;
                scope
                    .borrow_mut()
                    .insert(name, value.clone(), false, span)?;
                value
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
            AST::If(span, cond, body, else_body) => {
                let cond = self.run(cond, scope.clone())?;
                match cond {
                    Value::Boolean(true) => self.run(body, scope)?,
                    Value::Boolean(false) => match else_body {
                        Some(else_body) => self.run(else_body, scope)?,
                        None => Value::Nothing,
                    },
                    _ => error!(span, "If condition must be a boolean"),
                }
            }
            AST::While(span, cond, body) => {
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
                        _ => error!(span, "While condition must be a boolean"),
                    };
                }
                Value::Nothing
            }
            AST::ForEach(span, loop_var, iter, body) => {
                let val = self.run(iter, scope.clone())?;
                let iter = val.iterator(span)?;
                match iter {
                    Value::Iterator(IteratorValue(iter)) => {
                        let iter = &mut *(*iter).borrow_mut();
                        for val in iter {
                            let loop_scope =
                                Scope::new(Some(scope.clone()), scope.borrow_mut().in_function);
                            loop_scope
                                .borrow_mut()
                                .insert(loop_var, val.clone(), false, span)?;
                            self.run(body, loop_scope)?;
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
                    _ => error!(span, "For loop must iterate over an iterable"),
                };
                Value::Nothing
            }
            AST::Comprehension(span, var, iter, expr, cond) => {
                let val = self.run(iter, scope.clone())?;
                let iter_value = val.iterator(span)?;
                match iter_value {
                    Value::Iterator(IteratorValue(iter)) => {
                        let iter = &mut *(*iter).borrow_mut();
                        let mut vec = Vec::new();
                        for val in iter {
                            let loop_scope =
                                Scope::new(Some(scope.clone()), scope.borrow_mut().in_function);
                            loop_scope
                                .borrow_mut()
                                .insert(var, val.clone(), false, span)?;
                            if let Some(cond) = cond {
                                let condition = self.run(cond, loop_scope.clone())?;
                                match condition {
                                    Value::Boolean(true) => {}
                                    Value::Boolean(false) => continue,
                                    _ => error!(
                                        cond.span(),
                                        "Comprehension condition must be a boolean"
                                    ),
                                };
                            }
                            vec.push(self.run(expr, loop_scope)?);
                        }
                        Value::Array(make!(vec))
                    }
                    _ => error!(iter.span(), "Comprehension target must be iterable"),
                }
            }
            AST::For {
                span,
                init,
                cond,
                step,
                body,
            } => {
                let loop_scope = Scope::new(Some(scope.clone()), scope.borrow().in_function);
                if let Some(init) = init {
                    self.run(init, loop_scope.clone())?;
                }
                loop {
                    if let Some(cond) = cond {
                        let cond = self.run(cond, loop_scope.clone())?;
                        match cond {
                            Value::Boolean(true) => {}
                            Value::Boolean(false) => break,
                            _ => error!(span, "For condition must be a boolean"),
                        };
                    }
                    self.run(body, loop_scope.clone())?;
                    match self.control_flow {
                        ControlFlow::None => {}
                        ControlFlow::Continue => self.control_flow = ControlFlow::None,
                        ControlFlow::Break => {
                            self.control_flow = ControlFlow::None;
                            break;
                        }
                        ControlFlow::Return(_) => break,
                    }
                    if let Some(step) = step {
                        self.run(step, loop_scope.clone())?;
                    }
                }
                Value::Nothing
            }
            AST::FormatStringLiteral(_, strings, exprs) => {
                let mut result = String::new();
                for (i, string) in strings.iter().enumerate() {
                    result.push_str(string);
                    if i < exprs.len() {
                        let expr = self.run(&exprs[i], scope.clone())?;
                        result.push_str(format!("{:?}", expr).as_str());
                    }
                }
                Value::String(make!(result))
            }
            AST::Range(span, start, end) => {
                let start = self.run(start, scope.clone())?;
                let end = self.run(end, scope)?;
                Value::create_range(&start, &end, span)?
            }

            AST::Break(_) => {
                self.control_flow = ControlFlow::Break;
                Value::Nothing
            }
            AST::Continue(_) => {
                self.control_flow = ControlFlow::Continue;
                Value::Nothing
            }
            AST::Index(span, left, right) => {
                let left = self.run(left, scope.clone())?;
                let right = self.run(right, scope)?;
                // match left {
                //     // Value::Dict
                //     _ => left.index(&right, span)?,
                // }
                left.index(&right, span)?
            }
            AST::PostIncrement(span, expr, offset) => {
                let value = self.run(expr, scope.clone())?;
                match &value {
                    Value::Integer(val) => {
                        let new_val = Value::Integer(*val + offset);
                        self.handle_assign(scope, span, expr, new_val)?;
                    }
                    _ => error!(span, "Operation only supported for integers"),
                }
                value
            }
            AST::PreIncrement(span, expr, offset) => {
                let value = self.run(expr, scope.clone())?;
                match &value {
                    Value::Integer(val) => {
                        let new_val = Value::Integer(*val + offset);
                        self.handle_assign(scope, span, expr, new_val.clone())?;
                        new_val
                    }
                    _ => error!(span, "Operation only supported for integers"),
                }
            }
            AST::ArrayLiteral(_, arr) => Value::Array(make!(arr
                .iter()
                .map(|x| self.run(x, scope.clone()))
                .collect::<Result<Vec<_>>>()?)),
            AST::TupleLiteral(_, arr) => Value::Tuple(make!(arr
                .iter()
                .map(|x| self.run(x, scope.clone()))
                .collect::<Result<Vec<_>>>()?)),
            AST::DictionaryLiteral(_, arr) => {
                let mut map = HashMap::new();
                for (key, value) in arr {
                    let key = self.run(key, scope.clone())?;
                    let value = self.run(value, scope.clone())?;
                    map.insert(key, value);
                }
                Value::Dict(make!(map))
            }
        })
    }

    fn handle_assign(
        &mut self,
        scope: Ref<Scope>,
        span: &Span,
        left: &Rc<AST>,
        value: Value,
    ) -> Result<()> {
        match &**left {
            AST::Variable(span, name) => {
                if scope.borrow_mut().get(name.as_str()).is_none() {
                    error!(span, "Variable {} doesn't exist", name)
                }
                if self.builtins.contains_key(name.as_str()) {
                    error!(span, "`{}` is a built-in function, can't override it", name)
                }
                scope
                    .borrow_mut()
                    .insert(name.as_str(), value, true, span)?;
            }
            AST::Index(span, left, right) => {
                let left = self.run(left, scope.clone())?;
                let right = self.run(right, scope)?;
                left.set_index(&right, &value, span)?;
            }
            AST::FieldAccess(span, left, name) => {
                let left = self.run(left, scope)?;
                left.set_field(name.as_str(), &value, span)?;
            }
            _ => error!(span, "Invalid assignment target"),
        }
        Ok(())
    }

    fn handle_call(
        &mut self,
        scope: Ref<Scope>,
        span: &Span,
        func: &Rc<AST>,
        args: &[(Option<String>, Rc<AST>)],
    ) -> Result<Value> {
        let parent = match func.deref() {
            AST::FieldAccess(_, left, ..) => Some(left),
            _ => None,
        };
        let callee = self.run(func, scope.clone())?;
        return Ok(match callee {
            Value::Function(func) => {
                let new_scope = Scope::new(Some(func.borrow().scope.clone()), true);
                if let Some(parent) = parent {
                    let parent = self.run(parent, scope.clone())?;
                    new_scope.borrow_mut().insert("self", parent, false, span)?;
                }
                if args.len() > func.borrow().args.len() {
                    error!(
                        span,
                        "Function expected no more than {} arguments, got {}",
                        func.borrow().args.len(),
                        args.len()
                    )
                } else {
                    let func = func.borrow();
                    for (i, (name, default)) in func.args.iter().enumerate() {
                        if i < args.len() {
                            let (label, arg) = &args[i];
                            let arg = self.run(arg, scope.clone())?;
                            if let Some(label) = label {
                                if label != name {
                                    error!(
                                        span,
                                        "Function argument {} is required, but {} was provided",
                                        name,
                                        label
                                    )
                                }
                            }
                            new_scope.borrow_mut().insert(name, arg, false, span)?;
                        } else if let Some(default) = default {
                            new_scope
                                .borrow_mut()
                                .insert(name, default.clone(), false, span)?;
                        } else {
                            error!(
                                span,
                                "Function argument {} is required, but not provided", name
                            )
                        }
                    }
                }
                let body = func.borrow().body.clone();
                self.run(&body, new_scope)?;
                let value = if let ControlFlow::Return(value) = &self.control_flow {
                    value.clone()
                } else {
                    Value::Nothing
                };
                self.control_flow = ControlFlow::None;
                value
            }
            Value::BuiltInFunction(func) => {
                let args = args
                    .iter()
                    .map(|(_, arg)| self.run(arg, scope.clone()))
                    .collect::<Result<Vec<_>>>()?;
                match self.builtins.get(func.borrow().as_str()) {
                    Some(func) => func(span, args)?,
                    None => error!(span, "Built-in function {} not found", func.borrow()),
                }
            }
            Value::Class(class) => {
                let _class = class.borrow();
                let mut fields: HashMap<String, Value> = HashMap::new();
                for method in _class.methods.iter() {
                    fields.insert(method.0.clone(), method.1.clone());
                }
                let args = args
                    .iter()
                    .map(|(_, arg)| self.run(arg, scope.clone()))
                    .collect::<Result<Vec<_>>>()?;
                if args.len() > _class.fields.len() {
                    error!(
                        span,
                        "Class expected no more than {:?} arguments, got {:?}",
                        _class.fields.len(),
                        args.len()
                    );
                }
                for (i, (name, default)) in _class.fields.iter().enumerate() {
                    if i < args.len() {
                        let arg = &args[i];
                        fields.insert(name.clone(), arg.clone());
                    } else if let Some(default) = default {
                        fields.insert(name.clone(), default.clone());
                    } else {
                        error!(
                            span,
                            "Class argument {} is required, but not provided", name
                        );
                    }
                }
                Value::ClassInstance(make!(ClassInstance {
                    class: class.clone(),
                    fields
                }))
            }
            x => error!(span, "Can't call object {:?}", x),
        });
    }
}
