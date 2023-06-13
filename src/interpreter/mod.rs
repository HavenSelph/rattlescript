use crate::ast::ArgumentType::Keyword;
use crate::ast::{ArgumentType, CallArgs, AST};
use crate::common::{make, Ref, Span};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::{
    builtin, CallArgValues, Class, ClassInstance, Function, IteratorValue, Value,
};
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
                Some(parent) => parent.borrow().get(name),
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

pub struct Interpreter {
    control_flow: ControlFlow,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
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
                    match self.control_flow {
                        ControlFlow::None => {}
                        _ => break,
                    }
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
            AST::StringLiteral(_, string) => Value::String(Rc::new(string.clone())),
            AST::Nothing(_) => Value::Nothing,

            AST::Plus(span, left, right) => dispatch_op!(span, Value::plus, left, right),
            AST::Minus(span, left, right) => dispatch_op!(span, Value::minus, left, right),
            AST::Multiply(loc, left, right) => dispatch_op!(loc, Value::multiply, left, right),
            AST::Power(loc, left, right) => dispatch_op!(loc, Value::power, left, right),
            AST::Divide(loc, left, right) => dispatch_op!(loc, Value::divide, left, right),
            AST::Modulo(loc, left, right) => dispatch_op!(loc, Value::modulo, left, right),
            AST::Negate(loc, expr) => dispatch_op!(loc, Value::negate, expr),
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
                span,
                name,
                args,
                required,
                in_class: _,
                body,
            } => {
                let func = Value::Function(make!(Function {
                    span: *span,
                    name: name.clone().unwrap_or_else(|| "<anon>".to_string()),
                    args: args
                        .iter()
                        .map(|(name, default, argtype)| (
                            name.clone(),
                            default
                                .as_ref()
                                .map(|def| self.run(def, scope.clone()).unwrap()),
                            *argtype,
                        ))
                        .collect(),
                    required: *required,
                    body: body.clone(),
                    scope: scope.clone()
                }));
                match name {
                    Some(name) => scope.borrow_mut().insert(name, func.clone(), false, span)?,
                    None => {}
                }
                func
            }
            AST::FieldAccess(span, obj, field) => {
                let obj = self.run(obj, scope)?;
                obj.get_field(span, field)?
            }
            AST::Class {
                span,
                name,
                fields,
                parents,
            } => {
                let parents: Option<Ref<Vec<Value>>> = match parents {
                    Some(parents) => {
                        let parent_vals: Ref<Vec<Value>> = make!(Vec::new());
                        for parent in parents {
                            let parent_val = scope.borrow().get(parent.as_str());
                            match parent_val.clone() {
                                Some(value) => match value {
                                    Value::Class(_) => {}
                                    _ => {
                                        error!(span, "Classes may only inherit from other classes and not `{}`", value.type_of());
                                    }
                                },
                                _ => {
                                    error!(span, "Parent class `{}` not found", parent);
                                }
                            }
                            parent_vals.borrow_mut().push(parent_val.unwrap());
                        }
                        Some(parent_vals)
                    }
                    None => None,
                };
                let mut field_vals: HashMap<String, Value> = HashMap::new();
                for (name, definition) in fields {
                    field_vals.insert(name.to_string(), self.run(definition, scope.clone())?);
                }

                let class = Value::Class(make!(Class {
                    span: *span,
                    name: name.clone(),
                    parents,
                    fields: field_vals,
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
            AST::Variable(span, name) => match name.as_str() {
                "len" => builtin!(len),
                "print" => builtin!(print),
                "input" => builtin!(input),
                "str" => builtin!(to_str),
                "repr" => builtin!(repr),
                "open" => builtin!(file_open),
                "exit" => builtin!(exit),
                _ => match scope.borrow().get(name) {
                    Some(val) => val,
                    None => {
                        error!(span, "Variable '{}' not found", name);
                    }
                },
            },
            AST::Return(span, val) => {
                if !scope.borrow().in_function {
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
                self.check_arg_name(name, span)?;
                let value = self.run(value, scope.clone())?;
                scope.borrow_mut().insert(name, value, false, span)?;
                Value::Nothing
            }
            AST::Assert(loc, cond, msg) => {
                let cond = self.run(cond, scope)?;
                match cond {
                    Value::Boolean(true) => {}
                    Value::Boolean(false) => match msg {
                        Some(msg) => error!(loc, "Assertion failed: {}", msg),
                        _ => error!(loc, "Assertion failed"),
                    },
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
                self.check_arg_name(loop_var, span)?;
                let val = self.run(iter, scope.clone())?;
                let iter = val.iterator(span)?;
                match iter {
                    Value::Iterator(IteratorValue(iter)) => {
                        let iter = &mut *(*iter).borrow_mut();
                        for val in iter {
                            let loop_scope =
                                Scope::new(Some(scope.clone()), scope.borrow().in_function);
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
                                Scope::new(Some(scope.clone()), scope.borrow().in_function);
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
                Value::String(Rc::new(result))
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
                    let span = key.span();
                    let key = self.run(key, scope.clone())?;
                    if !key.is_hashable() {
                        error!(span, "Dictionary key must be hashable")
                    }
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
                if scope.borrow().get(name.as_str()).is_none() {
                    error!(span, "Variable {} doesn't exist", name)
                }
                // if self.builtins.contains_key(name.as_str()) {
                //     error!(span, "`{}` is a built-in function, can't override it", name)
                // }
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

    fn check_arg_name(&self, name: &str, span: &Span) -> Result<()> {
        if name == "self" {
            error!(span, "Argument name can't be `self`")
            // } else if self.builtins.contains_key(name) {
            //     error!(span, "Argument name can't be a built-in function name")
        } else {
            Ok(())
        }
    }

    fn handle_call(
        &mut self,
        scope: Ref<Scope>,
        span: &Span,
        obj: &Rc<AST>,
        args: &CallArgs,
    ) -> Result<Value> {
        let mut parent = None;

        let callee = match obj.deref() {
            AST::FieldAccess(_, left, field) => {
                let temp = self.run(left, scope.clone())?;
                parent = Some(temp.clone());
                temp.get_field(span, field)?
            }
            _ => self.run(obj, scope.clone())?,
        };
        let args = self.run_call_args(scope.clone(), args)?;
        self.do_call(span, scope, parent, callee, &args)
    }

    pub fn run_call_args(&mut self, scope: Ref<Scope>, args: &CallArgs) -> Result<CallArgValues> {
        let mut values = CallArgValues::new();
        for (name, value) in args {
            values.push((name.clone(), self.run(value, scope.clone())?));
        }
        Ok(values)
    }

    pub fn do_call(
        &mut self,
        span: &Span,
        scope: Ref<Scope>,
        parent: Option<Value>,
        callee: Value,
        args: &CallArgValues,
    ) -> Result<Value> {
        // Handle the call
        Ok(match callee.clone() {
            Value::Function(func) => {
                // Setup scope

                let run_scope = Scope::new(Some(func.borrow().scope.clone()), true);
                if let Some(parent) = parent {
                    run_scope.borrow_mut().insert("self", parent, false, span)?;
                }

                // Let's handle the function arguments
                let func = func.borrow();
                let mut variadic_name = None;
                let mut variadic_keyword_name = None;

                let mut need: Vec<String> = Vec::new();
                let mut seen: Vec<String> = Vec::new();
                let mut arguments: HashMap<String, Value> = HashMap::new();
                let mut variadic: Vec<Value> = Vec::new();
                let mut variadic_keyword: HashMap<Value, Value> = HashMap::new();

                for (name, arg, argtype) in func.args.iter() {
                    match argtype {
                        ArgumentType::Positional => need.push(name.to_string()),
                        Keyword => {
                            run_scope.borrow_mut().insert(
                                name,
                                arg.clone().expect("Keywords always have default values."),
                                false,
                                span,
                            )?;
                        }
                        ArgumentType::Variadic => {
                            variadic_name = Some(name);
                        }
                        ArgumentType::VariadicKeyword => {
                            variadic_keyword_name = Some(name);
                        }
                    };
                }

                let mut state = ArgumentType::Positional;
                for (i, (name, arg)) in args.iter().enumerate() {
                    match name {
                        Some(name) => {
                            if seen.contains(name) {
                                error!(span, "Duplicate keyword argument: `{}`", name);
                            } else if run_scope.borrow().vars.contains_key(name) || need.contains(name) {
                                arguments.insert(name.to_string(), arg.clone());
                                seen.push(name.clone());
                                state = Keyword;
                            } else if variadic_keyword_name.is_some() {
                                variadic_keyword
                                    .insert(Value::String(Rc::new(name.clone())), arg.clone());
                                seen.push(name.clone());
                                state = Keyword;
                            } else {
                                error!(span, "Unexpected keyword argument: `{}`", name);
                            }
                        }
                        None => {
                            if i < func.required {
                                if state != ArgumentType::Positional {
                                    error!(
                                        span,
                                        "Positional arguments must be the first provided."
                                    );
                                }
                                let (name, ..) = func.args.get(i).unwrap();
                                arguments.insert(name.to_string(), arg.clone());
                                seen.push(name.clone());
                            } else if variadic_name.is_some() {
                                if state != ArgumentType::Variadic
                                    && state != ArgumentType::Positional
                                {
                                    error!(span, "Variadic arguments must be the last provided.");
                                }
                                variadic.push(arg.clone());
                                state = ArgumentType::Variadic;
                            } else {
                                error!(span, "Unexpected positional argument");
                            }
                        }
                    }
                }

                // Check if all required arguments are provided
                for name in need {
                    if !seen.contains(&name) {
                        error!(span, "Missing required argument: `{}`", name);
                    }
                }

                if let Some(variadic_name) = variadic_name {
                    arguments.insert(variadic_name.to_string(), Value::Array(make!(variadic)));
                }

                if let Some(variadic_keyword_name) = variadic_keyword_name {
                    arguments.insert(
                        variadic_keyword_name.to_string(),
                        Value::Dict(make!(variadic_keyword)),
                    );
                }
                for key in arguments.keys() {
                    run_scope.borrow_mut().insert(
                        key,
                        arguments.get(key).unwrap().clone(),
                        false,
                        span,
                    )?;
                }

                // Run the function
                let body = func.body.clone();
                self.run(&body, run_scope)?;
                let value = if let ControlFlow::Return(value) = &self.control_flow {
                    value.clone()
                } else {
                    Value::Nothing
                };
                self.control_flow = ControlFlow::None;
                value
            }
            Value::BuiltInFunction(func) => {
                let run_scope = Scope::new(Some(scope), true);
                let mut args: Vec<Value> = args.iter().map(|(_, arg)| arg.clone()).collect();
                if let Some(parent) = parent {
                    args.insert(0, parent);
                }
                func.1.borrow()(self, run_scope, span, args)?
            }
            Value::Class(_class) => {
                let class = _class.borrow();
                let c_span = class.span;
                let name = class.name.clone();
                let parents = class.parents.clone();
                let fields = class.fields.clone();

                let new_scope = Scope::new(Some(scope), false);

                // Handle parent fields
                if let Some(parents) = parents {
                    for parent in parents.borrow().iter() {
                        match parent {
                            Value::Class(parent) => {
                                let parent = parent.borrow();
                                for (name, value) in parent.fields.iter() {
                                    new_scope.borrow_mut().insert(
                                        name,
                                        value.clone(),
                                        false,
                                        span,
                                    )?;
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                }

                // Handle class fields
                for (name, value) in fields.iter() {
                    new_scope
                        .borrow_mut()
                        .insert(name, value.clone(), false, span)?;
                }

                let instance = Value::ClassInstance(make!(ClassInstance {
                    span: c_span,
                    name,
                    parents: class.parents.clone(),
                    scope: new_scope.clone()
                }));

                // Call new if it exists
                let Some(function) = new_scope.borrow().get("new") else {
                    return Ok(instance);
                };
                match function {
                    Value::Function { .. } => {
                        self.do_call(
                            span,
                            new_scope,
                            Some(instance.clone()),
                            function,
                            args,
                        )?;
                    }
                    _ => error!(
                        span,
                        "Expected function for initializer, but got '{}'",
                        function.type_of()
                    ),
                }
                instance
            }
            _ => error!(span, "Can't call object {:?}", callee),
        })
    }
}
