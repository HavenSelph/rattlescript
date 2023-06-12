use crate::ast::AST;
use crate::error::{compiler_error, Result};
use std::env;
use std::rc::Rc;

pub struct Scope {
    pub variables: Vec<String>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
        }
    }

    pub fn add_variable(&mut self, name: &str) {
        self.variables.push(name.to_string());
    }
}

pub struct Compiler {
    buf: String,
    scopes: Vec<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            buf: String::new(),
            scopes: vec![],
        }
    }

    fn last_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn compile(&mut self, ast: &Rc<AST>) -> Result<String> {
        let runtimes = format!(
            "#include \"{}\\runtime\\datatypes\\value.h\"\n",
            env::current_dir().unwrap().display()
        );
        self.buf.push_str(runtimes.as_str());
        self.buf.push_str("\nint main() {\n");
        let out = self.comp(ast)?;
        self.buf.push_str(out.as_str());
        self.buf.push_str("\n}");
        Ok(self.buf.clone())
    }

    fn comp(&mut self, ast: &Rc<AST>) -> Result<String> {
        return Ok(match ast.as_ref() {
            AST::Block(_, statements) => {
                self.scopes.push(Scope::new());
                let mut out = String::new();
                for statement in statements {
                    out.push_str(self.comp(statement)?.as_str());
                    out.push_str(";\n");
                }
                for i in 0..self.last_scope().variables.len() {
                    out.push_str("value_unref(");
                    out.push_str(self.last_scope().variables[i].as_str());
                    out.push_str(");\n");
                }
                self.scopes.pop();
                out
            }
            AST::If(_, condition, if_body, else_body) => {
                let mut out = String::new();
                out.push_str("if (value_as_c_bool(");
                out.push_str(self.comp(condition)?.as_str());
                out.push_str(")) {\n");
                out.push_str(self.comp(if_body)?.as_str());
                out.push('}');
                if let Some(else_body) = else_body {
                    out.push_str(" else {\n");
                    out.push_str(self.comp(else_body)?.as_str());
                    out.push('}');
                }
                out
            }

            AST::Variable(_, name) => format!("value_ref({})", name),
            AST::VarDeclaration(_, name, value) => {
                if self.scopes.last().unwrap().variables.contains(name) {
                    format!("{name} = value_replace({}, {})", name, self.comp(value)?)
                } else {
                    self.scopes.last_mut().unwrap().add_variable(name);
                    format!("Value *{} = {}", name, self.comp(value)?)
                }
            }
            AST::Assignment(_, name, value) => {
                format!("{name} = value_replace({}, {})", name, self.comp(value)?)
            }

            AST::IntegerLiteral(_, num) => format!("value_int({})", num),
            AST::FloatLiteral(_, num) => format!("value_float({})", num),
            AST::StringLiteral(_, string) => format!("value_string(\"{}\")", string),
            AST::BooleanLiteral(_, bool) => format!("value_bool({})", bool),
            AST::ArrayLiteral(_, elements) => {
                let mut out = String::new();
                out.push_str(format!("value_list_v({}", elements.len()).as_str());
                for element in elements {
                    out.push_str(", ");
                    out.push_str(self.comp(element)?.as_str());
                }
                out.push(')');
                out
            }

            AST::Plus(_, left, right) => {
                format!("value_add({}, {})", self.comp(left)?, self.comp(right)?)
            }
            AST::Minus(_, left, right) => {
                format!("value_sub({}, {})", self.comp(left)?, self.comp(right)?)
            }
            AST::Multiply(_, left, right) => {
                format!("value_mul({}, {})", self.comp(left)?, self.comp(right)?)
            }
            AST::Divide(_, left, right) => {
                format!("value_div({}, {})", self.comp(left)?, self.comp(right)?)
            }
            AST::Modulo(_, left, right) => {
                format!("value_mod({}, {})", self.comp(left)?, self.comp(right)?)
            }
            AST::Power(_, left, right) => {
                format!("value_pow({}, {})", self.comp(left)?, self.comp(right)?)
            }

            _ => compiler_error!(ast.span(), "Unsupported AST node: {:?}", ast),
        });
    }
}
