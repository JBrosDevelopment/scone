use serde::Serialize;
use crate::{ast::*, lexer::{self, TokenType}, parser};

#[allow(unused_imports)]
use crate::{error_handling::{ErrorHandling, DEBUGGING}};
#[allow(unused_imports)]
use crate::debug;

#[derive(Clone, Debug, Serialize)]
pub struct MacroVariable {
    pub name: String,
    pub value: String,
}
#[derive(Clone, Debug, Serialize)]
pub struct MacroFunction {
    pub name: String,
    pub args: Vec<String>,
    pub body: String,
}
#[derive(Clone, Debug, Serialize)]
pub struct MacroHeader {
    pub name: String
}

#[derive(Clone, Debug, Serialize)]
pub struct Macros {
    pub variables: Vec<MacroVariable>,
    pub functions: Vec<MacroFunction>,
    pub headers: Vec<MacroHeader>
}
impl Macros {
    pub fn new() -> Macros {
        Macros {
            variables: Self::constants(),
            functions: vec![],
            headers: vec![]
        }
    }
    pub fn constants() -> Vec<MacroVariable> {
        vec![
            MacroVariable { name: "TRUE".to_string(), value: "1".to_string() },
            MacroVariable { name: "FALSE".to_string(), value: "0".to_string() },
            MacroVariable { name: "INT_MAX".to_string(), value: "2147483647".to_string() },
            MacroVariable { name: "INT_MIN".to_string(), value: "-2147483648".to_string() },
            MacroVariable { name: "UINT_MAX".to_string(), value: "4294967295".to_string() },
            MacroVariable { name: "VERSION".to_string(), value: env!("CARGO_PKG_VERSION").split(".").take(2).collect::<Vec<&str>>().join(".").to_string() },
        ]
    }
    pub fn add_variable(&mut self, name: String, value: String) {
        self.variables.push(MacroVariable { name, value });
    }
    pub fn add_function(&mut self, name: String, args: Vec<String>, body: String) {
        self.functions.push(MacroFunction { name, args, body });
    }
    pub fn add_header(&mut self, name: String) {
        self.headers.push(MacroHeader { name });
    }
    pub fn get_variable(&self, name: &str) -> Option<&MacroVariable> {
        self.variables.iter().find(|var| var.name == name)
    }
    pub fn get_function(&self, name: &str) -> Option<&MacroFunction> {
        self.functions.iter().find(|func| func.name == name)
    }
    pub fn get_header(&self, name: &str) -> Option<&MacroHeader> {
        self.headers.iter().find(|header| header.name == name)
    }
    pub fn remove_variable(&mut self, name: &str) {
        self.variables.retain(|var| var.name != name);
    }
    pub fn remove_function(&mut self, name: &str) {
        self.functions.retain(|func| func.name != name);
    }
    pub fn remove_header(&mut self, name: &str) {
        self.headers.retain(|header| header.name != name);
    }
    pub fn has_variable(&self, name: &str) -> bool {
        self.get_variable(name).is_some()
    }
    pub fn has_function(&self, name: &str) -> bool {
        self.get_function(name).is_some()
    }
    pub fn has_header(&self, name: &str) -> bool {
        self.get_header(name).is_some()
    }
    pub fn clear(&mut self) {
        self.variables.clear();
        self.functions.clear();
        self.headers.clear();
    }
    pub fn is_empty(&self) -> bool {
        self.variables.is_empty() && self.functions.is_empty() && self.headers.is_empty()
    }
    pub fn len(&self) -> usize {
        self.variables.len() + self.functions.len() + self.headers.len()
    }
    pub fn valid_name(name: &str) -> bool {
        (name.chars().nth(0).unwrap_or('0').is_alphabetic() || name.chars().nth(0).unwrap_or('0') == '_') && name.chars().all(|c| c.is_alphanumeric() || c == '_')
    }
    
    pub fn parse_if_directive(&mut self, message: String) -> Result<bool, String> {
        let parts: Vec<&str> = message.split_whitespace().collect();
        if parts.len() < 2 {
            return Err("Invalid directive".to_string());
        }

        let condition: String = parts[1..].join(" ");
        let value = self.parse_value(condition);

        if value.is_err() {
            return Err("Error parsing condition".to_string());
        }

        let result = value.unwrap();
        if result == "true" || result == "1" {
            Ok(true)
        } else if result == "false" || result == "0" {
            Ok(false)
        } else {
            Err("Invalid boolean value".to_string())
        }
    }

    pub fn parse_if_define_directive(&self, message: String) -> Result<bool, String> {
        let parts: Vec<&str> = message.split_whitespace().collect();
        if parts.len() != 2 {
            return Err("Invalid `define` directive, expected a name: `#! ifdef NAME ...`".to_string());
        }

        let name = parts[1];
        if self.has_header(name) || self.has_function(name) || self.has_variable(name) {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn parse_define_directive(&mut self, message: String) -> Result<(), String> {
        let parts: Vec<&str> = message.split_whitespace().collect();
        
        if parts.len() < 2 {
            return Err("Invalid `define` directive, expected at least a name: `#! def NAME ...`".to_string());
        }
        
        let name = parts[1].chars().take_while(|c| c != &'(').collect::<String>();
        if Macros::valid_name(&name) == false {
            return Err("Invalid macro name, only alphanumeric characters and underscores are allowed".to_string());
        }

        if parts.len() == 2 {
            // defining header
            if self.has_header(&name) {
                return Err(format!("Macro `{}` is already defined", name));
            }
            self.add_header(name.to_string());
        } else if parts[1].contains("(") {
            // define function
            if self.has_function(&name) {
                return Err(format!("Macro function `{}` is already defined", name));
            }
            let mut chars = message.chars().skip_while(|x| *x != '(').into_iter();
            chars.next(); // skip '('
            let mut args = vec![];
            let mut current_arg = "".to_string();
            let mut in_arg = false;
            let mut j = 1;
            while let Some(c) = chars.next() {
                j += 1;
                if c == ')' {
                    if !current_arg.is_empty() {
                        args.push(current_arg.to_string());
                    }
                    break;
                } else if c.is_whitespace() {
                    j += 1;
                    continue;
                } else if c == ',' {
                    if in_arg {
                        args.push(current_arg.to_string());
                        current_arg = "".to_string();
                        in_arg = false;
                    } else {
                        return Err("Unexpected comma in macro function definition".to_string());
                    }
                } else if (c.is_alphabetic() || c == '_') && !in_arg {
                    current_arg.push(c);
                    in_arg = true;
                } else if (c.is_alphabetic() || c.is_numeric() || c == '_') && in_arg {
                    current_arg.push(c);
                } else {
                    return Err(format!("Invalid character `{}` in macro function definition", c));
                }
            }
            let position = message.chars().position(|x| x == '(').unwrap_or(0);
            let body = message[position + j..].trim().to_string();
            if body.is_empty() {
                return Err("Macro function body cannot be empty".to_string());
            }
            self.add_function(name.to_string(), args, body);
        } else {
            // defining variable
            if self.has_variable(&name) {
                return Err(format!("Macro variable `{}` is already defined", name));
            }
            let parse_msg = parts[2..].join(" ");
            match self.parse_value(parse_msg) {
                Ok(value) => self.add_variable(name.to_string(), value),
                Err(e) => return Err(e)
            }
        }

        Ok(())
    }

    pub fn parse_undefine_directive(&mut self, message: String) -> Result<(), String> {
        let parts: Vec<&str> = message.split_whitespace().collect();

        if parts.len() != 2 {
            return Err("Invalid `define` directive, expected a name: `#! undef NAME`".to_string());
        }
        
        if Macros::valid_name(parts[1]) == false {
            return Err("Invalid macro name, only alphanumeric characters and underscores are allowed".to_string());
        }

        if self.has_variable(parts[1]) {
            self.remove_variable(parts[1]);
        } else if self.has_function(parts[1]) {
            self.remove_function(parts[1]);
        } else if self.has_header(parts[1]) {
            self.remove_header(parts[1]);
        } else {
            return Err(format!("Macro `{}` is not defined", parts[1]));
        }
        return Ok(());
    }

    pub fn string_to_node(&mut self, message: &String) -> Result<ASTNode, String> {
        let (tokens, error_handling, _) = lexer::lex(message, None, None);
        if error_handling.has_errors() {
            return Err("Error lexing macro string".to_string());
        }
        let (ast, output) = parser::parse(tokens, &message, None);
        if output.has_errors() {
            return Err("Error parsing macro string".to_string());
        }
        if ast.len() != 1 {
            return Err("Macro body should return 1 end node".to_string());
        }
        Ok(ast[0].clone())
    }

    pub fn parse_value(&mut self, message: String) -> Result<String, String> {
        let node = self.string_to_node(&message);
        if node.is_err() {
            return Err("Error parsing value".to_string());
        }
        let node = node.unwrap();
        self.evaluate_node(&node)
    }

    pub fn evaluate_node(&mut self, node: &ASTNode) -> Result<String, String> {
        match &node.node.as_ref() {
            NodeType::Identifier(ref val) => {
                match self.get_variable(&val.value) {
                    Some(var) => Ok(var.value.clone()),
                    None => Err(format!("Variable `{}` not defined", val.value))
                }
            }
            NodeType::Constant(ref val) => {
                if val.constant_type == ConstantType::String {
                    Ok(format!("\"{}\"", val.value.value.clone()))
                } else if val.constant_type == ConstantType::Char {
                    Ok(format!("'{}'", val.value.value.clone()))
                } else {
                    Ok(val.value.value.clone())
                }
            }
            NodeType::Operator(ref val) => {
                match val.operator.token_type {
                    TokenType::Plus => {
                        let l = self.evaluate_node(&val.left)?;
                        let r = self.evaluate_node(&val.right)?;
                        if let Ok(l_val) = l.parse::<f32>() {
                            if let Ok(r_val) = r.parse::<f32>() {
                                return Ok((l_val + r_val).to_string());
                            }
                        }
                        Ok(l + &r)
                    }
                    TokenType::Dash => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() - r.unwrap()).to_string());
                        }
                        Err("Error evaluating subtraction".to_string())
                    }
                    TokenType::Star => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() * r.unwrap()).to_string());
                        }
                        Err("Error evaluating multiplication".to_string())
                    }
                    TokenType::Slash => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() && r.clone().unwrap() != 0.0 {
                            return Ok((l.unwrap() / r.unwrap()).to_string());
                        }
                        Err("Error evaluating division or division by zero".to_string())
                    }
                    TokenType::Equal => {
                        let l = self.evaluate_node(&val.left)?;
                        let r = self.evaluate_node(&val.right)?;
                        if l == r {
                            return Ok("true".to_string());
                        }
                        Ok("false".to_string())
                    }
                    TokenType::NotEqual => {
                        let l = self.evaluate_node(&val.left)?;
                        let r = self.evaluate_node(&val.right)?;
                        if l != r {
                            return Ok("true".to_string());
                        }
                        Ok("false".to_string())
                    }
                    TokenType::GreaterThan => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() > r.unwrap()).to_string());
                        }
                        Err("Error evaluating greater than comparison".to_string())
                    }
                    TokenType::LessThan => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() < r.unwrap()).to_string());
                        }
                        Err("Error evaluating less than comparison".to_string())
                    }
                    TokenType::GreaterThanOrEqual => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() >= r.unwrap()).to_string());
                        }
                        Err("Error evaluating greater than or equal comparison".to_string())
                    }
                    TokenType::LessThanOrEqual => {
                        let l = self.evaluate_node(&val.left)?.parse::<f32>();
                        let r = self.evaluate_node(&val.right)?.parse::<f32>();
                        if l.is_ok() && r.is_ok() {
                            return Ok((l.unwrap() <= r.unwrap()).to_string());
                        }
                        Err("Error evaluating less than or equal comparison".to_string())
                    }
                    TokenType::And => {
                        let l = self.evaluate_node(&val.left)?;
                        let r = self.evaluate_node(&val.right)?;
                        if l == "true" && r == "true" {
                            return Ok("true".to_string());
                        }
                        Ok("false".to_string())
                    }
                    TokenType::Or => {
                        let l = self.evaluate_node(&val.left)?;
                        let r = self.evaluate_node(&val.right)?;
                        if l == "true" || r == "true" {
                            return Ok("true".to_string());
                        }
                        Ok("false".to_string())
                    }
                    TokenType::Not => {
                        let val = self.evaluate_node(&val.left)?;
                        if val == "true" {
                            return Ok("false".to_string());
                        } else if val == "false" {
                            return Ok("true".to_string());
                        }
                        Err("Error evaluating logical NOT operation".to_string())
                    }
                    _ => {
                        Err("Unsupported operator for macro evaluation".to_string())
                    }
                } 
            }
            NodeType::FunctionCall(ref val) => {
                if !self.has_function(&val.name.value) {
                    return Err(format!("Macro function `{}` not defined", val.name.value));
                }
                let func = self.get_function(&val.name.value).unwrap().clone();
                let args = val.parameters.parameters.clone();
                let evaluated_args = args.iter().map(|arg| self.evaluate_node(arg)).collect::<Result<Vec<String>, String>>();
                if evaluated_args.is_err() {
                    return Err("Error evaluating function arguments".to_string());
                }
                
                return self.macro_function(evaluated_args.unwrap(), &func)
            }
            NodeType::ScopedExpression(ref val) => {
                let mut result = String::new();
                if val.scope.len() != 1 {
                    return Err("No scoping for macro evaluation, expected exactly one identifier while calling".to_string());
                }
                for ident in &val.scope {
                    let expr = *ident.expression.clone();
                    let evaluated = self.evaluate_node(&expr)?;
                    result.push_str(&evaluated);
                }
                Ok(result)
            }
            _ => {
                return Err(format!("Unsupported node type for macro evaluation, supports: identifiers, operators, function calls, literals. Instead got: `{}`", node.node.to_string()))
            }
        }
    }

    pub fn macro_function(&mut self, args: Vec<String>, func: &MacroFunction) -> Result<String, String> {
        if args.len() != func.args.len() {
            return Err("Function argument count mismatch".to_string())
        }
        
        for arg in &func.args {
            if self.has_variable(arg) {
                return Err("Function argument name conflicts with existing variable".to_string());
            }
        }

        for (i, arg) in func.args.iter().enumerate() {
            self.add_variable(arg.clone(), args[i].clone());
        }

        let body = func.body.clone();
        let node = self.string_to_node(&body);
        if node.is_err() {
            return Err("Error parsing function body".to_string());
        }

        let evaluated_node = self.evaluate_node(&node.unwrap());

        for arg in &func.args {
            self.remove_variable(arg);
        }

        evaluated_node
    }
}