#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}};
#[allow(unused_imports)]
use crate::debug;

macro_rules! err {
    () => {
        "<ERROR_OCCURRED>".to_string()
    }
}

macro_rules! check_unexpected_type {
    ($self:expr, $location:expr, $type_id:expr, $found_type_id:expr) => {
        if $type_id.is_some() {
            return $self.error_unexpected_type($location, $type_id, $found_type_id);
        }
    };
}

macro_rules! return_err_if {
    ($condition:expr) => {
        if $condition {
            return err!();
        }
    };
}

pub fn generate_code(transpiler: &Transpiler) -> String {
    let mut gen = GenerateC::new(transpiler.clone());
    gen.generate();
    gen.outc
}

struct GenerateC {
    pub table: CodegenTable,
    pub transpiler: Transpiler,
    pub code: String,
    pub path: Option<String>,
    pub outc: String,
}

impl GenerateC {
    pub fn new(transpiler: Transpiler) -> GenerateC {
        let code = transpiler.output.full_code.clone();
        let path = transpiler.output.path.clone();
        GenerateC { table: CodegenTable::new(), transpiler: transpiler, code, path, outc: "".to_string() }
    }

    fn error(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra errors on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.error("transpiler error", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.transpiler.output.error("transpiler error", message, help, location);
        }
    }
    fn warning(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra warnings on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.warning("transpiler warning", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.transpiler.output.warning("transpiler warning", message, help, location);
        }
    }
    fn message(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra messages on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.message("transpiler message", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        } else {
            self.transpiler.output.message("transpiler message", message, help, location);
        }
    }

    fn add_header(&mut self, header: String) {
        self.outc = format!("{}\n{}", header, self.outc);
    }
    fn add_include(&mut self, header: String) {
        self.outc = format!("#include <{}>\n{}", header, self.outc);
    }
    fn add_line(&mut self, line: String, scope: Scope) {
        let tab = "\t".repeat(scope as usize);
        self.outc = format!("{}\n{}{}", self.outc, tab, line);
    }

    fn generate(&mut self) {
        let ast = self.transpiler.ast.clone();
        for node in ast.iter() {
            let c_node = self.evaluate_node(node, None, 0, false);
            if DEBUGGING {
                let scone_node = crate::parser::Parser::node_expr_to_string(node, self.table.scope() as usize);
                self.outc = format!("{}\n// {}\n{}", self.outc, scone_node, c_node);
            } else {
                self.outc = format!("{}\n{}", self.outc, c_node);
            }
        }
    }

    fn evaluate_node(&mut self, node: &ASTNode, type_id: Option<TypeId>, scope: Scope, constant: bool) -> String {
        if constant {
            return self.evaluate_constant_node(node, type_id);
        }
        match node.node.as_ref() {
            NodeType::Constant(ref v) => self.constant(v, type_id),
            NodeType::VariableDeclaration(ref v) => self.variable_declaration(v, type_id, scope),
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.transpiler.output.print_messages();
                todo!();
            }
        }
    }

    fn evaluate_constant_node(&mut self, node: &ASTNode, type_id: Option<TypeId>) -> String {
        self.error(line!(), "Parsing constant not implemented yet", "Parsing constant not implemented yet", &node.token.location);
        self.transpiler.output.print_messages();
        todo!();
    }

    fn get_type(&mut self, location: &Location, type_id: Option<TypeId>) -> String {
        if let Some(type_id) = type_id {
            if let Ok(ty) = self.table.get_type_id(type_id) {
                match ty {
                    TypeType::Type(ty) => ty.name,
                    TypeType::TypeParameter(ty) => ty.name
                }
            } else {
                self.error(line!(), "Type does not exist", format!("Type with the id `{}` does not exist", type_id).as_str(), location);
                return err!();
            }
        } else {
            self.error(line!(), "No current type", "Expected to return a specific type here", location);
            return err!();
        }
    }

    fn error_unexpected_type(&mut self, location: &Location, type_id: Option<TypeId>, found_type_id: Option<TypeId>) -> String {
        let expected_type = self.get_type(location, type_id);
        let found_type = if found_type_id.is_some() { self.get_type(location, found_type_id) } else { "()".to_string() };
        self.error(line!(), "Unexpected type", format!("Expected type `{expected_type}` but found `{found_type}` instead").as_str(), location);
        return err!();
    }

    fn get_type_from_name(&mut self, location: &Location, name: &String) -> Option<TypeId> {
        if let Ok(ty) = self.table.get_type_name(&name) {
            match ty {
                TypeType::Type(v) => Some(v.type_id),
                TypeType::TypeParameter(v) => Some(v.type_id)
            }
        } else {
            self.error(line!(), "Type does not exist", format!("Type `{name}` does not exist in this scope").as_str(), &location);
            None
        }
    }

    fn variable_declaration(&mut self, node: &VariableDeclaration, type_id: Option<TypeId>, scope: Scope) -> String {
        check_unexpected_type!(self, &node.var_name.location, type_id, None);
        
        let type_name = self.evaluate_node(&node.var_type, None, scope, false);
        return_err_if!(type_name == err!());

        let var_type_id = self.get_type_from_name(&node.var_type.token.location, &type_name);
        return_err_if!(var_type_id == None);

        let name = node.var_name.value.clone();
        return_err_if!(name.is_empty());

        let has_value;
        let value;
        if let Some(v) = &node.var_value {
            value = Some(self.evaluate_node(&v, var_type_id, scope, false));
            has_value = true;
        } else {
            value = None;
            has_value = false;
        }

        let first_part = format!("{} {}", type_name, name);
        let second_part;
        if value.is_some() {
            second_part = format!("= {}", value.unwrap());
        } else {
            second_part = "".to_string();
        }
        
        let result = format!("{} {};", first_part, second_part);
        self.add_line(result, scope);

        let var = self.table.generate_variable(name, var_type_id.unwrap(), has_value, false, node.access_modifier.clone(), node.tags.clone());
        self.table.add_identifier_scope(var);

        "".to_string()
    }

    fn constant(&mut self, node: &ConstantNode, type_id: Option<TypeId>) -> String { 
        let expected = self.get_type(&node.value.location, type_id);
        let found = node.constant_type.to_string();
        if found != expected {
            if !((expected == "i64" && matches!(found.as_str(), "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
               (expected == "i32" && matches!(found.as_str(), "i32" | "i16" | "i8" | "u8" | "u32" | "u16")) ||
               (expected == "i16" && matches!(found.as_str(), "i16" | "i8" | "u8" | "u16")) ||
               (expected == "i8" && matches!(found.as_str(), "i8" | "u8")) ||
               (expected == "u64" && matches!(found.as_str(), "u64" | "u32" | "u16" | "u8")) ||
               (expected == "u32" && matches!(found.as_str(), "u32" | "u16" | "u8")) ||
               (expected == "u16" && matches!(found.as_str(), "u16" | "u8")) ||
               (expected == "f64" && matches!(found.as_str(), "f32" | "f64" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
               (expected == "f32" && matches!(found.as_str(), "f32" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")))
            {
                self.error(line!(), "Type mismatch", format!("Expected type `{}` does not match constant type `{}`", expected, found).as_str(), &node.value.location);
            } 
        }

        if node.constant_type.is_number() {
            let number = node.value.value.replace("_", "");
            if number.ends_with("f64") && !number.contains("0x") { return number.replace("f64", ""); }
            else if number.ends_with("f32") && !number.contains("0x") { return number.replace("f32", ""); }
            else if number.ends_with("u8") { return number.replace("u8", ""); }
            else if number.ends_with("u16") { return number.replace("u16", ""); }
            else if number.ends_with("u32") { return number.replace("u32", ""); }
            else if number.ends_with("u64") { return number.replace("u64", ""); }
            else if number.ends_with("i8") { return number.replace("i8", ""); }
            else if number.ends_with("i16") { return number.replace("i16", ""); }
            else if number.ends_with("i32") { return number.replace("i32", ""); }
            else if number.ends_with("i64") { return number.replace("i64", ""); }
            else {
                return number;
            }
        }

        match node.constant_type.clone() {
            ConstantType::String => format!("\"{}\"", node.value.value),
            ConstantType::Char => format!("'{}'", node.value.value),
            _ => node.value.value.clone(),
        }
    }
}