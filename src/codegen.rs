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
        if let Some(__type_id__) = $type_id {
            if let Some(__found_type_id__) = $found_type_id {
                if $self.table.check_types_compatibility(__type_id__, __found_type_id__) == false {
                    return $self.error_unexpected_type($location, $type_id, $found_type_id);
                }
            } else {
                return $self.error_unexpected_type($location, $type_id, $found_type_id);
            }
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

pub fn generate_code(transpiler: &mut Transpiler) -> String {
    let mut gen = GenerateC::new(transpiler.clone());
    gen.add_default_types();
    gen.generate();
    transpiler.output = gen.transpiler.output.clone();
    gen.outc
}

struct GenerateC {
    pub table: CodegenTable,
    pub transpiler: Transpiler,
    pub code: String,
    pub path: Option<String>,
    pub outc: String,
    includes: String,
}

impl GenerateC {
    pub fn new(transpiler: Transpiler) -> GenerateC {
        let code = transpiler.output.full_code.clone();
        let path = transpiler.output.path.clone();
        GenerateC { table: CodegenTable::new(), transpiler: transpiler, code, path, outc: "".to_string(), includes: "".to_string() }
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

    fn add_default_types(&mut self) {
        self.add_header("".to_string());
        let _f32 = self.table.generate_type("f32".to_string(), 0, vec![3, 4, 5, 6, 7, 8, 9, 10]);
        self.table.add_type_scope(_f32); // type_id = 1
        self.add_header("typedef float f32;".to_string());

        let _f64 = self.table.generate_type("f64".to_string(), 0, vec![2, 3, 4, 5, 6, 7, 8, 9, 10]);
        self.table.add_type_scope(_f64); // type_id = 2
        self.add_header("typedef double f64;".to_string());

        let i8 = self.table.generate_type("i8".to_string(), 0, vec![]);
        self.table.add_type_scope(i8); // type_id = 3
        self.add_header("typedef signed char i8;".to_string());

        let _i16 = self.table.generate_type("i16".to_string(), 0, vec![3, 7]);
        self.table.add_type_scope(_i16); // type_id = 4
        self.add_header("typedef short i16;".to_string());

        let _i32 = self.table.generate_type("i32".to_string(), 0, vec![3, 4, 7, 8]);
        self.table.add_type_scope(_i32); // type_id = 5
        self.add_header("typedef int i32;".to_string());

        let _i64 = self.table.generate_type("i64".to_string(), 0, vec![3, 4, 5, 7, 8, 9]);
        self.table.add_type_scope(_i64); // type_id = 6
        self.add_header("typedef long i64;".to_string());

        let _u8 = self.table.generate_type("u8".to_string(), 0, vec![]);
        self.table.add_type_scope(_u8); // type_id = 7
        self.add_header("typedef unsigned char u8;".to_string());

        let _u16 = self.table.generate_type("u16".to_string(), 0, vec![7]);
        self.table.add_type_scope(_u16); // type_id = 8
        self.add_header("typedef unsigned short u16;".to_string());

        let _u32 = self.table.generate_type("u32".to_string(), 0, vec![7, 8]);
        self.table.add_type_scope(_u32); // type_id = 9
        self.add_header("typedef unsigned int u32;".to_string());

        let _u64 = self.table.generate_type("u64".to_string(), 0, vec![7, 8, 9]);
        self.table.add_type_scope(_u64); // type_id = 10
        self.add_header("typedef unsigned long u64;".to_string());

        let _bool = self.table.generate_type("bool".to_string(), 0, vec![]);
        self.table.add_type_scope(_bool); // type_id = 11
        self.add_include("<stdbool.h>".to_string());

        let _string = self.table.generate_type("string".to_string(), 0, vec![]);
        self.table.add_type_scope(_string); // type_id = 12
        self.add_include("\"scone_string.h\"".to_string());
        
        let _vector = self.table.generate_type("vector".to_string(), 1, vec![]);
        self.table.add_type_scope(_vector); // type_id = 13
        self.add_include("\"scone_vector.h\"".to_string());
        
        self.add_header("// Default types".to_string());
    }

    fn add_header(&mut self, header: String) {
        self.outc = format!("{}\n{}", header, self.outc);
    }
    fn add_include(&mut self, header: String) {
        self.includes = format!("#include {}\n{}", header, self.includes);
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
        self.outc = format!("//includes\n{}\n{}", self.includes, self.outc);
        self.includes.clear();
    }

    fn evaluate_node(&mut self, node: &ASTNode, type_id: Option<TypeId>, scope: Scope, constant: bool) -> String {
        if constant {
            return self.evaluate_constant_node(node, type_id);
        }
        match node.node.as_ref() {
            NodeType::Constant(ref v) => self.constant(v, type_id),
            NodeType::VariableDeclaration(ref v) => self.variable_declaration(v, type_id, scope),
            NodeType::ScopedType(ref v) => self.scoped_type(v, type_id),
            NodeType::Identifier(ref v) => self.identifier(v, type_id),
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

    fn get_identifier_from_name(&mut self, location: &Location, name: &String) -> Option<IdentifierType> {
        if let Ok(ident) = self.table.get_identifer_type_by_name(&name) {
            Some(ident)
        } else {
            self.error(line!(), "Identifier does not exist", format!("Identifier `{name}` does not exist in this scope").as_str(), &location);
            None
        }
    }

    fn get_type_from_identifier(&mut self, location: &Location, identifier: &IdentifierType) -> Option<TypeId> {
        match identifier {
            IdentifierType::Variable(var) => Some(var.type_id),
            IdentifierType::Function(func) => Some(func.type_id),
            IdentifierType::Enum(enum_) => self.get_type_from_name(location, &enum_.name),
            IdentifierType::Struct(struct_) => self.get_type_from_name(location, &struct_.name),
            IdentifierType::Trait(trait_) => self.get_type_from_name(location, &trait_.name)
        }
    }

    fn identifier(&mut self, node: &Box<Token>, type_id: Option<TypeId>) -> String {
        let name = node.value.to_string();
        return_err_if!(name.is_empty());

        let ident = self.get_identifier_from_name(&node.location, &name);
        return_err_if!(ident == None);
        let ident = ident.unwrap();
        
        let ident_type = self.get_type_from_identifier(&node.location, &ident);
        return_err_if!(ident_type == None);

        check_unexpected_type!(self, &node.location, type_id, ident_type);

        CodegenTable::get_idname_from_identifier_type(&ident)
    }

    fn type_identifier(&mut self, node: &TypeIdentifier) -> String {
        let name = node.name.value.to_string();
        return_err_if!(name.is_empty());

        let prefix;
        if let Some(st) = &node.scope_type {
            prefix = match st {
                ScopeType::DoubleColon => "__dc__".to_string(),
                _ => {
                    self.error(line!(), "Invalid Scoping punctuation", "Scoping into types allows only `::`. `->` and `.` are not allowed.", &node.name.location);
                    return err!();
                }
            }
        } else {
            prefix = "".to_string();
        }

        let prefix_name = format!("{prefix}{name}");
        let new_type_id = self.get_type_from_name(&node.name.location, &prefix_name);
        return_err_if!(new_type_id == None);

        let postfix;
        let mut type_parameters_vec = vec![];
        if let Some(tp) = &node.type_parameters {
            let mut parameters = "".to_string();
            let mut first_param = true;
            for param in &tp.parameters {
                let string_param = self.evaluate_node(param, None, 0, false);
                return_err_if!(string_param == err!());
                type_parameters_vec.push(string_param.clone());
                
                let param_type_id = self.get_type_from_name(&param.token.location, &string_param);
                return_err_if!(param_type_id == None);

                let comma = if first_param { "".to_string() } else { "_c_".to_string() };
                parameters = format!("{parameters}{comma}{string_param}");
                first_param = false;
            }
            postfix = format!("_lt_{parameters}_gt")
        } else {
            postfix = "".to_string();
        }

        let result = format!("{prefix_name}{postfix}");
        let result_type_id = self.get_type_from_name(&node.name.location, &result);
        if result_type_id.is_none() {
            let new_type = self.table.generate_type(result.clone(), 0, vec![]);
            self.table.add_type_scope(new_type);

            let type_parameters = type_parameters_vec.join(", ");
            let define_name = format!("DEFINE_{name}({type_parameters})");
            self.add_header(define_name);
        }

        result
    }

    fn scoped_type(&mut self, node: &ScopedType, type_id: Option<TypeId>) -> String {
        let mut type_name = "".to_string();

        for type_identifier in node.scope.iter() {
            let scoped_name = self.type_identifier(type_identifier);
            return_err_if!(scoped_name == err!());

            type_name = format!("{}{}", type_name, scoped_name);
        }
        return_err_if!(type_name.is_empty());
        return_err_if!(type_name == err!());

        let found_type_id = self.get_type_from_name(&node.token.location, &type_name);
        return_err_if!(found_type_id == None);

        let mut type_name_with_postfixes = type_name;

        for postfix in node.type_modifiers.iter() {
            let type_name_with_new_postfix = match postfix {
                TypeModifier::Ptr => format!("{}_ptr", type_name_with_postfixes),
                TypeModifier::Array => format!("Vec_lt_{}_gt", type_name_with_postfixes)
            };
            
            if self.table.get_type_name(&type_name_with_new_postfix).is_err() {
                let header = match postfix {
                    TypeModifier::Ptr => format!("typedef {type_name_with_postfixes}* {type_name_with_new_postfix};"),
                    TypeModifier::Array => format!("DEFINE_vector({type_name_with_postfixes})")
                };
                self.add_header(header);
                let new_type = self.table.generate_type(type_name_with_new_postfix.clone(), 0, vec![]);
                self.table.add_type_scope(new_type);
            }

            type_name_with_postfixes = type_name_with_new_postfix;
        }

        return type_name_with_postfixes;
    }
    
    fn variable_declaration(&mut self, node: &VariableDeclaration, type_id: Option<TypeId>, scope: Scope) -> String {
        check_unexpected_type!(self, &node.var_name.location, type_id, None::<TypeId>);
        
        let type_name = self.evaluate_node(&node.var_type, None, scope, false);
        return_err_if!(type_name == err!());

        let var_type_id = self.get_type_from_name(&node.var_type.token.location, &type_name);
        return_err_if!(var_type_id == None);

        let name = node.var_name.value.clone();
        return_err_if!(name.is_empty());

        let var = self.table.generate_variable(name, var_type_id.unwrap(), node.var_value.is_some(), false, node.access_modifier.clone(), node.tags.clone());
        let idname = CodegenTable::get_idname_from_identifier_type(&var);
        self.table.add_identifier_scope(var);

        let first_part = format!("{} {}", type_name, idname);
        let second_part;

        if let Some(v) = &node.var_value {
            let value = self.evaluate_node(&v, var_type_id, scope, false);
            return_err_if!(value == err!());

            second_part = format!("= {}", value);
        } else {
            second_part = "".to_string();
        }
                
        let result = format!("{} {};", first_part, second_part);
        result
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