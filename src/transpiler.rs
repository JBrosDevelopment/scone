use std::usize;

use serde::Serialize;
#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, error_handling::{ErrorHandling, DEBUGGING, Message}};
#[allow(unused_imports)]
use crate::debug;

#[derive(Clone, Debug, Serialize)]
struct FunctionEntry {
    pub name: String,
    pub parameter_len: usize,
    pub returns: usize,
    pub contains_params: bool,
    pub first_default_param: usize
}

#[derive(Clone, Debug, Serialize)]
struct ValueTable {
    pub variables: Vec<String>,
    pub functions: Vec<FunctionEntry>,
    pub types: Vec<String>,
}

impl ValueTable {
    pub fn get_variable(&self, name: &String) -> Option<usize> { self.variables.iter().position(|x| x == name) }
    pub fn get_type(&self, name: &String) -> Option<usize> { self.types.iter().position(|x| x == name) }
    pub fn get_function(&self, name: &String, parameter_len: usize) -> Option<usize> { self.functions.iter().position(|x| &x.name == name && ((x.parameter_len == parameter_len) || (parameter_len > x.parameter_len && x.contains_params) || (parameter_len < x.parameter_len && parameter_len >= x.first_default_param))) }
    pub fn new() -> ValueTable {
        ValueTable {
            variables: vec![
                "_".to_string(),
            ],
            functions: vec![],
            types: vec![
                "i64".to_string(),
                "i32".to_string(),
                "i16".to_string(),
                "i8".to_string(),
                "u64".to_string(),
                "u32".to_string(),
                "u16".to_string(),
                "u8".to_string(),
                "f64".to_string(),
                "f32".to_string(),
                "bool".to_string(),
                "char".to_string(),
                "string".to_string(),
                "void".to_string(),
            ],
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
struct Variable {
    pub id: usize,
    pub type_id: usize,
    pub has_value: bool,
    pub needs_free: bool,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
struct Parameter {
    pub id: usize,
    pub type_id: usize,
    pub default: String,
    pub is_params: bool,
    pub is_const: bool,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
struct Function {
    pub id: usize,
    pub type_id: usize,
    pub parameters: Vec<Parameter>,
    pub has_body: bool,
}

pub fn codegen(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let mut transpiler = Transpiler::new(ast, code, path, macros);
    let code = transpiler.transpile();

    transpiler.output.print_messages();
    (code, transpiler.output)
}

pub struct Transpiler {
    ast: Vec<ASTNode>,
    output: ErrorHandling,
    macros: Macros,
    
    level: u32,
    current_variable_count: u32,
    header: String,
    includes: String,
    main: String,
    
    table: ValueTable,
    scope: Vec<Vec<Variable>>,
    functions: Vec<Function>,
    // structs: Vec<StructDeclaration>,
    // traits: Vec<TraitDeclaration>,
    // enums: Vec<EnumDeclaration>,

    lines_before: Vec<String>,
    lines_after: Vec<String>,
}

impl Transpiler {
    pub fn new(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> Transpiler {
        Transpiler { 
            ast, 
            output: ErrorHandling::new(path.clone(), code.clone()),
            macros,
            level: 0,
            includes: "".to_string(),
            header: "".to_string(),
            main: "".to_string(),
            table: ValueTable::new(),
            scope: vec![vec![]],
            functions: vec![],
            lines_before: vec![],
            lines_after: vec![],
            current_variable_count: 0,
        }
    }
    pub fn error(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t: &&Message| t.location.line == location.line) { // prevent extra errors on the same line
            return;
        }
        if DEBUGGING {
            self.output.error("transpiler error", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.output.error("transpiler error", message, help, location);
        }
    }
    pub fn warning(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra warnings on the same line
            return;
        }
        if DEBUGGING {
            self.output.warning("transpiler warning", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.output.warning("transpiler warning", message, help, location);
        }
    }
    pub fn message(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra messages on the same line
            return;
        }
        if DEBUGGING {
            self.output.message("transpiler message", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        } else {
            self.output.message("transpiler message", message, help, location);
        }
    }

    pub fn transpile(&mut self) -> String {
        for node in self.ast.clone() {
            let expression = self.evaluate_node(&node, None, false);

            for line in self.lines_before.clone() {
                self.main.push_str(self.level_string().as_str());
                self.main.push_str(&line);
                self.main.push('\n');
                println!("{}", line);
            }
            
            self.main.push_str(self.level_string().as_str());
            self.main.push_str(&expression);
            self.main.push('\n');
            println!("{}", expression);

            
            for line in self.lines_after.clone() {
                self.main.push_str(self.level_string().as_str());
                self.main.push_str(&line);
                self.main.push('\n');
                println!("{}", line);
            }

            self.lines_before.clear();
            self.lines_after.clear();
        }

        format!("//include:\n{}\n//header:\n\n{}\n\n//main:\n\n{}\n", self.includes, self.header.replace(";;", ";"), self.main.replace(";;", ";"))
    }

    fn is_constant(&self, node: &ASTNode) -> bool { 
        return true;
    }

    pub fn evaluate_node(&mut self, node: &ASTNode, current_type: Option<usize>, constant: bool) -> String {
        if constant && !self.is_constant(node) {
            self.error(line!(), "Cannot use non-constant in constant expression", "Expected a constant expression here. Cannot use non-constant in constant expression", &node.token.location);
        }
        match node.node.as_ref() {
            //NodeType::AnonymousType(ref v) => self.evaluate_anonymous_type(v, current_type),
            NodeType::ArrayExpression(ref v) => self.evaluate_array_expression(v, current_type),
            //NodeType::AsCast(ref v) => self.evaluate_as_cast(v, current_type),
            //NodeType::Assignment(ref v) => self.evaluate_assignment(v, current_type),
            //NodeType::Break(ref v) => self.evaluate_break(v, current_type),
            //NodeType::ClassDeclaration(ref v) => self.evaluate_class_declaration(v, current_type),
            //NodeType::CodeBlock(ref v) => self.evaluate_code_block(v, current_type),
            NodeType::Constant(ref v) => self.evaluate_constant(v, current_type),
            //NodeType::Continue(ref v) => self.evaluate_continue(v, current_type),
            //NodeType::Discard(ref v) => self.evaluate_discard(v, current_type),
            //NodeType::EnumDeclaration(ref v) => self.evaluate_enum_declaration(v, current_type),
            //NodeType::EnumDeclaration(ref v) => self.evaluate_enum_declaration(v, current_type),
            //NodeType::For(ref v) => self.evaluate_for(v, current_type),
            //NodeType::ForEach(ref v) => self.evaluate_for_each(v, current_type),
            NodeType::FunctionCall(ref v) => self.evaluate_function_call(v, current_type),
            NodeType::FunctionDeclaration(ref v) => self.evaluate_function_declaration(v, current_type),
            NodeType::Identifier(ref v) => self.evaluate_identifier(v, current_type),
            //NodeType::Indexer(ref v) => self.evaluate_indexer(v, current_type),
            //NodeType::If(ref v) => self.evaluate_if(v, current_type),
            //NodeType::IsCheck(ref v) => self.evaluate_is_check(v, current_type),
            //NodeType::LambdaExpression(ref v) => self.evaluate_lambda_expression(v, current_type),
            //NodeType::Match(ref v) => self.evaluate_match(v, current_type),
            //NodeType::ObjectInstantiation(ref v) => self.evaluate_object_instantiation(v, current_type),
            NodeType::Operator(ref v) => self.evaluate_operator(v, current_type),
            NodeType::ReturnExpression(ref v) => self.evaluate_return_expression(v, current_type),
            NodeType::ScopedExpression(ref v) => self.evaluate_scoped_expression(v, current_type),
            //NodeType::Shebang(ref v) => self.evaluate_shebang(v, current_type),
            //NodeType::StructDeclaration(ref v) => self.evaluate_struct_declaration(v, current_type),
            NodeType::TernaryOperator(ref v) => self.evaluate_ternary_operator(v, current_type),
            //NodeType::TraitDeclaration(ref v) => self.evaluate_trait_declaration(v, current_type),
            //NodeType::TupleDeclaration(ref v) => self.evaluate_tuple_declaration(v, current_type),
            //NodeType::TupleExpression(ref v) => self.evaluate_tuple_expression(v, current_type),
            NodeType::TypeDef(ref v) => self.evaluate_type_def(v, current_type),
            //NodeType::TypeDefinition(ref v) => self.evaluate_type_definition(v, current_type),
            NodeType::TypeIdentifier(ref v) => self.evaluate_type_identifier(v, current_type),
            NodeType::UnaryOperator(ref v) => self.evaluate_unary_operator(v, current_type),
            //NodeType::Use(ref v) => self.evaluate_use(v, current_type),
            NodeType::VariableDeclaration(ref v) => self.evaluate_variable_declaration(v, current_type),
            //NodeType::While(ref v) => self.evaluate_while(v, current_type),
            //NodeType::DeferStatement(ref v) => self.evaluate_defer_statement(v, current_type),
            _ => {
                self.error(line!(), "Unknown node type", format!("Node type: {} not implemented yet", node.node.to_string()).as_str(), &node.token.location);
                "".to_string()
            }
        }
    }

    fn get_current_type(&mut self, location: &Location, current_type: Option<usize>) -> String {
        if current_type.is_none() {
            self.error(line!(), "No current type", "Expected to return a specific type here", location);
            return "".to_string();
        }

        
        self.table.types[current_type.unwrap()].clone()
    }

    fn level_string(&self) -> String {
        "    ".repeat(self.level as usize)
    }

    fn parse_access_modifiers(&self, access_modifiers: &Vec<AccessModifier>) -> String { 
        let mut modifiers = "".to_string();
        for modifier in access_modifiers {
            modifiers = match modifier {
                AccessModifier::Const => "const ".to_string(),
                AccessModifier::Extern => "extern ".to_string(),
                AccessModifier::Static => "static ".to_string(),
                AccessModifier::Override => "override ".to_string(),
                AccessModifier::Virtual => "virtual ".to_string(),
                _ => "".to_string(),
            }
        }
        modifiers
    }

    fn add_to_scope(&mut self, err_location: &Location, name: &String, type_: &String, has_value: bool, is_ptr: bool) {
        if self.scope.last().is_none() {
            self.scope.push(vec![]);
        }

        if let Some(_) = self.table.get_variable(name) {
            self.error(line!(), "Variable already exists", format!("Variable {} already defined in this scope", name).as_str(), err_location);
            return;
        }
        let type_id = self.table.get_type(type_);
        if type_id.is_none() {
            self.error(line!(), "Type does not exist", format!("Type `{}` is not defined in this scope", type_).as_str(), err_location);
            return;
        }

        let id = self.table.variables.len();
        let type_id = type_id.unwrap();
        let variable = Variable { id, type_id, has_value, needs_free: false };
        self.table.variables.push(name.clone());
        self.scope.last_mut().unwrap().push(variable);
    }

    fn is_in_scope(&self, name: &String) -> Option<Variable> {
        for scope in &self.scope {
            for variable in scope {
                if self.table.variables[variable.id].clone() == *name {
                    return Some(variable.clone());
                }
            }
        }
        None
    }

    fn remove_scope(&mut self) -> Vec<String> {
        if self.scope.last().is_none() {
            self.scope.push(vec![]);
        }

        let mut frees = vec![];

        for variable in self.scope.last().unwrap() {
            if variable.needs_free {
                let name = self.table.variables[variable.id].clone();
                if variable.type_id == self.table.get_type(&"string".to_string()).unwrap() { 
                    //frees.push(format!("string_free({name});"));
                } else {
                    //frees.push(format!("safe_free({name});"));
                }
            }
        }

        self.scope.pop();

        frees
    }

    fn evaluate_variable_declaration(&mut self, node: &VariableDeclaration, current_type: Option<usize>) -> String {
        if current_type.is_some() {
            let ct = self.get_current_type(&node.var_name.location, current_type);
            self.error(line!(), "Unexpected variable declaration", format!("Expected type `{}` but found type declaration", ct).as_str(), &node.var_name.location);
        }
        // tags don't get transpiled
        // access modifiers:
        let modifiers = self.parse_access_modifiers(&node.access_modifier);
        // type:

        let type_ = self.evaluate_node(&node.var_type, None, false);
        let fixed_type = Self::unconvert_type_name(&type_);

        let type_id  = self.table.get_type(&fixed_type);
        if type_id.is_none() {
            self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", fixed_type).as_str(), &node.var_name.location);
        }

        // name:
        let name = node.var_name.value.clone();

        let is_ptr = fixed_type.ends_with("_ptr") || fixed_type == "string";
        
        if let Some(value) = &node.var_value {
            // value:
            let value = self.evaluate_node(value, type_id, false);
            
            self.add_to_scope(&node.var_name.location, &name, &fixed_type, true, is_ptr);
            format!("{modifiers}{type_} {name} = {value};")
        } else {
            self.add_to_scope(&node.var_name.location, &name, &fixed_type, false, is_ptr);
            format!("{modifiers}{type_} {name};")
        }
    }

    fn parse_type_reference(&self, node: &Vec<TypeModifier>) -> String {
        node.iter().filter(|x| x == &&TypeModifier::Ref).map(|_| "ref_").collect::<String>()
    }

    fn parse_type_pointer(&self, node: &Vec<TypeModifier>) -> String {
        node.iter().filter(|x| x == &&TypeModifier::Ptr).map(|_| "_ptr").collect::<String>()
    }

    fn parse_reference_or_pointer(&self, node: &Vec<TypeModifier>) -> String {
        node.iter().filter(|x| x != &&TypeModifier::Array).map(|_| "*").collect::<String>()
    }

    fn parse_type_array(&self, node: &Vec<TypeModifier>) -> String {
        node.iter().filter(|x| x == &&TypeModifier::Array).map(|_| "Vec_").collect::<String>()
    }

    fn parse_node_parameters(&mut self, node: &NodeParameters, current_type: Option<usize>) -> String {
        let mut s = "".to_string();
        for (index, param) in node.parameters.iter().enumerate() {
            s += self.evaluate_node(param, current_type, false).as_str();
            if index < node.parameters.len() - 1 {
                s += ", ";
            }
        }
        s
    }

    fn parse_typed_scope_type(&self, node: &Option<ScopeType>) -> String {
        if node.is_none() {
            return "".to_string()
        } 
        match node.clone().unwrap() {
            ScopeType::DoubleColon => "__".to_string(),
            ScopeType::Arrow => "___arrow____".to_string(),
            ScopeType::Dot => "___dot___".to_string() // shouldn't get here, but just in case
        }
    }

    fn convert_type_name(&mut self, name: &String) -> String {
        match name.as_str() {
            "i64" => "long".to_string(),
            "i32" => "int".to_string(),
            "i16" => "short".to_string(),
            "i8" => {
                if !self.header.contains("typedef signed char schar;") {
                    self.header.push_str("typedef signed char schar;\n");
                }
                "schar".to_string()
            }
            "f64" => "double".to_string(),
            "f32" => "float".to_string(),
            "u64" => {
                if !self.header.contains("typedef unsigned long ulong;") { 
                    self.header.push_str("typedef unsigned long ulong;\n");
                } 
                "ulong".to_string()
            }
            "u32" => {
                if !self.header.contains("typedef unsigned int uint;") { 
                    self.header.push_str("typedef unsigned int uint;\n");
                } 
                "uint".to_string()
            },
            "u16" => {
                if !self.header.contains("typedef unsigned short ushort;") { 
                    self.header.push_str("typedef unsigned short ushort;\n");
                } 
                "ushort".to_string()
            },
            "u8" => "char".to_string(),
            "bool" => {
                if !self.includes.contains("stdbool.h") { 
                    self.includes.push_str("#include <stdbool.h>\n");
                } 
                "bool".to_string()
            }
            _ => name.clone()
        }
    }

    fn unconvert_type_name(name: &String) -> String {
        match name.as_str() {
            "long" => "i64".to_string(),
            "int" => "i32".to_string(),
            "short" => "i16".to_string(),
            "schar" => "i8".to_string(),
            "double" => "f64".to_string(),
            "float" => "f32".to_string(),
            "ulong" => "u64".to_string(),
            "uint" => "u32".to_string(),
            "ushort" => "u16".to_string(),
            "char" => "u8".to_string(),
            _ => name.clone()
        }
    }

    fn parse_type_identifier(&mut self, node: &TypeIdentifier, current_type: Option<usize>) -> String {
        // scope type (`.` or `::`):
        let scope_type = self.parse_typed_scope_type(&node.scope_type);
        // name:
        let name = self.convert_type_name(&node.name.value);

        if self.table.get_type(&node.name.value).is_none() {
            self.error(line!(), "Type does not exist", format!("Type `{}` is not defined in this scope", name).as_str(), &node.name.location);
        }
        if let Some(ct) = current_type {
            let expected = self.table.types[ct].clone();
            if expected != name {
                self.error(line!(), "Type does not match current type", format!("Type mismatch, expected type `{}` but found type `{}`", expected, name).as_str(), &node.name.location);
            }
        }

        if let Some(np) = &node.type_parameters {
            // generics
            let generics = self.parse_node_parameters(&np, current_type);
            
            format!("{scope_type}{name}_{generics}")
        } else {
            format!("{scope_type}{name}")
        }
    }

    fn parse_type_scope(&mut self, node: &Vec<TypeIdentifier>, current_type: Option<usize>) -> String {
        let mut scope = "".to_string();
        for n in node {
            scope += self.parse_type_identifier(n, current_type).as_str();
        }

        scope
    }

    fn evaluate_type_identifier(&mut self, node: &ScopedType, current_type: Option<usize>) -> String {
        // scope:
        let scope = self.parse_type_scope(&node.scope, current_type);
        // type references:
        let type_refs = self.parse_type_reference(&node.is_ptr_or_ref);
        // type pointers:
        let type_pointers = self.parse_type_pointer(&node.is_ptr_or_ref);
        // references or pointers:
        let ref_or_ptr = self.parse_reference_or_pointer(&node.is_ptr_or_ref);
        // handle array if there:
        let array = self.parse_type_array(&node.is_ptr_or_ref);

        let type_result_without_array = format!("{type_refs}{scope}{type_pointers}");
        let type_result = format!("{array}{type_result_without_array}");
        let result = format!("{array}{scope}{ref_or_ptr}");

        if node.is_ptr_or_ref.iter().filter(|x| x != &&TypeModifier::Array).collect::<Vec<_>>().len() > 0 {
            if !self.header.contains(format!("typedef {result} {type_result};").as_str()) {
                self.header += format!("typedef {result} {type_result};\n").as_str();
                self.table.types.push(type_result.clone());
            }
        } else if node.is_ptr_or_ref.iter().filter(|x| x == &&TypeModifier::Array).collect::<Vec<_>>().len() > 0 {
            if !self.header.contains(format!("VECTOR_DEFINE({type_result_without_array});").as_str()) {
                self.header += format!("VECTOR_DEFINE({type_result_without_array});\n").as_str();
            }
            self.table.types.push(type_result.clone());
        }

        if let Some(id) = current_type {
            let ct = self.table.types[id].clone();
            if ct != type_result {
                self.error(line!(), "Type mismatch", format!("Expected type `{}` but found `{}`", ct, type_result).as_str(), &node.scope[0].name.location);
            }
        }

        type_result
    }

    fn evaluate_constant(&mut self, node: &ConstantNode, current_type: Option<usize>) -> String { 
        let expected = self.get_current_type(&node.value.location, current_type);
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

    fn get_temp_variable_name(&mut self) -> String {
        self.current_variable_count += 1; 
        format!("temp_{}", self.current_variable_count) 
    }

    fn evaluate_array_expression(&mut self, node: &NodeParameters, current_type: Option<usize>) -> String {
        let mut type_ = self.get_current_type(&node.token.location, current_type);
        if type_.starts_with("Vec_") {
            type_ = type_.chars().skip(4).collect();
        }
        let name = self.get_temp_variable_name();
        let array_innerds = self.parse_node_parameters(&node, current_type);
        let c_array = format!("{{ {array_innerds} }}");

        let line_before = format!("{type_} {name}[] = {c_array};");
        self.lines_before.push(line_before);

        let len = node.parameters.len();

        format!("Vec_{type_}_new({name}, {len})")
    }

    fn parse_identifier(&mut self, node: &Identifier, current_type: Option<usize>) -> String {
        // scope type (`.` or `::`):
        let scope_type = self.parse_typed_scope_type(&node.scope_type);
        // expression:
        let expression = self.evaluate_node(&node.expression, current_type, false);

        if let Some(np) = &node.type_parameters {
            // generics
            let generics = self.parse_generics_type_parameters(&np.parameters);
            
            format!("{scope_type}{expression}_{generics}_c_")
        } else {
            format!("{scope_type}{expression}")
        }
    }

    fn evaluate_scoped_expression(&mut self, node: &ScopedIdentifier, current_type: Option<usize>) -> String {
        let mut scope = "".to_string();
        for n in node.scope.iter() {
            scope += self.parse_identifier(n, current_type).as_str();
        }
        scope
    }

    fn parse_defined_node_parameters(&mut self, node: &Vec<DefinedNodeParameter>) -> String {
        let mut result = "".to_string();

        for (i, param) in node.iter().enumerate() {
            let type_ = self.evaluate_node(&param.ty, None, false);
            let fixed_type = Self::unconvert_type_name(&type_);
            let name = param.name.value.clone();

            if self.table.get_type(&fixed_type).is_none() {
                self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", fixed_type).as_str(), &param.ty.token.location);
            }

            result.push_str(format!("{type_} {name}").as_str());

            if i < node.len() - 1 {
                result.push_str(", ");
            }
        }

        result
    }

    fn parse_generics_inside_undefined(&mut self, node: &AnonymousTypeParameters) -> String {
        node.parameters.iter().map(|x| x.name.value.clone()).collect::<Vec<String>>().join(", ")
    }
    
    fn parse_anonymous_type_parameters(&mut self, node: &AnonymousTypeParameters) -> String {
        let mut result = "".to_string();
        for (i, param) in node.parameters.iter().enumerate() {
            result.push('#');
            result.push('#');
            result.push_str(param.name.value.as_str());
            result.push('#');
            result.push('#');
            if i < node.parameters.len() - 1 {
                result.push_str("_");
            }
        }

        result
    }
    
    fn parse_generics_type_parameters(&mut self, node: &Vec<Box<ASTNode>>) -> String {
        let mut result = "".to_string();
        for (i, param) in node.iter().enumerate() {
            let expression = self.evaluate_node(&param, None, false);

            if self.table.get_type(&expression).is_none() {
                self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", expression).as_str(), &param.token.location);
            }

            result.push_str(&expression);
            if i < node.len() - 1 {
                result.push_str("_");
            }
        }

        result
    }

    fn parse_undefined_function_declaration(&mut self, node: &FunctionDeclaration, parameters: Vec<Variable>) -> String {
        if node.type_parameters.is_none() {
            self.error(line!(), "Error parsing function declaration", "Function declaration is missing type parameters.", &node.name.location);
            return "".to_string();
        }
        let mut result = "#define DEFINE_".to_string();
        let mut define_name = "".to_string();

        result.push_str(&node.name.value);
        result.push('_');
        define_name.push_str(&node.name.value);
        define_name.push('_');
        
        let type_parameters = self.parse_anonymous_type_parameters(&node.type_parameters.clone().unwrap());
        let type_parameters_len = node.type_parameters.clone().unwrap().parameters.len().to_string();
        result.push_str(&type_parameters_len);
        define_name.push_str(&type_parameters);
        
        let parameter_len = node.parameters.len().to_string();
        result.push('_');
        result.push_str(&parameter_len);
        define_name.push('_');
        define_name.push_str(&parameter_len);

        result.push('(');
        let type_parameters = self.parse_generics_inside_undefined(&node.type_parameters.clone().unwrap());
        result.push_str(&type_parameters);
        result.push(')');

        result.push(' ');
        result.push('\\');
        result.push('\n');

        let mut func_dec_no_body = node.clone();
        func_dec_no_body.body = None;
        func_dec_no_body.type_parameters = None;
        func_dec_no_body.name.value = define_name.clone();
        let func_decl = self.evaluate_function_declaration(&func_dec_no_body, None);
        let func_decl_without_semicolon = func_decl.chars().take(func_decl.len() - 1).collect::<String>();
        result.push_str(&func_decl_without_semicolon);
        
        if let Some(body) = node.body.clone() {
            result.push(' ');
            result.push('\\');
            result.push('\n');

            let return_type = self.evaluate_node(&node.return_type, None, false);
            let returns = self.table.get_type(&return_type);
            if returns.is_none() {
                self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", return_type).as_str(), &node.return_type.token.location);
            }

            let body = self.parse_undefined_body_region(&body, returns, parameters);
            result.push_str(&body);
        } 

        result.push('\n');

        result
    }

    fn parse_undefined_body_region(&mut self, node: &BodyRegion, current_type: Option<usize>, parameters: Vec<Variable>) -> String {
        let body = self.evaluate_code_block(&node, &"".to_string(), parameters, current_type);
        let lines: Vec<&str> = body.split('\n').filter(|x| !x.trim().is_empty()).collect();
        let mut result = "".to_string();
        for (i, line) in lines.iter().enumerate() {
            result.push_str(line);

            if i < lines.len() - 1 {
                result.push(' ');
                result.push('\\');
                result.push('\n');
            }
        }
        result
    }

    fn evaluate_code_block(&mut self, node: &BodyRegion, insert: &String, parameters: Vec<Variable>, current_type: Option<usize>) -> String {
        self.scope.push(parameters);

        let mut result = "{\n".to_string();
        let mut return_expression = "".to_string();

        let line_before_copy = self.lines_before.clone();
        let line_after_copy = self.lines_after.clone();
        self.lines_before.clear();
        self.lines_after.clear();

        self.level += 1;
        for n in node.body.clone() {
            if !return_expression.is_empty() {
                self.warning(line!(), "Dead code", "Code will not be executed after return statement", &n.token.location);
                break;
            }
            if let NodeType::ReturnExpression(ref ret) = n.node.as_ref() {
                return_expression = self.evaluate_return_expression(&ret, current_type);
                continue;
            } 

            let expression = self.evaluate_node(&n, None, false);
            
            for line in self.lines_before.clone() {
                result.push_str(self.level_string().as_str());
                result.push_str(&line);
                result.push(';');
                result.push('\n');
            }
            
            result.push_str(&self.level_string());
            result.push_str(&expression);
            result.push(';');
            result.push('\n');

        
            for line in self.lines_after.clone() {
                result.push_str(self.level_string().as_str());
                result.push_str(&line);
                result.push('\n');
                result.push(';');
            }

            self.lines_before.clear();
            self.lines_after.clear();
        }

        self.lines_before = line_before_copy;
        self.lines_after = line_after_copy;

        let frees = self.remove_scope();
        for free in frees {
            result.push_str(&self.level_string());
            result.push_str(&free);
            result.push('\n');
        }
        
        if !insert.is_empty() {
            result.push_str(&self.level_string());
            result.push_str(&insert);
            result.push('\n');
        }

        if !return_expression.is_empty() {
            result.push_str(&self.level_string());
            result.push_str(&return_expression);
            result.push('\n');
        }

        self.level -= 1;

        result.push_str(&self.level_string());
        result.push('}');
        result.push('\n');

        result
    }

    fn evaluate_function_declaration(&mut self, node: &FunctionDeclaration, current_type: Option<usize>) -> String {
        if current_type.is_some() {
            let ct = self.get_current_type(&node.name.location, current_type);
            self.error(line!(), "Error parsing function declaration", format!("Expected type `{}` but found function declaration", ct).as_str(), &node.name.location);
        }

        // access modifiers:
        let mut modifiers = self.parse_access_modifiers(&node.access_modifier);
        let mut include_io = false;

        if !modifiers.is_empty() && node.type_parameters.is_some() {
            self.error(line!(), "Unexpected access modifier", "External function declarations can not have access modifier.", &node.name.location);
            return "".to_string();
        } else if modifiers.contains("extern") && matches!(node.name.value.as_str(), "printf" | "scanf" | "vprintf" | "vscanf" | "sprintf" | "sscanf") {
            if !self.includes.contains("#include <stdio.h>") {
                self.includes += "#include <stdio.h>\n";
            }
            include_io = true;
        } else if modifiers.contains("extern") {
            self.warning(line!(), "Function is external", "This function is external and won't be managed by scone. If any errors occur in it, they will be handled by the C compiler and not scone", &node.name.location);
            modifiers = modifiers.replace("extern ", "");
        }

        if let Some(f) = self.table.get_function(&node.name.value, node.parameters.len()) {
            if self.table.functions[f].parameter_len == node.parameters.len() {
                self.error(line!(), "Function already exists", format!("The function `{}` with `{}` parameters is already defined", node.name.value, node.parameters.len()).as_str(), &node.name.location);
            }
        }

        let name = node.name.value.clone();
        let parameters = self.parse_defined_node_parameters(&node.parameters);
        let mut return_type = self.evaluate_node(&node.return_type, None, false);

        let mut returns = self.table.get_type(&Self::unconvert_type_name(&return_type));
        if returns.is_none() {
            self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", return_type).as_str(), &node.return_type.token.location);
            returns = Some(0);
        }
        
        let variable_len_before_function = self.table.variables.len();
        let mut function_parameters: Vec<Parameter> = vec![];
        for (i, p) in node.parameters.iter().enumerate() { 
            function_parameters.push(Parameter {
                is_const: p.is_const,
                type_id: {
                    let type_node_str = self.evaluate_node(&p.ty, None, false);
                    let type_node_str_fixed = Self::unconvert_type_name(&type_node_str);
                    self.table.get_type(&type_node_str_fixed).unwrap_or_else(|| {
                        self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", type_node_str_fixed).as_str(), &p.ty.token.location);
                        return 0
                    })
                },
                id: variable_len_before_function + i,
                default: {
                    if p.default_value.is_none() { 
                        "".to_string() 
                    } else { 
                        let type_node_str = self.evaluate_node(&p.ty, None, false);
                        let type_node_str_fixed = Self::unconvert_type_name(&type_node_str);
                        let type_id = self.table.get_type(&type_node_str_fixed).unwrap_or_else(|| {
                            self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", type_node_str_fixed).as_str(), &p.ty.token.location);
                            return 0
                        });
                        self.evaluate_node(&p.default_value.as_ref().unwrap(), Some(type_id), true) 
                    }
                },
                is_params: p.params
            });
        }

        let function = Function { 
            id: self.table.functions.len(),
            type_id: returns.unwrap(),
            has_body: node.body.is_some(),
            parameters: function_parameters.clone(),
        };
        self.functions.push(function);
        self.table.functions.push(FunctionEntry {
            name: name.clone(), 
            parameter_len: node.parameters.len(), 
            returns: returns.unwrap(), 
            contains_params: node.parameters.iter().any(|x| x.params) ,
            first_default_param: node.parameters.iter().position(|x| x.default_value.is_some()).unwrap_or(usize::MAX)
        });
  
        let variables: Vec<Variable> = function_parameters.iter().map(|x| Variable {
            has_value: true,
            id: x.id,
            type_id: x.type_id,
            needs_free: false // don't want to free a parameter
        }).collect();
        
        if include_io {
            return "".to_string();
        }

        for i in 0..variables.len() {
            self.table.variables.push(node.parameters[i].name.value.clone());
        }

        if node.type_parameters.is_some() {
            let define_shabang = self.parse_undefined_function_declaration(node, variables);
            self.header += define_shabang.as_str();
            self.header += "\n";
            "".to_string()
        } else {
            if let Some(body) = node.body.clone() {
                let insert = if name == "main" && return_type == "void" {
                    return_type = "int".to_string();
                    "return 0;".to_string()
                } else {
                    "".to_string()
                };

                let body = self.evaluate_code_block(&body, &insert, variables, returns);
                format!("{modifiers}{return_type} {name}({parameters})\n{body}")
            } else {
                format!("{modifiers}{return_type} {name}({parameters});")
            }
        }
    }

    fn evaluate_return_expression(&mut self, node: &Option<Box<ASTNode>>, current_type: Option<usize>) -> String {
        if let Some(node) = node.clone() { 
            let expression = self.evaluate_node(&node, current_type, false); 
            format!("return {expression};")
        } else {
            "return;".to_string()
        }
    }

    fn evaluate_identifier(&mut self, node: &Box<Token>, current_type: Option<usize>) -> String {
        if let Some(var) = self.is_in_scope(&node.value) {
            if current_type.is_some() && var.type_id != current_type.unwrap() {
                let expected = self.get_current_type(&node.location, current_type);
                let found = self.get_current_type(&node.location, Some(var.type_id));
                if !(expected.starts_with("void") && found.ends_with("_ptr")) {
                    self.error(line!(), "Error parsing variable", format!("Expected type `{}` but found type `{}`", expected, found).as_str(), &node.location);
                }
            }
        } else {
            self.error(line!(), "Variable not found", format!("The variable `{}` is not defined", node.value).as_str(), &node.location);
        }
        node.value.clone()
    }

    fn evaluate_operator(&mut self, node: &Expression, current_type: Option<usize>) -> String {
        let l_expression = self.evaluate_node(&node.left, current_type, false);
        let r_expression = self.evaluate_node(&node.right, current_type, false);
        let operator = node.operator.value.clone();
        match node.operator.token_type {
            TokenType::As => {
                format!("({r_expression}){l_expression}")
            }
            _ => {
                format!("{l_expression} {operator} {r_expression}")
            }
        }
    }

    fn evaluate_function_call(&mut self, node: &FunctionCall, current_type: Option<usize>) -> String {
        let function_id = self.table.get_function(&node.name.value, node.parameters.parameters.len());
        if function_id.is_none() {
            self.error(line!(), "Function not found", format!("The function `{}` is not defined", node.name.value).as_str(), &node.name.location);
            return "".to_string();
        }
        let function_id = function_id.unwrap();
        
        if current_type.is_some() {
            let function_entry = &self.table.functions[function_id];
            let function_return_type_id = function_entry.returns;
            let return_type = self.table.types[function_return_type_id].clone();
            let expected_type = self.get_current_type(&node.name.location, current_type);
            if expected_type != return_type {
                self.error(line!(), "Invalid return type", format!("Function `{}` returns `{}`, but expected `{}`", node.name.value, return_type, expected_type).as_str(), &node.name.location);
                return "".to_string();
            }
        }

        let call_name;
        if let Some(type_parameters) = node.type_parameters.clone() {
            let mut header_name = "DEFINE_".to_string();
            header_name.push_str(&node.name.value);
            header_name.push('_');
    
            let tp_len = type_parameters.len().to_string();
            header_name.push_str(&tp_len);
            let type_parameters_generics = self.parse_generics_type_parameters(node.type_parameters.as_ref().unwrap());
    
            let parameter_len = node.parameters.parameters.len();
            header_name.push('_');
            header_name.push_str(&parameter_len.to_string());
            
            call_name = format!("{name}_{type_parameters_generics}_{parameter_len}", name = node.name.value);

            header_name.push('(');
            for (i, type_parameter) in type_parameters.iter().enumerate() {
                let parameter = self.evaluate_node(&type_parameter, current_type, false);
                header_name.push_str(&parameter);
                if i < type_parameters.len() - 1 {
                    header_name.push(',');
                    header_name.push(' ');
                }
            }
            header_name.push(')');

            if !self.header.contains(&header_name) {
                self.header += format!("{header_name};\n").as_str();   
            }
        } else {
            call_name = node.name.value.clone();
        }


        let parameters = self.parse_argument_node_parameters(&node.parameters, function_id);

        format!("{call_name}({parameters})")
    }

    fn parse_argument_node_parameters(&mut self, node: &NodeParameters, function_id: usize) -> String {
        let function = self.functions[function_id].clone();
        let mut result = "".to_string();
        for (i, parameter) in node.parameters.iter().enumerate() {
            if function.parameters[i].is_params {
                // handle params
                // go to line before and initialize array
                // pass the pointer into the function
                break;
            }
            let parameter = self.evaluate_node(&parameter, Some(function.parameters[i].type_id), function.parameters[i].is_const);
            result.push_str(&parameter);
            if i < function.parameters.len() - 1 {
                result.push(',');
                result.push(' ');
            }
        }
        if function.parameters.len() > node.parameters.len() {
            // handle default parameters
            for i in node.parameters.len()..function.parameters.len() {
                let default_value = function.parameters[i].default.clone();
                result.push_str(&default_value);
                if i < function.parameters.len() - 1 {
                    result.push(',');
                    result.push(' ');
                }                
            }
        }
        result
    }

    fn evaluate_ternary_operator(&mut self, node: &TernaryConditional, current_type: Option<usize>) -> String {
        let condition = self.evaluate_node(&node.condition, current_type, false);

        let then_expression = if node.then.token.token_type == TokenType::Underscore {
            "0".to_string()
        } else {
            self.evaluate_node(&node.then, current_type, false)
        };
        let otherwise_expression = if node.else_then.token.token_type == TokenType::Underscore {
            "0".to_string()
        } else {
            self.evaluate_node(&node.else_then, current_type, false)
        };

        format!("{condition} ? {then_expression} : {otherwise_expression}")
    }

    fn evaluate_type_def(&mut self, node: &TypeDefDeclaration, current_type: Option<usize>) -> String {
        if current_type.is_some() {
            let current_type = self.get_current_type(&node.name.location, current_type);
            self.error(line!(), "Unexpected Type definition", format!("Expected type `{}` but found type definition", current_type).as_str(), &node.name.location);
        } 
        let type_name = node.name.value.clone();
        let type_body = self.evaluate_node(&node.type_definition, None, false);
        if self.table.get_type(&type_name).is_some() {
            self.error(line!(), "Type already defined", format!("Type `{}` is already defined in this scope", type_name).as_str(), &node.name.location);
        }
        if self.table.get_type(&type_body).is_none() {
            self.error(line!(), "Type not defined", format!("Type `{}` is not defined in this scope", type_body).as_str(), &node.name.location);
        }
        self.table.types.push(type_name.clone());
        format!("typedef {type_body} {type_name};")
    }

    fn evaluate_unary_operator(&mut self, node: &UnaryExpression, mut current_type: Option<usize>) -> String {
        let operator = node.operator.value.clone();

        let ct = self.get_current_type(&node.operator.location, current_type);
        if operator == "&" && ct.ends_with("_ptr") {
            let new_ct_unfixed = ct.clone().chars().take(ct.len() - 4).collect::<String>();
            let new_ct = Self::unconvert_type_name(&new_ct_unfixed);
            let mut new_ct_id = self.table.get_type(&new_ct);
            if new_ct_id.is_none() {
                if !self.header.contains(format!("typedef {ct}* {new_ct};").as_str()) {
                    self.header += format!("typedef {ct}* {new_ct};\n").as_str();
                }
                new_ct_id = Some(self.table.types.len());
            }
            self.table.types.push(new_ct);
            current_type = new_ct_id;
        }

        let expression = self.evaluate_node(&node.operand, current_type, false);
        format!("{operator}{expression}")
    }
}