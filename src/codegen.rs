//
// Right now, Im not going to delete this file incase I need to refer back to it, 
// but I'm going to split this step up into a checker, and then a transpiler. So right 
// now I'm going to work on the checker and leave this file to edit later.
//
/*

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

macro_rules! return_if {
    ($condition:expr, $err_msg:expr) => {
        if $condition {
            return $err_msg;
        }
    };
}

macro_rules! add_macro_and_header {
    ($self:expr, $type_to_add:expr, $header:expr) => {
        if !$type_to_add.contains(&$header) {
            $self.add_header($header.clone());
            $type_to_add.push($header);
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
    pub outc: String,
    headers: String,
    includes: String,
    defined_macros: Vec<String>,
    defined_typedefs: Vec<String>,
}

impl GenerateC {
    pub fn new(transpiler: Transpiler) -> GenerateC {
        GenerateC { table: CodegenTable::new(), transpiler: transpiler, outc: "".to_string(), includes: "".to_string(), defined_macros: vec![], defined_typedefs: vec![], headers: "".to_string() }
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
        self.add_header("// Default types".to_string());

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

        let _char_ptr_ptr = self.table.generate_type("char_ptr_ptr".to_string(), 0, vec![]);
        self.table.add_type_scope(_char_ptr_ptr); // type_id = 14
        self.add_header("typedef char** char_ptr_ptr;".to_string());
        self.defined_typedefs.push("typedef char** char_ptr_ptr".to_string());

        self.add_header("".to_string());
        
        let _entry_argc = self.table.generate_variable("ENTRY_ARGC".to_string(), 5, true, false, vec![AccessModifier::Const], vec![], Location::new_empty());
        self.table.add_identifier_scope(_entry_argc); // id = 1
        self.add_header("const int v1; // ENTRY_ARGC".to_string());
        
        let _entry_argv = self.table.generate_variable("ENTRY_ARGV".to_string(), 14, true, false, vec![AccessModifier::Const], vec![], Location::new_empty());
        self.table.add_identifier_scope(_entry_argv); // id = 2
        self.add_header("const char_ptr_ptr v2; // ENTRY_ARGV\n".to_string());
    }

    fn add_header(&mut self, line: String) {
        if self.table.is_inside_macro() {
            self.table.add_macro_header(line);
        } else {
            self.headers = format!("{}\n{}", self.headers, line);
        }
    }
    fn add_include(&mut self, line: String) {
        self.includes = format!("{}#include {}\n", self.includes, line);
    }
    fn indent(scope: Scope) -> String {
        "\t".repeat(scope as usize)
    }

    fn entry_point(&mut self) {
        let function = self.table.get_all_functions().iter().find(|function| function.tags.contains(&Tag::Entry)).cloned();
        let function_name;
        match function {
            Some(function) => {
                let return_type = self.get_type(&function.location, Some(function.type_id));
                if !matches!(return_type.as_str(), "void" | "i8" | "i16" | "i32" | "u16" | "u8" | "bool") {
                    self.error(line!(), "Invalid return type for entry point", format!("Entry point function `{}` has invalid return type `{}`. Expects `void`, `i8`, `i16`, `i32`, `u16`, `u8` or `bool", function.name, return_type).as_str(), &function.location);
                    return;
                }
                if function.parameters.len() != 0 {
                    self.error(line!(), "Invalid parameters for entry point", format!("Entry point function `{}` has invalid parameters. Expects no parameters", function.name).as_str(), &function.location);
                    return;
                }
                if function.type_parameters.len() != 0 {
                    self.error(line!(), "Invalid type parameters for entry point", format!("Entry point function `{}` has invalid type parameters. Expects no type parameters", function.name).as_str(), &function.location);
                    return;
                }
                function_name = self.table.get_idname_from_identifier_type(&IdentifierType::Function(function));
            },
            None => {
                let first_function = self.transpiler.ast.iter().find(|node| match node.node.as_ref() {
                    NodeType::FunctionDeclaration(_) => true,
                    _ => false
                }).cloned();
                if let Some(first_function) = first_function {
                    self.error(line!(), "No entry point", "No entry point found. Use `#! entry` tag to set function as entry point", &first_function.token.location);
                }
                let first_line = self.transpiler.ast.get(0).unwrap_or(&ASTNode::err()).clone();
                self.error(line!(), "No entry point", "No entry point found. Use `#! entry` tag to set function as entry point", &first_line.token.location);
                return;
            }
        }

        let main_function = format!("\n\n// entry point\nint main(char** argv) {{\n    v2 = argv;\n    v1 = argc;\n    return {function_name}();\n}}");
        self.outc = format!("{}\n{}", self.outc, main_function);
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

    fn get_identifier_from_id(&mut self, location: &Location, id: Id) -> Option<IdentifierType> {
        if let Ok(ident) = self.table.get_identifer_type_by_id(id) {
            Some(ident)
        } else {
            self.error(line!(), "Identifier does not exist", format!("Identifier with id `{id}` does not exist in this scope").as_str(), &location);
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

    fn wrap_type(&mut self, location: &Location, wrapper: TypeId, type_to_wrap: TypeId) -> Option<TypeHolder> {
        let wrapper_type;
        if let Ok(v) = self.table.get_type_id(wrapper) {
            match v {
                TypeType::TypeParameter(_) => {
                    self.error(line!(), "Can not wrap type in generic type", format!("Type `{}` is a generic type and can not be used to wrap another type", wrapper).as_str(), &location);
                    return None;
                }
                TypeType::Type(v) => wrapper_type = v
            }
        } else {
            self.error(line!(), "Type does not exist", format!("Type `{}` does not exist in this scope", wrapper).as_str(), &location);
            return None;
        }
        let type_to_wrap_type;
        if let Ok(v) = self.table.get_type_id(type_to_wrap) {
            type_to_wrap_type = v;
        } else {
            self.error(line!(), "Type does not exist", format!("Type `{}` does not exist in this scope", type_to_wrap).as_str(), &location);
            return None;
        }

        if wrapper_type.type_parameter_count == 0 {
            self.error(line!(), "Can not use generics with a non generic type", format!("Type `{}` is a non generic type and can not be used to wrap another type", wrapper).as_str(), &location);
            return None;
        } else if wrapper_type.type_parameter_count != 1 {
            self.error(line!(), "Generic type has more than one generic, but expected a type with one generic", format!("Type `{}` has more than one generic, but expected a type with one generic", wrapper).as_str(), &location);
            return None;
        }

        let type_to_wrap_type_name = match type_to_wrap_type {
            TypeType::Type(v) => v.name,
            TypeType::TypeParameter(v) => v.name
        };

        let new_type_name = format!("{}_lt_##{}##_gt", wrapper_type.name, type_to_wrap_type_name);
        
        let new_type;
        if let Ok(t) = self.table.get_type_name(&new_type_name) {
            new_type = t
        } else {            
            let header = format!("DEFINE_{}_{}({})", wrapper_type.name, wrapper_type.type_parameter_count, type_to_wrap_type_name);
            add_macro_and_header!(self, self.defined_macros, header);
            new_type = self.table.generate_type(new_type_name, 0, vec![]);
            self.table.add_type_scope(new_type.clone());
        }

        Some(match new_type {
            TypeType::Type(v) => v,
            _ => unreachable!("New type should be a type")
        })
    }

    fn defined_node_parameters(&mut self, parameters: &Vec<DefinedNodeParameter>) -> (String, Vec<ParameterHolder>) {
        let mut params = vec![];
        let mut parameter_holders = vec![];

        for p in parameters.iter() {
            let name = p.name.value.clone();
            return_if!(name.is_empty(), (err!(), vec![]));

            let type_name = self.evaluate_node(&p.ty, None, false);
            return_if!(type_name == err!(), (err!(), vec![]));
            
            let type_id = self.get_type_from_name(&p.ty.token.location, &type_name);
            return_if!(type_id == None, (err!(), vec![]));

            if p.params {
                // this should all be done at `[]`, what i need to do is check if `[]` is present
                if !type_name.starts_with("vector") {
                    self.error(line!(), "Invalid type", format!("Parameter `{}` is not a vector, use wrap in `vector` or use `[]` instead: `i32[]: values ...`", p.name.value).as_str(), &p.name.location);
                }
                // // add vector wrapper to type: `i32: values ...` -> `vector<i32>: values`
                // let vector_type_id = self.get_type_from_name(&p.name.location, &"vector".to_string());
                // return_if!(vector_type_id == None, (err!(), vec![]));

                // let new_type = self.wrap_type(&p.name.location, vector_type_id.unwrap(), unchecked_type_id.unwrap());
                // return_if!(new_type == None, (err!(), vec![]));
                // let new_type = new_type.unwrap();

                // type_id = Some(new_type.type_id);
                // type_name = new_type.name;
            } 
            return_if!(type_id == None, (err!(), vec![]));

            let default_value;
            if let Some(d) = &p.default_value {
                let expression = self.evaluate_node(d, type_id, false);
                return_if!(expression == err!(), (err!(), vec![]));

                default_value = Some(expression);
            } else {
                default_value = None;
            }

            let param_holder = self.table.generate_parameter(p.name.value.clone(), type_id.unwrap(), p.is_const, p.params, default_value.clone(), p.name.location.clone());
            let idname = CodegenTable::get_idname_from_parameter_holder(&param_holder);
            parameter_holders.push(param_holder);

            let result;
            if let Some(p) = &default_value {
                result = format!("{type_name} {idname} = {p}");
            } else {
                result = format!("{type_name} {idname}");
            }
            params.push(result);

            if p.params {
                // add extra parameter for vector size
                
                let size_param = "int __va_size__".to_string();
                let size_param_holder = self.table.generate_parameter("__va_size__".to_string(), 5, true, false, None, p.name.location.clone());
                params.push(size_param);
                parameter_holders.push(size_param_holder);
            }
        }

        let result = params.join(", ");
        (format!("({result})"), parameter_holders)
    }

    fn constraints_are_valid(&mut self, node: &ASTNode, id: Option<Id>) -> bool {
        match node.node.as_ref() {
            NodeType::Operator(v) => {
                let left = self.constraints_are_valid(&v.left, id);
                let right = self.constraints_are_valid(&v.right, id);
                match v.operator.token_type {
                    TokenType::And => left && right,
                    TokenType::Or => left || right,
                    _ => {
                        self.error(line!(), "Invalid operator in constraints", "Only `&&` and `||` are allowed in constraints", &node.token.location);
                        return false;
                    }
                }
            }
            NodeType::Identifier(v) => {
                let trait_identifier_type = self.get_identifier_from_name(&v.location, &v.value.to_string());
                return_if!(trait_identifier_type == None, false);
                let _trait = match trait_identifier_type.unwrap() {
                    IdentifierType::Trait(s) => s,
                    _ => {
                        self.error(line!(), "Invalid constraint", "Only traits are allowed in constraints: `T is Trait1 || Trait2`", &node.token.location);
                        return false;
                    }
                };

                return_if!(id == None, true);
                let id = id.unwrap();

                let struct_identifier_type = self.get_identifier_from_id(&node.token.location, id);
                return_if!(struct_identifier_type == None, false);
                let _struct = match struct_identifier_type.unwrap() {
                    IdentifierType::Struct(s) => s,
                    _ => {
                        self.error(line!(), "Invalid constraint", "Only structs are allowed in constraints: `T is Trait1 || Trait2`", &node.token.location);
                        return false;
                    }
                };

                _struct.inherits.iter().any(|t| t.name == _trait.name)
            }
            _ => {
                self.error(line!(), "Invalid constraint", "Only `&&` and `||` are allowed in constraints: `T is Trait1 || Trait2`", &node.token.location);
                return false;
            }
        }
    }

    fn type_parameters(&mut self, type_parameters: &Vec<AnonymousType>) -> Vec<TypeType> {
        let mut result = vec![];
        for tp in type_parameters {
            if let Some(constraint) = &tp.constraints {
                if self.constraints_are_valid(constraint, None) {
                    return vec![];
                }
            }

            let holder = self.table.generate_type_parameter(tp.name.value.clone());
            result.push(holder);
        }
        result
    }

    fn scope_type(&mut self, node: &Option<ScopeType>) -> String {
        match &node {
            Some(s) => match s {
                ScopeType::Arrow => "->".to_string(),
                ScopeType::Dot => ".".to_string(),
                ScopeType::DoubleColon => "__dc__".to_string(),
            },
            None => "".to_string(),
        }
    }

    fn get_type_parameters(&mut self, type_parameters: &Option<NodeParameters>) -> (String, Vec<String>) {
        let mut type_parameters_vec = vec![];
        if let Some(tp) = &type_parameters {
            let mut parameters = "".to_string();
            let mut first_param = true;
            for param in &tp.parameters {
                let string_param = self.evaluate_node(param, None, false);
                return_if!(string_param == err!(), (err!(), vec![]));
                type_parameters_vec.push(string_param.clone());
                
                let param_type_id = self.get_type_from_name(&param.token.location, &string_param);
                return_if!(param_type_id == None, (err!(), vec![]));

                let comma = if first_param { "".to_string() } else { "_c_".to_string() };
                parameters = format!("{parameters}{comma}{string_param}");
                first_param = false;
            }
            return (format!("_lt_##{parameters}##_gt"), type_parameters_vec);
        }
        ("".to_string(), vec![])
    }

    fn identifier_expression(&mut self, node: &IdentifierExpression, type_id: Option<TypeId>) -> String {
        let prefix = self.scope_type(&node.scope_type);
        let expression = self.evaluate_node(&node.expression, type_id, false);
        return_err_if!(expression == err!());

        let (postfix, type_parameters_vec) = self.get_type_parameters(&node.type_parameters);

        let result = format!("{prefix}{expression}{postfix}");
        let result_ident = self.table.get_identifer_type_by_name(&result);
        if result_ident.is_err() && node.type_parameters.is_some() {
            let header = format!("DEFINE_{}_{}({})", expression, type_parameters_vec.len(), type_parameters_vec.join(", "));
            add_macro_and_header!(self, self.defined_macros, header);
            
            let new_type = self.table.generate_type(result.clone(), 0, vec![]);
            self.table.add_type_scope(new_type);
        }

        result
    }

    fn function_call_parameters(&mut self, node: &NodeParameters, function: &FunctionHolder) -> String {
        let mut arguments = vec![];
        let mut last_param = None;
        let mut index = 0;
        for arg in &node.parameters {
            let param = function.parameters.get(index);
            let inside_params = param.is_some_and(|p: &ParameterHolder| p.is_params) || last_param.is_some_and(|p: &ParameterHolder| p.is_params);
            if inside_params {
                
                todo!();

                //continue;
            } else if param.is_none() {
                self.error(line!(), "Too many arguments", format!("Function `{}` expects {} parameters, but got {} arguments", function.name, function.parameters.len(), node.parameters.len()).as_str(), &arg.token.location);
                return err!();
            } else {
                last_param = param;
            }
            let param = param.unwrap();

            let arg_expression = self.evaluate_node(arg, Some(param.type_id), param.is_const);
            return_err_if!(arg_expression == err!());

            arguments.push(arg_expression);
            index += 1;
        }

        while let Some(p) = function.parameters.get(index) {
            if let Some(default_value) = p.default_value.clone() {
                arguments.push(default_value);
            } else {
                self.error(line!(), "Too few arguments", format!("Function `{}` expects {} parameters, but got {} arguments", function.name, function.parameters.len(), node.parameters.len()).as_str(), &node.token.location);
                return err!();
            }
            index += 1;
        } 

        arguments.join(", ")
    }

    fn generate(&mut self) {
        let ast = self.transpiler.ast.clone();
        let code = self.generate_from_nodes(&ast, None);
        self.outc = format!("//includes\n{}\n//headers\n{}\n//code\n{}", self.includes, self.headers, code);
        self.entry_point();
        self.includes.clear();
        self.headers.clear();
    }

    fn generate_from_nodes(&mut self, ast: &Vec<ASTNode>, return_type: Option<TypeId>) -> String {
        let mut out = "".to_string();
        for (index, node) in ast.iter().enumerate() {
            let tab = Self::indent(self.table.scope());
            let c_node;
            if index == ast.len() - 1 {
                c_node = self.evaluate_node(node, return_type, false);
            } else {
                c_node = self.evaluate_node(node, None, false);
            }

            if DEBUGGING {
                let scone_node = crate::parser::Parser::node_expr_to_string(node, self.table.scope() as usize);
                let scone_line = scone_node.split("\n").skip_while(|s| s.trim().starts_with("#!")).collect::<Vec<_>>().get(0).unwrap_or(&"").to_string();

                out = format!("{out}\n{tab}/* {scone_line} */\n{tab}{c_node}");
            } else {
                out = format!("{out}\n{tab}{c_node}");
            }
        }
        if out.is_empty() {
            return "".to_string();
        }
        out.remove(0); // removes the first newline character
        out
    }

    fn evaluate_constant_node(&mut self, node: &ASTNode, type_id: Option<TypeId>) -> String {
        self.error(line!(), "Parsing constant not implemented yet", "Parsing constant not implemented yet", &node.token.location);
        self.transpiler.output.print_messages();
        todo!();
    }

    fn evaluate_node(&mut self, node: &ASTNode, type_id: Option<TypeId>, constant: bool) -> String {
        if constant {
            return self.evaluate_constant_node(node, type_id);
        }
        match node.node.as_ref() {
            NodeType::Constant(ref v) => self.constant(v, type_id),
            NodeType::VariableDeclaration(ref v) => self.variable_declaration(v, type_id),
            NodeType::ScopedType(ref v) => self.scoped_type(v, type_id),
            NodeType::Identifier(ref v) => self.identifier(v, type_id),
            NodeType::FunctionDeclaration(ref v) => self.function_declaration(v, type_id),
            NodeType::ReturnExpression(ref v) => self.return_expression(v, type_id),
            NodeType::ScopedIdentifier(ref v) => self.scoped_identifier(v, type_id),
            NodeType::FunctionCall(ref v) => self.function_call(v, type_id),
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.transpiler.output.print_messages();
                todo!();
            }
        }
    }

    fn function_call(&mut self, node: &FunctionCall, type_id: Option<TypeId>) -> String { 
        let name = node.name.value.clone();
        return_err_if!(name.is_empty());

        let identifier = self.get_identifier_from_name(&node.name.location, &name);
        let function_holder = match identifier.clone() {
            Some(IdentifierType::Function(f)) => f,
            _ => {
                self.error(line!(), "Invalid function call", format!("Identifier `{name}` is not a function").as_str(), &node.name.location);
                return err!();
            }
        };

        let (type_parameters, type_parameters_vec) = self.get_type_parameters(&node.type_parameters);
        return_err_if!(type_parameters == err!());
        
        let idname = self.table.get_idname_from_identifier_type(&identifier.unwrap());

        // might be a generic function, so we need to check if the type_id matches
        if let Ok(function_type) = self.table.get_type_id(function_holder.type_id) {
            if let TypeType::Type(_) = function_type {
                check_unexpected_type!(self, &node.name.location, type_id, Some(function_holder.type_id));
            }
        } else {
            check_unexpected_type!(self, &node.name.location, type_id, Some(function_holder.type_id));
        }

        let parameters = self.function_call_parameters(&node.parameters, &function_holder);
        return_err_if!(parameters == err!());
        
        let type_name = self.get_type(&node.name.location, Some(function_holder.type_id));
        return_err_if!(type_name == err!());

        let result = format!("{idname}{type_parameters}");
        if !self.defined_macros.contains(&result) {
            let header = format!("DEFINE_{}_{}_{}({})", idname, type_parameters_vec.len(), node.parameters.parameters.len(), type_parameters_vec.join(", "));
            add_macro_and_header!(self, self.defined_macros, header);
            
            let new_type = self.table.generate_type(result.clone(), 0, vec![]);
            self.table.add_type_scope(new_type);
        }

        format!("{result}({parameters})")
    }

    fn scoped_identifier(&mut self, node: &ScopedIdentifier, type_id: Option<TypeId>) -> String {
        let mut result = "".to_string();
        for identifier in &node.scope {
            let identifier_expression = self.identifier_expression(&identifier, type_id);
            return_err_if!(identifier_expression == err!());
            result = format!("{result}{identifier_expression}");
        }
        result
    }

    fn return_expression(&mut self, node: &Option<Box<ASTNode>>, type_id: Option<TypeId>) -> String {
        if let Some(n) = node {
            let expression = self.evaluate_node(n, type_id, false);
            return_err_if!(expression == err!());
            format!("return {expression};")
        } else {
            "return;".to_string()
        }
    }

    fn code_block(&mut self, node: &CodeBlock, type_id: Option<TypeId>) -> String {
        let ast = node.body.clone().into_iter().map(|n| *n).collect();

        self.table.increase_scope(false);
        let result = self.generate_from_nodes(&ast, type_id);
        self.table.decrease_scope();

        let indent = Self::indent(self.table.scope());

        format!("\n{indent}{{\n{result}\n{indent}}}")
    }

    fn type_parameters_function_declaration(&mut self, node: &FunctionDeclaration, type_parameters: &Vec<AnonymousType>) -> String {
        let name = node.name.value.clone();
        return_err_if!(name.is_empty());

        self.table.increase_scope(true); // so type parameters can be added then removed later

        let type_types = self.type_parameters(type_parameters);
        return_err_if!(type_types.len() == 0);
        
        let type_parameter_holders = type_types.iter().map(|t| match t{
            TypeType::TypeParameter(ref p) => p.clone(),
            _ => unreachable!()
        }).collect::<Vec<_>>();

        let idnames = type_types.iter().map(|t| CodegenTable::get_idname_from_type_type(t)).collect::<Vec<_>>();
        return_err_if!(idnames.len() == 0);

        for tp in &type_types {
            self.table.add_type_scope(tp.clone());
        }
        
        let (parameters, parameter_holders) = self.defined_node_parameters(&node.parameters);
        return_err_if!(parameters == err!());

        self.table.add_parameters_to_scope(parameter_holders.clone());
        
        let type_name = self.evaluate_node(&node.return_type, None, false);
        return_err_if!(type_name == err!());

        let func_type_id = self.get_type_from_name(&node.return_type.token.location, &type_name);
        return_err_if!(func_type_id == None);

        let body;
        if let Some(b) = &node.body {
            body = format!(" {}", self.code_block(b, func_type_id));
        } else {
            body = ";".to_string();
        }
        return_err_if!(body == err!());

        let body_fixed = body.replace("\n", "\\\n");
        
        let macro_headers = self.table.get_macro_header().trim().to_string();
        self.table.decrease_scope();

        let func = self.table.generate_function(name, func_type_id.unwrap(), node.access_modifier.clone(), node.body.is_some(), parameter_holders, type_parameter_holders, node.tags.clone(), node.name.location.clone());
        let idname = self.table.get_idname_from_identifier_type(&func);
        self.table.add_identifier_scope(func);

        let type_parameter_len = type_types.len();
        let parameter_len = node.parameters.len();
        let type_parameters_string = type_parameters.iter().map(|tp| tp.name.value.clone()).collect::<Vec<_>>().join(", ");

        let result = format!("#define DEFINE_{idname}_{type_parameter_len}_{parameter_len}({type_parameters_string}) \\\n{macro_headers} \\\n{type_name} {idname}{parameters}{body_fixed}");
        self.add_header(result);
        
        "".to_string()
    }

    fn function_declaration(&mut self, node: &FunctionDeclaration, type_id: Option<TypeId>) -> String {
        check_unexpected_type!(self, &node.name.location, type_id, None::<TypeId>);

        if let Some(tp) = &node.type_parameters {
            return self.type_parameters_function_declaration(node, &tp.parameters);
        }
        
        let type_name = self.evaluate_node(&node.return_type, None, false);
        return_err_if!(type_name == err!());

        let func_type_id = self.get_type_from_name(&node.return_type.token.location, &type_name);
        return_err_if!(func_type_id == None);

        let name = node.name.value.clone();
        return_err_if!(name.is_empty());
        
        let (parameters, parameter_holders) = self.defined_node_parameters(&node.parameters);
        return_err_if!(parameters == err!());

        self.table.increase_scope(false); // so parameters can be added then removed later
        self.table.add_parameters_to_scope(parameter_holders.clone());

        let body; 
        if let Some(b) = &node.body {
            body = format!(" {}", self.code_block(b, func_type_id));
        } else {
            body = ";".to_string();
        }
        return_err_if!(body == err!());

        self.table.decrease_scope();
        
        let func = self.table.generate_function(name, func_type_id.unwrap(), node.access_modifier.clone(), node.body.is_some(), parameter_holders, vec![], node.tags.clone(), node.name.location.clone());
        let idname = self.table.get_idname_from_identifier_type(&func);
        self.table.add_identifier_scope(func);

        format!("{} {}{}{}", type_name, idname, parameters, body)
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

        self.table.get_idname_from_identifier_type(&ident)
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

        let (postfix, type_parameters_vec) = self.get_type_parameters(&node.type_parameters);

        let result = format!("{prefix_name}{postfix}");
        let result_type_id = self.get_type_from_name(&node.name.location, &result);
        if result_type_id.is_none() {
            let new_type = self.table.generate_type(result.clone(), 0, vec![]);
            self.table.add_type_scope(new_type);

            let type_parameters = type_parameters_vec.join(", ");
            let type_parameter_len = type_parameters_vec.len();
            
            let define_name = format!("DEFINE_{name}_{type_parameter_len}({type_parameters})");
            add_macro_and_header!(self, self.defined_macros, define_name);
        }

        result
    }

    fn scoped_type(&mut self, node: &ScopedType, type_id: Option<TypeId>) -> String {
        check_unexpected_type!(self, &node.token.location, type_id, None::<TypeId>);

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
                TypeModifier::Array => format!("vector_lt_##{}##_gt", type_name_with_postfixes)
            };
            
            if self.table.get_type_name(&type_name_with_new_postfix).is_err() {
                let header = match postfix {
                    TypeModifier::Ptr => {
                        let header = format!("typedef {type_name_with_postfixes}* {type_name_with_new_postfix};") ;
                        if !self.defined_typedefs.contains(&header) {
                            self.defined_typedefs.push(header.clone());
                            header
                        } else {
                            "".to_string()
                        }
                    },
                    TypeModifier::Array => {
                        let header = format!("DEFINE_vector_1({type_name_with_postfixes})");
                        if !self.defined_macros.contains(&header) {
                            self.defined_macros.push(header.clone());
                            header
                        } else {
                            "".to_string()
                        }
                    }
                };
                self.add_header(header);
                let new_type = self.table.generate_type(type_name_with_new_postfix.clone(), 0, vec![]);
                self.table.add_type_scope(new_type);
            }

            type_name_with_postfixes = type_name_with_new_postfix;
        }

        return type_name_with_postfixes;
    }
    
    fn variable_declaration(&mut self, node: &VariableDeclaration, type_id: Option<TypeId>) -> String {
        check_unexpected_type!(self, &node.var_name.location, type_id, None::<TypeId>);
        
        let type_name = self.evaluate_node(&node.var_type, None, false);
        return_err_if!(type_name == err!());

        let var_type_id = self.get_type_from_name(&node.var_type.token.location, &type_name);
        return_err_if!(var_type_id == None);

        let name = node.var_name.value.clone();
        return_err_if!(name.is_empty());

        let var = self.table.generate_variable(name, var_type_id.unwrap(), node.var_value.is_some(), false, node.access_modifier.clone(), node.tags.clone(), node.var_name.location.clone());
        let idname = self.table.get_idname_from_identifier_type(&var);
        self.table.add_identifier_scope(var);

        let first_part = format!("{} {}", type_name, idname);
        let second_part;

        if let Some(v) = &node.var_value {
            let value = self.evaluate_node(&v, var_type_id, false);
            return_err_if!(value == err!());

            second_part = format!("= {}", value);
        } else {
            second_part = "".to_string();
        }
                
        format!("{} {};", first_part, second_part)
    }

    fn constant(&mut self, node: &ConstantNode, type_id: Option<TypeId>) -> String { 
        let expected = self.get_type(&node.value.location, type_id);
        return_err_if!(expected == err!());

        let found = node.constant_type.to_string();
        if found != expected && self.table.get_type_parameter_from_type_id(type_id.unwrap()).is_none() {
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
}*/

/*


    fn get_type(&mut self, location: &Location, type_id: Option<TypeId>) -> Result<String, ()> {
        if let Some(type_id) = type_id {
            if let Ok(ty) = self.transpiler.table.get_type_id(type_id) {
                Ok(self.transpiler.table.get_type_enum_name(&ty))
            } else {
                self.error(line!(), "Type does not exist", format!("Type with the id `{}` does not exist", type_id).as_str(), location);
                return Err(());
            }
        } else {
            self.error(line!(), "No current type", "Expected to return a specific type here", location);
            return Err(());
        }
    }
    pub fn resolve_pass(&mut self) {
        for index in 0..self.transpiler.ast.len() {
            let mut node = self.transpiler.ast[index].clone();
            _ = self.check_node(&mut node, None, false);
        }
    }

    fn check_if_node_is_constant(&mut self, node: &ASTNode, type_id: Option<TypeId>) -> Result<(), ()> {
        todo!()
    }

    fn check_node(&mut self, node: &mut ASTNode, type_id: Option<TypeId>, constant: bool) -> Result<(), ()> {
        if constant {
            return self.check_if_node_is_constant(node, type_id);
        }
        match node.node.as_mut() {
            NodeType::Constant(v) => self.constant(v, type_id),
            NodeType::VariableDeclaration(v) => {
                if let Ok(_) = self.variable_declaration(v, type_id) {
                    return ok!();
                } else {
                    return Err(());
                }
            }
            NodeType::StructDeclaration(v) => {
                if let Ok(_) = self.struct_declaration(v, type_id) {
                    return ok!();
                } else {
                    return Err(());
                }
            }
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.output.print_messages();
                todo!();
            }
        }
    }

    fn evaluate_type(&mut self, node: &ASTNode) -> Result<TypeId, ()> {
        match node.node.as_ref() {
            NodeType::Constant(ref v) => self.constant_type(v),
            NodeType::ScopedType(ref v) => self.scoped_type(v),
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.output.print_messages();
                todo!();
            }
        }
    }

    fn scoped_type(&mut self, node: &ScopedType) -> Result<TypeId, ()> {
        if node.scope.is_empty() {
            self.error(line!(), "No scope in type", "Expected type to have a scope", &node.token.location);
            return Err(());
        }
        
        // check for scoping errors
        if node.scope.iter().skip(1).enumerate().any(|(_, x)| x.scope_type != Some(ScopeType::DoubleColon)) {
            // all scoping needs to be done with double colons
            self.error(line!(), "Invalid scoping", "Expected all type scoping to use `::`, for example `A::B::C", &node.token.location);
            return Err(());
        }
        let capture_table_scope = self.transpiler.table.scope.clone();

        // go through the scope, each time, removing the first element of the scope
        let mut scope = node.scope.clone();
        let mut root_type_id: TypeId = 0;
        while scope.len() > 0 {
            // get the root type
            let root_name = scope[0].name.value.clone();
            if let Ok(e) = self.transpiler.table.get_type_name(&root_name) {
                root_type_id = self.transpiler.table.get_type_enum_type_id(&e);
            } else {
                //debug!(self.transpiler.table);
                self.error(line!(), "Type does not exist", format!("Type `{}` does not exist", root_name).as_str(), &node.token.location);
                self.transpiler.table.scope = capture_table_scope;
                return Err(());
            }
            
            // get the id of the root type if it is a module, ie. struct or enum 
            let root_type_module_id = self.transpiler.table.get_module_identifier_enum_id_from_type_id(root_type_id).ok();
            
            if root_type_module_id.is_none() {
                if scope.len() > 1 {
                    // an error occurs because the root type is not a module, meaning it can't scope into anything because it doesn't know where to scope into
                    self.error(line!(), "Invalid scoping", "Expected the root type to be a module, eg. `struct`, `enum`", &node.token.location);
                    self.transpiler.table.scope = capture_table_scope;
                    return Err(());
                } else {
                    // if there is no more scoping to do, then the root type is the type
                    break;
                }
            } 
    
            // get the module
            let module_identifier_enum: IdentifierEnum;
            if let Some(module_id) = root_type_module_id {
                if let Ok(m) = self.transpiler.table.get_identifer_enum_by_id(module_id) {
                    module_identifier_enum = m;
                } else {
                    // could not find the module
                    self.error(line!(), "Could not find module", "Could not find type with this name", &scope[0].name.location);
                    self.transpiler.table.scope = capture_table_scope;
                    return Err(());
                }
            } else {
                // could not find the module
                self.error(line!(), "Could not find module", "Could not find type with this name", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return Err(());
            }
    
            // get module id
            let module_type_id: TypeId;
            match &module_identifier_enum {
                IdentifierEnum::Function(_) | IdentifierEnum::Variable(_) => {
                    self.error(line!(), "Invalid scoping", "Expected the root type to be a module, eg. `struct`, `enum`", &scope[0].name.location);
                    self.transpiler.table.scope = capture_table_scope;
                    return Err(());
                }
                IdentifierEnum::Struct(x) => module_type_id = x.type_id,
                IdentifierEnum::Enum(x) => module_type_id = x.type_id,
                IdentifierEnum::Trait(x) => module_type_id = x.type_id,
            }
    
            // make sure the type_id's match
            if module_type_id != root_type_id {
                self.error(line!(), "Invalid scoping", "The root type and module have a mismatching type id.", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return Err(());
            }
    
            // scope has a length of 1, so we are at the end of the scope
            if scope.len() == 1 {
                break;
            }
    
            // if module type is an enum, then we can not scope any farther
            if let IdentifierEnum::Enum(_) = module_identifier_enum {
                self.error(line!(), "Invalid scoping", "Can not scope into an enum as if were a type", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return Err(());
            }
    
            // if the module is a trait, then we can not scope any farther
            if let IdentifierEnum::Trait(_) = module_identifier_enum {
                self.error(line!(), "Invalid scoping", "Can not scope into a trait as if were a type", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return Err(());
            }
    
            // now go through the scope and find the ending type
            // first check if the next part in the scope exists under the current module
            let next_element_name = &scope[1].name.value;

            match module_identifier_enum {
                IdentifierEnum::Struct(strct) => {
                    if let Some(e) = strct.enums.iter().find(|x| &x.name == next_element_name) {
                        // set the transpiler scope to the current module
                        self.transpiler.table.scope = e.scope.clone();
                        // remove the root type
                        scope.remove(0);
                        // restart the loop
                        continue; 
                    } 
                    else if let Some(s) = strct.structs.iter().find(|x| &x.name == next_element_name) {
                        // set the transpiler scope to the current module
                        self.transpiler.table.scope = s.scope.clone();
                        // remove the root type
                        scope.remove(0);
                        // restart the loop
                        continue; 
                    } else if strct.members.iter().any(|x| &x.name == next_element_name) || strct.methods.iter().any(|x| &x.name == next_element_name) {
                        self.error(line!(), "Invalid scoping", "Expected element to be a module, eg. `struct`, `enum`", &scope[1].name.location);
                        self.transpiler.table.scope = capture_table_scope;
                        return Err(());
                    } else {
                        self.error(line!(), "Could not find type", "Could not find type with this name", &scope[1].name.location);
                        self.transpiler.table.scope = capture_table_scope;
                        return Err(());
                    }
                }
                _ => unreachable!()
            }
        }

        self.transpiler.table.scope = capture_table_scope;
        Ok(root_type_id)
    }

    fn variable_declaration(&mut self, variable_declaration: &mut VariableDeclaration, type_id: Option<TypeId>) -> Result<IdentifierEnum, ()> {
        check_unexpected_type!(self, &variable_declaration.name.location, type_id, None::<TypeId>);

        // evaluate and get type id
        let type_id = self.evaluate_type(&variable_declaration.var_type)?;

        // check if variable name is unique
        let name = variable_declaration.name.value.clone();
        if !self.transpiler.table.name_is_used_in_scope(&name) {
            self.error(line!(), "Variable name is not unique", format!("Variable name `{}` is not unique", name).as_str(), &variable_declaration.name.location);
            return Err(());
        }
        
        // evaluate value
        let value_id: Option<TypeId>;
        if let Some(value) = &variable_declaration.value {
            value_id = Some(self.evaluate_type(value)?);
        } else {
            value_id = None;
        }

        // check value type is compatible with type
        check_unexpected_type!(self, &variable_declaration.value.clone().unwrap().token.location, Some(type_id), value_id);

        // create variable
        let variable = self.transpiler.table.generate_variable(name, type_id, variable_declaration.value.is_some(), false, variable_declaration.access_modifier.clone(), variable_declaration.tags.clone(), variable_declaration.name.location.clone());

        // update AST variable id
        variable_declaration.symbol = self.transpiler.table.get_identifier_enum_id(&variable);

        // add variable to table
        self.transpiler.table.add_identifier_scope(variable.clone());

        Ok(variable)
    }

    fn constant(&mut self, constant: &ConstantNode, type_id: Option<TypeId>) -> Result<(), ()> {
        let expected = self.get_type(&constant.value.location, type_id)?;

        let found = constant.constant_type.to_string();
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
                self.error(line!(), "Type mismatch", format!("Expected type `{}` does not match constant type `{}`", expected, found).as_str(), &constant.value.location);
            } 
        }

        ok!()
    }

    fn constant_type(&mut self, constant: &ConstantNode) -> Result<TypeId, ()> {
        match self.transpiler.table.get_type_name(&constant.constant_type.to_string()) {
            Ok(TypeEnum::TypeParameter(_)) => unreachable!(),
            Ok(TypeEnum::Type(id)) => Ok(id.type_id),
            Err(_) => {
                self.error(line!(), "Could not find type", format!("Could not find constant type `{}`", constant.value.value).as_str(), &constant.value.location);
                Err(())
            }
        }
    }

    fn check_type_parameters(&mut self, type_parameters: &AnonymousTypeParameters) -> Result<Vec<TypeParameterHolder>, ()> {
        todo!()
    }

    fn evaluate_extensions(&mut self, extensions: &Vec<Box<Token>>) -> Result<Vec<TraitHolder>, ()> {
        let mut result: Vec<TraitHolder> = vec![];

        for extension in extensions {
            if let Ok(identifier_enum) = self.transpiler.table.get_identifer_enum_by_name(&extension.value) {
                match identifier_enum {
                    IdentifierEnum::Trait(trait_id) => {
                        result.push(trait_id.clone());
                    }
                    _ => {
                        self.error(line!(), "Could not find trait", format!("Could not find trait `{}`", extension.value).as_str(), &extension.location);
                        return Err(());
                    }
                }
            } else {
                self.error(line!(), "Could not find trait", format!("Could not find trait `{}`", extension.value).as_str(), &extension.location);
                return Err(());
            }
        }

        Ok(result)
    }

    fn evaluate_struct_body(&mut self, struct_body: &mut CodeBlock) -> Result<(Vec<FunctionHolder>, Vec<VariableHolder>, Vec<EnumHolder>, Vec<StructHolder>), ()> { 
        let mut functions: Vec<FunctionHolder> = vec![];
        let mut variables: Vec<VariableHolder> = vec![];
        let mut enums: Vec<EnumHolder> = vec![];
        let mut structs: Vec<StructHolder> = vec![];

        self.transpiler.table.increase_scope(false);

        for index in 0..struct_body.body.len() {
            let stmt = &mut struct_body.body[index];
            match stmt.node.as_mut() {
                NodeType::VariableDeclaration(v) => {
                    let variable = self.variable_declaration(v, None)?;
                    variables.push(self.transpiler.table.get_variable_from_identifier_enum(&variable).unwrap()); // can unwrap because variable is created in variable_declaration 100%
                }
                NodeType::StructDeclaration(v) => {
                    let struct_ = self.struct_declaration(v, None)?;
                    structs.push(self.transpiler.table.get_struct_from_identifier_enum(&struct_).unwrap()); // can unwrap because struct is created in struct_declaration 100%
                }
                NodeType::EnumDeclaration(v) => {
                    todo!()
                }
                NodeType::FunctionCall(v) => {
                    todo!()
                }
                _ => {
                    self.error(line!(), "Invalid statement in struct body", format!("Statement `{}` is not valid in struct body, expected `VariableDeclaration`, `StructDeclaration`, `EnumDeclaration` or `FunctionCall", stmt.node.to_string()).as_str(), &stmt.token.location);
                }
            }
        }

        self.transpiler.table.decrease_scope();

        Ok((functions, variables, enums, structs))
    }
    
    fn struct_declaration(&mut self, struct_declaration: &mut StructDeclaration, type_id: Option<TypeId>) -> Result<IdentifierEnum, ()> { 
        check_unexpected_type!(self, &struct_declaration.name.location, type_id, None::<TypeId>);
        
        // check if name is unique
        let name = struct_declaration.name.value.clone();
        if !self.transpiler.table.name_is_used_in_scope(&name) {
            self.error(line!(), "Struct name is not unique", format!("Struct name `{}` is not unique", name).as_str(), &struct_declaration.name.location);
            return Err(());
        }

        // evaluate type parameters
        let type_parameters: Vec<TypeParameterHolder>;
        if let Some(tp) = &struct_declaration.type_parameters {
            type_parameters = self.check_type_parameters(&tp)?;
        } else {
            type_parameters = vec![];
        }

        // evaluate extensions
        let extensions: Vec<TraitHolder>;
        if struct_declaration.extends.len() > 0 {
            extensions = self.evaluate_extensions(&struct_declaration.extends)?;
        } else {
            extensions = vec![];
        }

        // evaluate and check body
        let (functions, variables, enums, structs) = self.evaluate_struct_body(&mut struct_declaration.body)?;

        // add struct to table
        let struct_ = self.transpiler.table.generate_struct(name, functions, variables, structs, enums, struct_declaration.access_modifier.clone(), type_parameters, extensions, struct_declaration.tags.clone(), struct_declaration.name.location.clone());
        self.transpiler.table.add_module_identifier_to_type_scope(&struct_);
        self.transpiler.table.add_identifier_scope(struct_.clone());

        Ok(struct_)
    } */