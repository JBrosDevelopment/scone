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

        let _char_ptr_ptr = self.table.generate_type("char_ptr_ptr".to_string(), 0, vec![]);
        self.table.add_type_scope(_char_ptr_ptr); // type_id = 14
        self.add_header("typedef char** char_ptr_ptr;".to_string());
        
        self.add_header("// Default types".to_string());

        let _entry_argc = self.table.generate_variable("ENTRY_ARGC".to_string(), 5, true, false, vec![AccessModifier::Const], vec![], Location { line: 0, column: 0, length: 0 });
        self.table.add_identifier_scope(_entry_argc); // id = 1
        self.add_header("const int v1; // ENTRY_ARGC\n".to_string());
        
        let _entry_argv = self.table.generate_variable("ENTRY_ARGV".to_string(), 14, true, false, vec![AccessModifier::Const], vec![], Location { line: 0, column: 0, length: 0 });
        self.table.add_identifier_scope(_entry_argv); // id = 2
        self.add_header("const char** v2; // ENTRY_ARGV".to_string());
    }

    fn add_header(&mut self, header: String) {
        self.outc = format!("{}\n{}", header, self.outc);
    }
    fn add_include(&mut self, header: String) {
        self.includes = format!("#include {}\n{}", header, self.includes);
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
                function_name = CodegenTable::get_idname_from_identifier_type(&IdentifierType::Function(function));
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

        let new_type_name = format!("{}_lt_{}_gt", wrapper_type.name, type_to_wrap_type_name);
        
        let new_type;
        if let Ok(t) = self.table.get_type_name(&new_type_name) {
            new_type = t
        } else {
            let header = format!("DEFINE_{}({})", wrapper_type.name, type_to_wrap_type_name);
            self.add_header(header);
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

            let type_name_unchecked = self.evaluate_node(&p.ty, None, false);
            return_if!(type_name_unchecked == err!(), (err!(), vec![]));
            
            let unchecked_type_id = self.get_type_from_name(&p.ty.token.location, &type_name_unchecked);
            return_if!(unchecked_type_id == None, (err!(), vec![]));

            let type_id;
            let type_name;
            if p.params {
                // add vector wrapper to type: `i32: values ...` -> `vector<i32>: values`
                let vector_type_id = self.get_type_from_name(&p.name.location, &"vector".to_string());
                return_if!(vector_type_id == None, (err!(), vec![]));

                let new_type = self.wrap_type(&p.name.location, vector_type_id.unwrap(), unchecked_type_id.unwrap());
                return_if!(new_type == None, (err!(), vec![]));
                let new_type = new_type.unwrap();

                type_id = Some(new_type.type_id);
                type_name = new_type.name;
            } else {
                type_id = unchecked_type_id;
                type_name = type_name_unchecked;
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

            let param_holder = self.table.generate_parameter(p.name.value.clone(), type_id.unwrap(), p.is_const, p.params, default_value.clone());
            let idname = CodegenTable::get_idname_from_parameter_holder(&param_holder);
            parameter_holders.push(param_holder);

            let result;
            if let Some(p) = &default_value {
                result = format!("{type_name} {idname} = {p}");
            } else {
                result = format!("{type_name} {idname}");
            }
            params.push(result);
        }

        let result = params.join(", ");
        (format!("({result})"), parameter_holders)
    }

    fn type_parameters(&mut self, type_parameters: &Vec<AnonymousType>) -> Vec<TypeType> {
        let mut result = vec![];
        for tp in type_parameters {
            let holder = self.table.generate_type_parameter(tp.name.value.clone());
            result.push(holder);
        }
        result
    }

    fn generate(&mut self) {
        let ast = self.transpiler.ast.clone();
        let code = self.generate_from_nodes(&ast, None);
        self.outc = format!("//includes\n{}\n//headers\n{}\n//code\n{}", self.includes, self.outc, code);
        self.entry_point();
        self.includes.clear();
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
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.transpiler.output.print_messages();
                todo!();
            }
        }
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

        self.table.increase_scope();
        let result = self.generate_from_nodes(&ast, type_id);
        self.table.decrease_scope();

        format!("{{\n{}\n{}}}", result, Self::indent(self.table.scope()))
    }

    fn type_parameters_function_declaration(&mut self, node: &FunctionDeclaration, type_parameters: &Vec<AnonymousType>) -> String {
        let name = node.name.value.clone();
        return_err_if!(name.is_empty());

        self.table.increase_scope(); // so type parameters can be added then removed later

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

        self.table.decrease_scope();

        let func = self.table.generate_function(name, func_type_id.unwrap(), node.access_modifier.clone(), node.body.is_some(), parameter_holders, type_parameter_holders, node.tags.clone(), node.name.location.clone());
        let idname = CodegenTable::get_idname_from_identifier_type(&func);
        self.table.add_identifier_scope(func);

        let type_parameter_len = type_types.len();
        let parameter_len = parameters.len();
        let type_parameters_string = type_parameters.iter().map(|tp| tp.name.value.clone()).collect::<Vec<_>>().join(", ");

        let result = format!("#define DEFINE_{idname}_{type_parameter_len}_{parameter_len}({type_parameters_string}) {type_name} {idname}{parameters}{body_fixed}");
        
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

        let body; 
        if let Some(b) = &node.body {
            body = format!(" {}", self.code_block(b, func_type_id));
        } else {
            body = ";".to_string();
        }
        return_err_if!(body == err!());
        
        let func = self.table.generate_function(name, func_type_id.unwrap(), node.access_modifier.clone(), node.body.is_some(), parameter_holders, vec![], node.tags.clone(), node.name.location.clone());
        let idname = CodegenTable::get_idname_from_identifier_type(&func);
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
                let string_param = self.evaluate_node(param, None, false);
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
    
    fn variable_declaration(&mut self, node: &VariableDeclaration, type_id: Option<TypeId>) -> String {
        check_unexpected_type!(self, &node.var_name.location, type_id, None::<TypeId>);
        
        let type_name = self.evaluate_node(&node.var_type, None, false);
        return_err_if!(type_name == err!());

        let var_type_id = self.get_type_from_name(&node.var_type.token.location, &type_name);
        return_err_if!(var_type_id == None);

        let name = node.var_name.value.clone();
        return_err_if!(name.is_empty());

        let var = self.table.generate_variable(name, var_type_id.unwrap(), node.var_value.is_some(), false, node.access_modifier.clone(), node.tags.clone(), node.var_name.location.clone());
        let idname = CodegenTable::get_idname_from_identifier_type(&var);
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
}