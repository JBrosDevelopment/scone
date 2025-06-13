use crate::transpiler;
#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, error_handling::{ErrorHandling, DEBUGGING, Message}};
#[allow(unused_imports)]
use crate::debug;

pub fn codegen(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let mut transpiler = transpiler::Transpiler::new(ast, code, path, macros);
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
    
    functions: Vec<FunctionDeclaration>,
    structs: Vec<StructDeclaration>,
    traits: Vec<TraitDeclaration>,
    enums: Vec<EnumDeclaration>,
    typedefs: Vec<TypeDefDeclaration>,

    current_scope: Vec<Vec<VariableDeclaration>>,
    current_type: Vec<String>,
    current_type_unfixed: String,
    current_parsing_token: Vec<Box<Token>>,
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
            functions: vec![],
            structs: vec![],
            traits: vec![],
            enums: vec![],
            typedefs: vec![],
            current_scope: vec![],
            lines_before: vec![],
            lines_after: vec![],
            current_type: vec![],
            current_parsing_token: vec![],
            current_variable_count: 0,
            current_type_unfixed: "".to_string(),
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
            let expression = self.evaluate_node(&node, false);

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

    pub fn evaluate_node(&mut self, node: &ASTNode, constant: bool) -> String {
        match node.node.as_ref() {
            //NodeType::AnonymousType(ref v) => self.evaluate_anonymous_type(v),
            NodeType::ArrayExpression(ref v) => self.evaluate_array_expression(v),
            //NodeType::AsCast(ref v) => self.evaluate_as_cast(v),
            //NodeType::Assignment(ref v) => self.evaluate_assignment(v),
            //NodeType::Break(ref v) => self.evaluate_break(v),
            //NodeType::ClassDeclaration(ref v) => self.evaluate_class_declaration(v),
            //NodeType::CodeBlock(ref v) => self.evaluate_code_block(v),
            NodeType::Constant(ref v) => self.evaluate_constant(v),
            //NodeType::Continue(ref v) => self.evaluate_continue(v),
            //NodeType::Discard(ref v) => self.evaluate_discard(v),
            //NodeType::EnumDeclaration(ref v) => self.evaluate_enum_declaration(v),
            //NodeType::EnumDeclaration(ref v) => self.evaluate_enum_declaration(v),
            //NodeType::For(ref v) => self.evaluate_for(v),
            //NodeType::ForEach(ref v) => self.evaluate_for_each(v),
            NodeType::FunctionCall(ref v) => self.evaluate_function_call(v),
            NodeType::FunctionDeclaration(ref v) => self.evaluate_function_declaration(v),
            NodeType::Identifier(ref v) => self.evaluate_identifier(v),
            //NodeType::Indexer(ref v) => self.evaluate_indexer(v),
            //NodeType::If(ref v) => self.evaluate_if(v),
            //NodeType::IsCheck(ref v) => self.evaluate_is_check(v),
            //NodeType::LambdaExpression(ref v) => self.evaluate_lambda_expression(v),
            //NodeType::Match(ref v) => self.evaluate_match(v),
            //NodeType::ObjectInstantiation(ref v) => self.evaluate_object_instantiation(v),
            NodeType::Operator(ref v) => self.evaluate_operator(v),
            NodeType::ReturnExpression(ref v) => self.evaluate_return_expression(v),
            NodeType::ScopedExpression(ref v) => self.evaluate_scoped_expression(v),
            //NodeType::Shebang(ref v) => self.evaluate_shebang(v),
            //NodeType::StructDeclaration(ref v) => self.evaluate_struct_declaration(v),
            NodeType::TernaryOperator(ref v) => self.evaluate_ternary_operator(v),
            //NodeType::TraitDeclaration(ref v) => self.evaluate_trait_declaration(v),
            //NodeType::TupleDeclaration(ref v) => self.evaluate_tuple_declaration(v),
            //NodeType::TupleExpression(ref v) => self.evaluate_tuple_expression(v),
            //NodeType::TypeDef(ref v) => self.evaluate_type_def(v),
            //NodeType::TypeDefinition(ref v) => self.evaluate_type_definition(v),
            NodeType::TypeIdentifier(ref v) => self.evaluate_type_identifier(v),
            //NodeType::UnaryOperator(ref v) => self.evaluate_unary_operator(v),
            //NodeType::Use(ref v) => self.evaluate_use(v),
            NodeType::VariableDeclaration(ref v) => self.evaluate_variable_declaration(v),
            //NodeType::While(ref v) => self.evaluate_while(v),
            _ => {
                self.error(line!(), "Unknown node type", format!("Node type: {} not implemented yet", node.node.to_string()).as_str(), &node.token.location);
                "".to_string()
            }
        }
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

    fn evaluate_variable_declaration(&mut self, node: &VariableDeclaration) -> String {
        if self.current_scope.last().is_none() {
            self.current_scope.push(vec![]);
        }
        self.current_scope.last_mut().unwrap().push(node.clone());
        self.current_parsing_token.push(node.var_name.clone());

        // tags don't get transpiled
        // access modifiers:
        let modifiers = self.parse_access_modifiers(&node.access_modifier);
        // type:
        let type_ = self.evaluate_node(&node.var_type, false);
        // name:
        let name = node.var_name.value.clone();
        
        if let Some(value) = &node.var_value {
            // value:
            self.current_type.push(type_.clone());
            let value = self.evaluate_node(value, false);
            self.current_type.pop();
            
            self.current_parsing_token.pop();
            format!("{modifiers}{type_} {name} = {value};")
        } else {
            self.current_parsing_token.pop();
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

    fn parse_node_parameters(&mut self, node: &NodeParameters) -> String {
        let mut s = "".to_string();
        for (index, param) in node.parameters.iter().enumerate() {
            self.current_parsing_token.push(node.token.clone());
            s += self.evaluate_node(param, false).as_str();
            self.current_parsing_token.pop();
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
            ScopeType::Dot => "___dot___".to_string() // shouldn't get here, but just in case
        }
    }

    fn parse_type_identifier(&mut self, node: &TypeIdentifier) -> String {
        // scope type (`.` or `::`):
        let scope_type = self.parse_typed_scope_type(&node.scope_type);let a: i8 = 0;
        // name:
        let name = match node.name.value.as_str() {
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
            _ => node.name.value.clone()
        };
        self.current_type_unfixed = name.clone();

        if let Some(np) = &node.type_parameters {
            // generics
            let generics = self.parse_node_parameters(&np);
            
            format!("{scope_type}{name}_{generics}_c_")
        } else {
            format!("{scope_type}{name}")
        }
    }

    fn parse_type_scope(&mut self, node: &Vec<TypeIdentifier>) -> String {
        let mut scope = "".to_string();
        for n in node {
            scope += self.parse_type_identifier(n).as_str();
        }

        scope
    }

    fn evaluate_type_identifier(&mut self, node: &ScopedType) -> String {
        // scope:
        let scope = self.parse_type_scope(&node.scope);
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
            self.header += format!("typedef {result} {type_result};\n").as_str();
        } else if node.is_ptr_or_ref.iter().filter(|x| x == &&TypeModifier::Array).collect::<Vec<_>>().len() > 0 {
            self.header += format!("VECTOR_DEFINE({type_result_without_array});\n").as_str();
        }
        type_result
    }

    fn evaluate_constant(&mut self, node: &ConstantNode) -> String { 
        match node.constant_type.clone() {
            ConstantType::String => format!("\"{}\"", node.value.value),
            ConstantType::Char => format!("'{}'", node.value.value),
            _ => node.value.value.clone(),
        }
    }

    fn get_current_type(&mut self) -> String {
        if self.current_type.last().is_none() {
            let token = self.get_current_token();
            self.error(line!(), "No type found for expression", "Couldn't find a type for the expression.", &token.location);
            return "".to_string();
        }
        self.current_type.last().unwrap().clone()
    }

    fn get_current_token(&mut self) -> Box<Token> {
        if self.current_parsing_token.last().is_none() {
            self.error(line!(), "No token found for expression", "Couldn't find a token for the expression.", &Token::new_empty().location);
            return Box::new(Token::new_empty());
        }
        self.current_parsing_token.last().unwrap().clone()
    }

    fn get_temp_variable_name(&mut self) -> String {
        self.current_variable_count += 1; 
        format!("temp_{}", self.current_variable_count) 
    }

    fn evaluate_array_expression(&mut self, node: &NodeParameters) -> String {
        let type_ = self.current_type_unfixed.clone();
        let name = self.get_temp_variable_name();
        let array_innerds = self.parse_node_parameters(&node);
        let c_array = format!("{{ {array_innerds} }}");

        let line_before = format!("{type_} {name}[] = {c_array};");
        self.lines_before.push(line_before);

        format!("Vec_{type_}_new({name})")
    }

    fn parse_identifier(&mut self, node: &Identifier) -> String {
        // scope type (`.` or `::`):
        let scope_type = self.parse_typed_scope_type(&node.scope_type);
        // expression:
        let expression = self.evaluate_node(&node.expression, false);

        if let Some(np) = &node.type_parameters {
            // generics
            let generics = self.parse_node_parameters(&np);
            
            format!("{scope_type}{expression}_{generics}_c_")
        } else {
            format!("{scope_type}{expression}")
        }
    }

    fn evaluate_scoped_expression(&mut self, node: &ScopedIdentifier) -> String {
        let mut scope = "".to_string();
        for n in node.scope.iter() {
            scope += self.parse_identifier(n).as_str();
        }        
        scope
    }

    fn parse_defined_node_parameters(&mut self, node: &Vec<DefinedNodeParameter>) -> String {
        let mut result = "".to_string();

        for (i, param) in node.iter().enumerate() {
            let type_ = self.evaluate_node(&param.ty, false);
            let name = param.name.value.clone();

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
            let expression = self.evaluate_node(&param, false);
            result.push_str(&expression);
            if i < node.len() - 1 {
                result.push_str("_");
            }
        }

        result
    }

    fn parse_undefined_function_declaration(&mut self, node: &FunctionDeclaration) -> String {
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
        let func_decl = self.evaluate_function_declaration(&func_dec_no_body);
        let func_decl_without_semicolon = func_decl.chars().take(func_decl.len() - 1).collect::<String>();
        result.push_str(&func_decl_without_semicolon);
        
        if let Some(body) = node.body.clone() {
            result.push(' ');
            result.push('\\');
            result.push('\n');

            let body = self.parse_undefined_body_region(&body);
            result.push_str(&body);
        } 

        result.push('\n');

        result
    }

    fn parse_undefined_body_region(&mut self, node: &BodyRegion) -> String {
        let body = self.evaluate_code_block(&node, &"".to_string());
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

    fn evaluate_code_block(&mut self, node: &BodyRegion, insert: &String) -> String {
        let mut result = "{\n".to_string();

        self.level += 1;
        for n in node.body.clone() {
            let expression = self.evaluate_node(&n, false);
            result.push_str(&self.level_string());
            result.push_str(&expression);
            result.push(';');
            result.push('\n');
        }
        
        if !insert.is_empty() {
            result.push_str(&self.level_string());
            result.push_str(&insert);
            result.push('\n');
        }

        self.level -= 1;

        result.push_str(&self.level_string());
        result.push('}');
        result.push('\n');

        result
    }

    fn evaluate_function_declaration(&mut self, node: &FunctionDeclaration) -> String {
        // access modifiers:
        let modifiers = self.parse_access_modifiers(&node.access_modifier);

        if !modifiers.is_empty() && node.type_parameters.is_some() {
            self.error(line!(), "Unexpected access modifier", "External function declarations can not have access modifier.", &node.name.location);
            return "".to_string();
        }

        else if modifiers.contains("extern") && matches!(node.name.value.as_str(), "printf" | "scanf" | "vprintf" | "vscanf" | "sprintf" | "sscanf") {
            if !self.includes.contains("#include <stdio.h>") {
                self.includes += "#include <stdio.h>\n";
            }
            return "".to_string();
        }

        let return_type = self.evaluate_node(&node.return_type, false);
        let name = node.name.value.clone();
        let parameters = self.parse_defined_node_parameters(&node.parameters);

        if node.type_parameters.is_some() {
            let define_shabang = self.parse_undefined_function_declaration(node);
            self.header += define_shabang.as_str();
            self.header += "\n";
            "".to_string()
        }
        else {
            if let Some(body) = node.body.clone() {
                let insert = if name == "main" && return_type == "void" {
                    "return 0;".to_string()
                } else {
                    "".to_string()
                };
                let body = self.evaluate_code_block(&body, &insert);
                format!("{modifiers}{return_type} {name}({parameters})\n{body}")
            } else {
                format!("{modifiers}{return_type} {name}({parameters});")
            }
        }
    }

    fn evaluate_return_expression(&mut self, node: &Box<ASTNode>) -> String {
        let expression = self.evaluate_node(node, false);
        format!("return {expression};")
    }

    fn evaluate_identifier(&mut self, node: &Box<Token>) -> String {
        node.value.clone()
    }

    fn evaluate_operator(&mut self, node: &Expression) -> String {
        let l_expression = self.evaluate_node(&node.left, false);
        let r_expression = self.evaluate_node(&node.right, false);
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

    fn evaluate_function_call(&mut self, node: &FunctionCall) -> String {
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
                let parameter = self.evaluate_node(&type_parameter, false);
                header_name.push_str(&parameter);
                if i < type_parameters.len() - 1 {
                    header_name.push(',');
                    header_name.push(' ');
                }
            }
            header_name.push(')');

            self.header += format!("{header_name};\n").as_str();   
        } else {
            call_name = node.name.value.clone();
        }

        let parameters = self.parse_node_parameters(&node.parameters);

        format!("{call_name}({parameters})")
    }

    fn evaluate_ternary_operator(&mut self, node: &TernaryConditional) -> String {
        let condition = self.evaluate_node(&node.condition, false);

        let then_expression = if node.then.token.token_type == TokenType::Underscore {
            "0".to_string()
        } else {
            self.evaluate_node(&node.then, false)
        };
        let otherwise_expression = if node.else_then.token.token_type == TokenType::Underscore {
            "0".to_string()
        } else {
            self.evaluate_node(&node.else_then, false)
        };

        format!("{condition} ? {then_expression} : {otherwise_expression}")
    }
}