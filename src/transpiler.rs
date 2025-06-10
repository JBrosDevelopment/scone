use serde::de::value;

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
    header: String,
    main: String,
    
    functions: Vec<FunctionDeclaration>,
    structs: Vec<StructDeclaration>,
    traits: Vec<TraitDeclaration>,
    enums: Vec<EnumDeclaration>,
    typedefs: Vec<TypeDefDeclaration>,

    current_scope: Vec<Vec<VariableDeclaration>>,
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
            println!("{}", expression);

            self.main.push_str(&expression);
            self.main.push('\n');
        }

        format!("//header:\n\n{}\n\n//main:\n\n{}\n", self.header, self.main)
    }

    pub fn evaluate_node(&mut self, node: &ASTNode, constant: bool) -> String {
        match node.node.as_ref() {
            //NodeType::AnonymousType(ref v) => self.evaluate_anonymous_type(v),
            //NodeType::ArrayExpression(ref v) => self.evaluate_array_expression(v),
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
            //NodeType::FunctionCall(ref v) => self.evaluate_function_call(v),
            //NodeType::FunctionDeclaration(ref v) => self.evaluate_function_declaration(v),
            //NodeType::Identifier(ref v) => self.evaluate_identifier(v),
            //NodeType::Indexer(ref v) => self.evaluate_indexer(v),
            //NodeType::If(ref v) => self.evaluate_if(v),
            //NodeType::IsCheck(ref v) => self.evaluate_is_check(v),
            //NodeType::LambdaExpression(ref v) => self.evaluate_lambda_expression(v),
            //NodeType::Match(ref v) => self.evaluate_match(v),
            //NodeType::ObjectInstantiation(ref v) => self.evaluate_object_instantiation(v),
            //NodeType::Operator(ref v) => self.evaluate_operator(v),
            //NodeType::ReturnExpression(ref v) => self.evaluate_return_expression(v),
            //NodeType::ScopedExpression(ref v) => self.evaluate_scoped_expression(v),
            //NodeType::Shebang(ref v) => self.evaluate_shebang(v),
            //NodeType::StructDeclaration(ref v) => self.evaluate_struct_declaration(v),
            //NodeType::TernaryOperator(ref v) => self.evaluate_ternary_operator(v),
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
        let mut s = "".to_string();
        for _ in 0..self.level {
            s.push_str("    ");
        }
        s
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

        // tags don't get transpiled
        // access modifiers:
        let modifiers = self.parse_access_modifiers(&node.access_modifier);
        // type:
        let type_ = self.evaluate_node(&node.var_type, false);
        // name:
        let name = node.var_name.value.clone();
        
        if let Some(value) = &node.var_value {
            // value:
            let value = self.evaluate_node(value, false);
            
            format!("{modifiers}{type_} {name} = {value};")
        } else {
            format!("{modifiers}{type_} {name};")
        }
    }

    fn parse_type_reference(&self, node: &Vec<TypeMemoryModifier>) -> String {
        node.iter().filter(|x| x == &&TypeMemoryModifier::Ref).map(|_| "ref_").collect::<String>()
    }

    fn parse_type_pointer(&self, node: &Vec<TypeMemoryModifier>) -> String {
        node.iter().filter(|x| x == &&TypeMemoryModifier::Ptr).map(|_| "_ptr").collect::<String>()
    }

    fn parse_reference_or_pointer(&self, node: &Vec<TypeMemoryModifier>) -> String {
        node.iter().map(|_| "*").collect::<String>()
    }

    fn parse_node_parameters(&mut self, node: &NodeParameters) -> String {
        let mut s = "".to_string();
        for (index, param) in node.parameters.iter().enumerate() {
            s += self.evaluate_node(param, false).as_str();
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
        let scope_type = self.parse_typed_scope_type(&node.scope_type);
        // name:
        let name = node.name.value.clone();

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

        let type_result = format!("{type_refs}{scope}{type_pointers}");
        let result = format!("{scope}{ref_or_ptr}");

        if node.is_ptr_or_ref.len() > 0 {
            self.header += format!("typedef {result} {type_result};\n").as_str();
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
}