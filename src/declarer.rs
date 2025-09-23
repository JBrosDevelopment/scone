// The purpose of this step is to go over the AST and grab the symbols of each type, and then add them to the symbol table

use std::default;

#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};

pub fn declarer_pass_on_ast(transpiler: &mut Transpiler, error_handling: &mut ErrorHandling) {
    let mut declarer = Declarer::new(transpiler, error_handling);
    declarer.declare_pass();

    error_handling.print_messages();
}

#[derive(Debug)]
pub struct Declarer<'a> {
    pub transpiler: &'a mut Transpiler,
    pub output: &'a mut ErrorHandling,
    scope: Scope,
    last_id: u32,
}

impl<'a> Declarer<'a> {
    pub fn new(transpiler: &'a mut Transpiler, error_handling: &'a mut ErrorHandling) -> Declarer<'a> {
        Declarer { transpiler, output: error_handling, scope: Scope::new(), last_id: 0 }
    }

    #[allow(dead_code)]
    fn error(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_error("declarer", line, file!(), title, message, location);
    }

    #[allow(dead_code)]
    fn warning(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_warning("declarer", line, file!(), title, message, location);
    }

    #[allow(dead_code)]
    fn message(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_message("declarer", line, file!(), title, message, location);
    }

    fn next_id(&mut self) -> u32 {
        self.last_id += 1;
        self.last_id
    }

    fn get_symbol_by_name(&self, name: &String) -> Option<Symbol> {
        self.transpiler.symbols.iter().find(|symbol| &symbol.name == name).cloned()
    }

    fn get_symbol_id(&mut self, name: &String) -> u32 {
        self.get_symbol_by_name(name).filter(|c| c.scope.in_scope(&self.scope)).map(|c| c.id).unwrap_or_else(|| self.next_id())
    }

    fn get_symbol_type(&mut self, name: &String, default: ObjectTypes) -> ObjectTypes {
        self.get_symbol_by_name(name).filter(|c| c.scope.in_scope(&self.scope)).map(|c| c.object_type).unwrap_or_else(|| default)
    }

    fn symbol_is_unique(&mut self, symbol: &Symbol) -> bool {
        self.transpiler.symbols.iter().find(|x| x.id == symbol.id).is_none()
    }

    fn push_symbol(&mut self, symbol: Symbol) {
        self.transpiler.symbols.push(symbol);
    }

    pub fn declare_pass(&mut self) {
        let mut ast = std::mem::take(&mut self.transpiler.ast);
    
        for node in ast.iter_mut() {
            self.pass_node(node);
        }
    
        self.transpiler.ast = ast;
    }

    pub fn pass_vector_nodes(&mut self, nodes: &mut Vec<Box<ASTNode>>) {
        let mut ast = std::mem::take(nodes);
    
        for node in ast.iter_mut() {
            self.pass_node(node);
        }
    
        *nodes = ast;
    }

    fn pass_node(&mut self, node: &mut ASTNode) {
        match node.node.as_mut() {
            NodeType::ArrayExpression(v) => self.pass_vector_nodes(&mut v.parameters),
            NodeType::AsCast(v) => {
                self.pass_node(&mut v.left);
                self.pass_node(&mut v.right);
                return;
            }            
            NodeType::Break(_) => return,
            NodeType::ClassDeclaration(_) => {
                self.error(line!(), "Class not supported", "Classes are not supported, use a `struct` instead", &node.token.location);
                return;
            }
            NodeType::CodeBlock(v) => {
                self.scope.increase();
                self.pass_vector_nodes(&mut v.body);
                self.scope.decrease();
            }
            NodeType::Constant(_) => return,
            NodeType::Continue(_) => return,
            NodeType::DeferStatement(v) => self.pass_node(v),
            NodeType::Discard(_) => return,
            NodeType::EnumDeclaration(v) => self.pass_enum(v),

            NodeType::VariableDeclaration(v) => self.pass_variable(v),
            NodeType::ScopedType(v) => self.pass_scoped_type(v),
            NodeType::Identifier(v) => self.pass_identifier(v),
            NodeType::StructDeclaration(v) => self.pass_struct(v),
            NodeType::FunctionDeclaration(v) => self.pass_function_declaration(v),
            NodeType::ScopedIdentifier(v) => self.pass_scoped_identifier(v),
            
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.output.print_messages();
                unimplemented!();
            }
        }
    }

    fn pass_enum(&mut self, enum_declaration: &mut EnumDeclaration) {
        let name = &enum_declaration.name.value;
        let object_type = ObjectTypes::Enum;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &enum_declaration.name.location);
        }

        enum_declaration.symbol_id = id;

        self.scope.increase();

        for variant in enum_declaration.body.iter_mut() {
            self.pass_enum_variant(variant);
        }

        self.scope.decrease();
    }

    fn pass_enum_variant(&mut self, enum_variant: &mut EnumVariant) {
        let name = &enum_variant.name.value;
        let object_type = ObjectTypes::EnumVariant;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &enum_variant.name.location);
        }

        enum_variant.id = id;
    }

    fn pass_variable(&mut self, variable_declaration: &mut VariableDeclaration) {
        let name = &variable_declaration.name.value;
        let object_type = ObjectTypes::Variable;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &variable_declaration.name.location);
        }

        self.pass_node(&mut variable_declaration.var_type);

        if let Some(value) = &mut variable_declaration.value {
            self.pass_node(value);
        }
    }

    fn pass_scoped_type(&mut self, scoped_type: &mut ScopedType) {
        for node in scoped_type.scope.iter_mut() {
            let name = &node.name.value;
            let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
            let id = self.get_symbol_id(name);

            node.symbol_id = id;
            
            let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
            if self.symbol_is_unique(&symbol) {
                self.push_symbol(symbol);
            }
            
            if let Some(type_parameters) = &mut node.type_parameters {
                self.pass_vector_nodes(&mut type_parameters.parameters);
            }
        }
    }

    fn pass_identifier(&mut self, identifier: &mut Identifier) {
        let name = &identifier.token.value;
        let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        }
    }

    fn pass_token(&mut self, token: &Box<Token>) {
        let name = &token.value;
        let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        }
    }

    fn pass_anonymous_types(&mut self, type_parameters: &mut Vec<AnonymousType>) {
        for anonymous_type in type_parameters.iter_mut() {
            self.pass_token(&anonymous_type.name);
            anonymous_type.symbol_id = self.get_symbol_id(&anonymous_type.name.value);

            if let Some(constraints) = &mut anonymous_type.constraints {
                self.pass_node(constraints);
            }
        }
    }

    fn pass_struct(&mut self, struct_declaration: &mut StructDeclaration) {
        let name = &struct_declaration.name.value;
        let object_type = ObjectTypes::Struct;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &struct_declaration.name.location);
        }

        struct_declaration.symbol_id = id;

        for extension in struct_declaration.extends.iter() {
            self.pass_token(extension);
        }

        if let Some(type_parameters) = &mut struct_declaration.type_parameters {
            self.pass_anonymous_types(&mut type_parameters.parameters);
        }

        self.scope.increase();

        for node in struct_declaration.body.body.iter_mut() {
            self.pass_node(node);
        }

        self.scope.decrease();
    }

    fn pass_function_declaration(&mut self, function_declaration: &mut FunctionDeclaration) {
        let name = &function_declaration.name.value;
        let object_type = ObjectTypes::Function;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &function_declaration.name.location);
        }

        function_declaration.symbol_id = id;

        self.pass_node(&mut function_declaration.return_type);

        if let Some(type_parameters) = &mut function_declaration.type_parameters {
            self.pass_anonymous_types(&mut type_parameters.parameters);
        }

        self.scope.increase();

        for parameter in function_declaration.parameters.iter_mut() {
            self.pass_token(&parameter.name);
            self.pass_node(&mut parameter.ty);
            if let Some(default_value) = &mut parameter.default_value {
                self.pass_node(default_value);
            }
            parameter.symbol_id = self.get_symbol_id(&parameter.name.value);
        }

        if let Some(body) = &mut function_declaration.body {
            for node in body.body.iter_mut() {
                self.pass_node(node);
            }
        }

        self.scope.decrease();
    }

    fn pass_scoped_identifier(&mut self, scoped_identifier: &mut ScopedIdentifier) {
        for node in scoped_identifier.scope.iter_mut() {
            self.pass_node(&mut node.expression);
            
            if let Some(type_parameters) = &mut node.type_parameters {
                self.pass_vector_nodes(&mut type_parameters.parameters);
            }
        }
    }
}