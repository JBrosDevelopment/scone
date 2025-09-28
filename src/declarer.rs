// The purpose of this step is to go over the AST and grab the symbols of each type, and then add them to the symbol table
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
            NodeType::Assignment(v) => {
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
            NodeType::For(v) => {
                self.pass_node(&mut v.set_segment);
                self.pass_node(&mut v.condition_segment);
                if let Some(index_segment) = &mut v.index_segment {
                    self.pass_identifier(index_segment);
                }
                self.scope.increase();
                self.pass_vector_nodes(&mut v.body.body);
                self.scope.decrease();
            }
            NodeType::ForEach(v) => {
                self.pass_node(&mut v.iter_value);
                self.pass_node(&mut v.iter_range);
                self.scope.increase();
                if let Some(index_segment) = &mut v.index_segment {
                    self.pass_identifier(index_segment);
                }
                self.pass_vector_nodes(&mut v.body.body);
                self.scope.decrease();
            }
            NodeType::FunctionCall(v) => self.pass_function_call(v),
            NodeType::FunctionDeclaration(v) => self.pass_function_declaration(v),
            NodeType::Identifier(v) => self.pass_identifier(v),
            NodeType::If(v) => self.pass_conditional(v),
            NodeType::Indexer(v) => {
                self.pass_node(&mut v.object);
                self.pass_vector_nodes(&mut v.index.parameters);
            }
            NodeType::IsCheck(v) => {
                self.pass_node(&mut v.left);
                self.pass_node(&mut v.right);
                return;
            }
            NodeType::LambdaExpression(v) => {
                self.scope.increase();
                self.pass_vector_nodes(&mut v.parameters.parameters);
                self.pass_vector_nodes(&mut v.body.body);
                self.scope.decrease();
            }
            NodeType::Match(v) => self.pass_match(v),
            NodeType::ObjectInstantiation(v) => self.pass_object_instantiation(v),
            NodeType::Operator(v) => {
                self.pass_node(&mut v.left);
                self.pass_node(&mut v.right);
                return;
            }
            NodeType::ReturnConditionalExpression(v) => {
                self.pass_node(&mut v.value);
                return;
            }
            NodeType::ReturnExpression(v) => {
                if let Some(value) = v {
                    self.pass_node(value);
                }
                return;
            }
            NodeType::ScopedIdentifier(v) => self.pass_scoped_identifier(v),
            NodeType::ScopedType(v) => self.pass_scoped_type(v),
            NodeType::Shebang(_) => return,
            NodeType::StructDeclaration(v) => self.pass_struct(v),
            NodeType::TernaryOperator(v) => {
                self.pass_node(&mut v.condition);
                self.pass_node(&mut v.then);
                self.pass_node(&mut v.else_then);
                return;
            }
            NodeType::TraitDeclaration(v) => self.pass_trait(v),
            NodeType::TupleDeclaration(v) => self.pass_vector_nodes(&mut v.parameters),
            NodeType::TupleExpression(v) => self.pass_vector_nodes(&mut v.parameters),
            NodeType::TypeDef(v) => self.pass_typedef(v),
            NodeType::UnaryOperator(v) => {
                self.pass_node(&mut v.operand);
                return;
            }
            NodeType::Use(v) => self.pass_token(&v.name),
            NodeType::VariableDeclaration(v) => self.pass_variable(v),
            NodeType::While(v) => self.pass_conditional(v),            
            NodeType::None => {
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

        enum_variant.symbol_id = id;
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

        variable_declaration.symbol = id;

        self.pass_node(&mut variable_declaration.var_type);

        if let Some(value) = &mut variable_declaration.value {
            self.pass_node(value);
        }
    }

    fn get_id_in_child_of_scope(&mut self, name: &String, parent_scope: &Scope) -> u32 {
        let mut id = u32::MAX;
        for symbol in self.transpiler.symbols.iter() {
            if parent_scope.coords().0 + 1 != symbol.scope.coords().0 || parent_scope.coords().1 != symbol.scope.coords().1 {
                continue;
            }
            if symbol.name == *name {
                id = symbol.id;
                self.scope = symbol.scope.clone();
                break;
            }
        }
        if id == u32::MAX {
            id = self.get_symbol_id(name);
        }
        id
    }

    fn pass_scoped_type(&mut self, scoped_type: &mut ScopedType) {
        let copy_scope = self.scope.clone();
        let mut last_scope: Option<Scope> = None; 
        for node in scoped_type.scope.iter_mut() {
            let name = &node.name.value;
            let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
            let id = if last_scope.is_none() {
                self.get_symbol_id(name)
            } else {
                self.get_id_in_child_of_scope(name, last_scope.as_ref().unwrap())
            };
            
            let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
            if self.symbol_is_unique(&symbol) {
                self.push_symbol(symbol);
            }

            node.symbol_id = id;
            
            if let Some(type_parameters) = &mut node.type_parameters {
                self.pass_vector_nodes(&mut type_parameters.parameters);
            }

            last_scope = Some(self.scope.clone());
        }
        self.scope = copy_scope;
    }

    fn pass_identifier(&mut self, identifier: &mut Identifier) {
        let name = &identifier.token.value;
        let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        }

        identifier.symbol_id = id;
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

        self.scope.increase();

        if let Some(type_parameters) = &mut struct_declaration.type_parameters {
            self.pass_anonymous_types(&mut type_parameters.parameters);
        }

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
        let copy_scope = self.scope.clone();
        let mut last_scope: Option<Scope> = None;
        for node in scoped_identifier.scope.iter_mut() {
            if let NodeType::Identifier(identifier) = &mut node.expression.node.as_mut() {
                // if it's an identifier, we can get its symbol id if it's a child of a member in scope
                let name = &identifier.token.value;
                let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
                let id = if last_scope.is_none() {
                    self.get_symbol_id(name)
                } else {
                    self.get_id_in_child_of_scope(name, last_scope.as_ref().unwrap())
                };
                
                let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
                if self.symbol_is_unique(&symbol) {
                    self.push_symbol(symbol);
                }

                identifier.symbol_id = id; 
            } else {
                self.pass_node(&mut node.expression);
            }
            
            if let Some(type_parameters) = &mut node.type_parameters {
                self.pass_vector_nodes(&mut type_parameters.parameters);
            }

            last_scope = Some(self.scope.clone());
        }
        self.scope = copy_scope;
    }

    fn pass_function_call(&mut self, function_call: &mut FunctionCall) {
        let name = &function_call.name.value;
        let object_type = ObjectTypes::Function;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        }

        function_call.symbol_id = id;
        
        self.pass_vector_nodes(&mut function_call.parameters.parameters);

        if let Some(type_parameters) = &mut function_call.type_parameters {
            self.pass_vector_nodes(&mut type_parameters.parameters);
        }
    }

    fn pass_conditional(&mut self, if_statement: &mut ConditionalRegion) {
        self.pass_node(&mut if_statement.condition);
        
        self.scope.increase();
        self.pass_vector_nodes(&mut if_statement.body.body);
        self.scope.decrease();

        for else_if in if_statement.else_if_regions.iter_mut() {
            self.pass_conditional(else_if);
        }

        if let Some(else_region) = &mut if_statement.else_region {
            self.scope.increase();
            self.pass_vector_nodes(&mut else_region.body);
            self.scope.decrease();
        }
    }

    fn pass_match(&mut self, match_statement: &mut MatchRegion) {
        self.pass_node(&mut match_statement.match_value);

        self.scope.increase();

        for case in match_statement.match_cases.iter_mut() {
            self.pass_node(&mut case.pattern);
            self.pass_node(&mut case.body);
        }

        self.scope.decrease();
    }

    fn pass_object_instantiation(&mut self, object_instantiation: &mut ObjectInstantiation) {
        let name = &object_instantiation.object_type.value;
        let object_type = ObjectTypes::Struct;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        }

        object_instantiation.symbol_id = id;

        if let Some(type_parameters) = &mut object_instantiation.type_parameters {
            self.pass_vector_nodes(&mut type_parameters.parameters);
        }

        for property in object_instantiation.properties.iter_mut() {
            self.pass_node(&mut property.value);
        }
    }

    fn pass_trait(&mut self, trait_declaration: &mut TraitDeclaration) {
        let name = &trait_declaration.name.value;
        let object_type = ObjectTypes::Struct;
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &trait_declaration.name.location);
        }

        trait_declaration.symbol_id = id;

        for extension in trait_declaration.extends.iter() {
            self.pass_token(extension);
        }

        self.scope.increase();

        for node in trait_declaration.body.body.iter_mut() {
            self.pass_node(node);
        }

        self.scope.decrease();
    }

    fn pass_typedef(&mut self, typedef: &mut TypeDefDeclaration) {
        let name = &typedef.name.token.value;
        let object_type = self.get_symbol_type(name, ObjectTypes::Identifier);
        let id = self.get_symbol_id(name);
        
        let symbol = Symbol::new(name.to_string(), object_type, id, self.scope.clone());
        if self.symbol_is_unique(&symbol) {
            self.push_symbol(symbol);
        } else {
            self.error(line!(), "Symbol is not unique", format!("The name `{name}` is already in use").as_str(), &typedef.name.token.location);
        }

        self.pass_node(&mut typedef.type_definition);
    }
}