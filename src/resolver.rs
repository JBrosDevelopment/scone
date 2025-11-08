// the purpose of this step is to go over the AST using symbols from the symbol table and identifing types adding them to codegen table and resolving any errors

#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};
use std::{f32::consts::E, rc::Rc};

pub fn resolver_pass_on_ast(transpiler: &mut Transpiler, error_handling: &mut ErrorHandling) {
    let mut resolver = Resolver::new(transpiler, error_handling);
    resolver.add_default_types();
    resolver.resolve_pass();

    error_handling.print_messages();
}

macro_rules! return_if {
    ($condition:expr, $return:expr) => {
        if $condition {
            return $return;
        }
    };
}

macro_rules! return_error {
    ($self:expr, $title:expr, $message:expr, $location:expr) => {
        $self.error(line!(), $title, $message, $location);
        return Err(());
    };
}

macro_rules! compare_types {
    ($self:expr, $expected_type:expr, $found_type:expr, $location:expr) => {
        {
            let ft: String;
            let et: String;
            if let Some(_et) = $expected_type {
                et = _et.symbol.name.to_string();
            } else {
                et = "()".to_string();
            }
            if let Some(_ft) = $found_type {
                ft = _ft.symbol.name.to_string();
            } else {
                ft = "()".to_string();
            }
            if !$self.equivalent_type(&et, &ft) {
                $self.error(line!(), "Unexpected type", format!("Expected `{}` but found `{}`", et, ft).as_str(), $location);
            }
        }
    };
}

#[derive(Debug)]
struct Resolver<'a> {
    pub transpiler: &'a mut Transpiler,
    pub output: &'a mut ErrorHandling,
    scope: Scope
}

impl<'a> Resolver<'a> {
    pub fn new(transpiler: &'a mut Transpiler, error_handling: &'a mut ErrorHandling) -> Resolver<'a> {
        Resolver { transpiler, output: error_handling, scope: Scope::new() }
    }

    #[allow(dead_code)]
    fn error(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_error("resolver", line, file!(), title, message, location);
    }

    #[allow(dead_code)]
    fn warning(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_warning("resolver", line, file!(), title, message, location);
    }

    #[allow(dead_code)]
    fn message(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_message("resolver", line, file!(), title, message, location);
    }

    fn get_symbol(&mut self, symbol_id: &Id, location: &Location) -> Result<Rc<Symbol>, ()> {
        let symbol = self.transpiler.symbols.get(symbol_id);
        if symbol.is_none() {
            self.error(line!(), "Symbol not found", format!("Internal Resolver Error: Symbol with id `{}` not found", symbol_id).as_str(), location);
            return Err(());
        }

        Ok(symbol.unwrap().clone())
    }

    fn get_base_type(&mut self, name: &'static str, location: &Location) -> Result<Rc<TypeHolder>, ()> {
        if !matches!(name, "bool" | "char" | "string" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64") {
            panic!("Expected `get_base_type` to be called with `name` as a base type: `bool` | `char` | `string` | `i8` | `i16` | `i32` | `i64` | `u8` | `u16` | `u32` | `u64` | `f32` | `f64`");
        }

        let symbol = self.transpiler.symbols.values().filter(|x| x.name == name).next();
        if symbol.is_none() {
            self.error(line!(), "Type not found", format!("Internal Resolver Error: Symbol with name `{}` not found", name).as_str(), location);
            return Err(());
        }
        let symbol = symbol.unwrap();
        if let Some(type_holder) = self.transpiler.table.types.iter().filter(|x| x.symbol.id == symbol.id).next() {
            return Ok(type_holder.clone());
        }

        self.error(line!(), "Type not found", format!("Internal Resolver Error: Type with name `{}` not found", name).as_str(), location);
        return Err(());
    }

    pub fn resolve_pass(&mut self) {
        let ast = std::mem::take(&mut self.transpiler.ast);
    
        for node in ast.iter() {
            _ = self.ast_node(node, None);
        }
    
        self.transpiler.ast = ast;
    }

    fn ast_node(&mut self, node: &ASTNode, expected: Option<Rc<TypeHolder>>) -> Result<(), ()> {
        match node.node.as_ref() {
            NodeType::VariableDeclaration(v) => self.variable_declaration(v, expected),
            NodeType::Constant(v) => self.constant(v, expected),
            NodeType::FunctionDeclaration(v) => self.function_declaration(v, expected),
            NodeType::ReturnExpression(v) => {
                if let Some(r) = v {
                    return self.ast_node(r, expected);
                }
                Ok(())
            }
            NodeType::TernaryOperator(v) => {
                let bool_type = self.get_base_type("bool", &node.token.location)?;
                self.ast_node(&v.condition, Some(bool_type))?;
                self.ast_node(&v.then, expected.clone())?;
                self.ast_node(&v.else_then, expected.clone())?;
                Ok(())
            }
            NodeType::Identifier(v) => {
                let symbol = self.get_symbol(&v.symbol_id, &node.token.location)?;
                
                if let Some(variable) = self.transpiler.table.variables.iter().find(|x| x.symbol.id == symbol.id) {
                    compare_types!(self, expected, Some(variable.ttype.type_holder.clone()), &node.token.location);
                } else if let Some(function) = self.transpiler.table.functions.iter().find(|x| x.symbol.id == symbol.id) {
                    todo!()

                } else if let Some(sstruct) = self.transpiler.table.structs.iter().find(|x| x.symbol.id == symbol.id) {
                    todo!()
                    
                } else if let Some(eenum) = self.transpiler.table.enums.iter().find(|x| x.symbol.id == symbol.id) {
                    todo!()
                    
                } else if let Some(ttrait) = self.transpiler.table.traits.iter().find(|x| x.symbol.id == symbol.id) {
                    todo!()
                    
                } else {
                    self.error(line!(), "Symbol not found", format!("Symbol `{}` not found", symbol.name).as_str(), &node.token.location);
                    return Err(());
                }

                Ok(())
            }
            NodeType::ScopedIdentifier(v) => self.scoped_identifier(v, expected),
            _ => {
                self.error(line!(), "Unimplemented resolver node", format!("Resolver for node type `{}` is not implemented yet", node.node.to_string()).as_str(), &node.token.location);
                self.output.print_messages();
                todo!();
            }
        }
    }

    fn scoped_identifier(&mut self, scoped_identifier_object: &ScopedIdentifier, expected: Option<Rc<TypeHolder>>) -> Result<(), ()> {
        if scoped_identifier_object.scope.is_empty() {
            self.error(line!(), "Empty scoped identifier", "Empty Scope, probobly internal parser error", &scoped_identifier_object.token.location);
            return Err(());
        }
        
        let mut scope_index = 0;
        let mut identifier_expression = &scoped_identifier_object.scope[scope_index];
        
        if scoped_identifier_object.scope.len() == 1 {
            return self.ast_node(&identifier_expression.expression, expected.clone());
        }

        let mut hasnt_been_colon = false;
        let mut cant_be_colon = false;
        let mut current_object = ObjectHolder::None;
        let mut current_location = scoped_identifier_object.token.location.clone();
        
        while scope_index < scoped_identifier_object.scope.len() {
            identifier_expression = &scoped_identifier_object.scope[scope_index];
            if identifier_expression.scope_type.clone().is_some_and(|x| x != ScopeType::DoubleColon) {
                hasnt_been_colon = true;
            } else if hasnt_been_colon {
                self.error(line!(), "Invalid scoped identifier", "After using `.` or `->` to access a member, all following scopes must also use `.` or `->`", &identifier_expression.expression.token.location);
                return Err(());
            }

            if cant_be_colon && identifier_expression.scope_type.clone().is_some_and(|x| x == ScopeType::DoubleColon) {
                self.error(line!(), "Invalid scoped identifier", "After scoping to instance, all following scopes must use `.` or `->`", &identifier_expression.expression.token.location);
                return Err(());
            }

            if let NodeType::Identifier(identifier) = identifier_expression.expression.node.as_ref() {
                current_location = identifier.token.location.clone();
                if identifier_expression.scope_type.as_ref().is_some_and(|x| *x == ScopeType::DoubleColon) || scope_index == 0 {
                    if let Some(holder) = self.transpiler.table.structs.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Struct(holder.clone());
                    } else if let Some(holder) = self.transpiler.table.enums.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Enum(holder.clone());
                    } else if let Some(holder) = self.transpiler.table.modules.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Module(holder.clone());
                    } else if let Some(holder) = self.transpiler.table.modules.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Module(holder.clone());
                    } else if let Some(holder) = self.transpiler.table.variables.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Variable(holder.clone());
                        cant_be_colon = true;
                    } else if !matches!(current_object, ObjectHolder::None) {
                        match &current_object {
                            ObjectHolder::Struct(s) => {
                                //debug!(s);
                                //debug!(identifier);
                                if let Some(holder) = s.structs.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Struct(holder.clone());
                                } else if let Some(holder) = s.enums.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Enum(holder.clone());
                                } else if let Some(holder) = s.members.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Variable(holder.clone());
                                    cant_be_colon = true;
                                } else {
                                    self.error(line!(), "Type not found", format!("Type `{}` not found in struct `{}`", identifier.token.value, s.symbol.name).as_str(), &identifier.token.location);
                                    return Err(());
                                }
                            }
                            ObjectHolder::Enum(e) => {
                                if let Some(holder) = e.members.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::EnumVariant(holder.clone());
                                } else {
                                    self.error(line!(), "Type not found", format!("Type `{}` not found in enum `{}`", identifier.token.value, e.symbol.name).as_str(), &identifier.token.location);
                                    return Err(());
                                }
                            }
                            ObjectHolder::Module(m) => {
                                if let Some(holder) = m.structs.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Struct(holder.clone());
                                } else if let Some(holder) = m.enums.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Enum(holder.clone());
                                } else if let Some(holder) = m.traits.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Trait(holder.clone());
                                } else if let Some(holder) = m.members.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Variable(holder.clone());
                                    cant_be_colon = true;
                                } else if let Some(holder) = m.modules.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Module(holder.clone());
                                } else {
                                    self.error(line!(), "Type not found", format!("Type `{}` not found in module `{}`", identifier.token.value, m.symbol.name).as_str(), &identifier.token.location);
                                    return Err(());
                                }
                            }
                            ObjectHolder::Trait(t) => {
                                if let Some(holder) = t.members.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                                    current_object = ObjectHolder::Variable(holder.clone());
                                    cant_be_colon = true;
                                } else {
                                    self.error(line!(), "Scope should end at trait", format!("Type `{}` not found in trait `{}`", identifier.token.value, t.symbol.name).as_str(), &identifier.token.location);
                                    return Err(());
                                }
                            }
                            _ => {
                                self.error(line!(), "Can't scope out of non type", format!("Type `{}` not found as type, maybe you meant to scope with `.` as instance", identifier.token.value).as_str(), &identifier.token.location);
                                return Err(());
                            }
                        }
                    } else {
                        self.error(line!(), "Type not found", format!("Type `{}` not found", identifier.token.value).as_str(), &identifier.token.location);
                        return Err(());
                    }
                } else { // isn't double colon
                    if let Some(variable) = self.transpiler.table.variables.iter().find(|x| x.symbol.id == identifier.symbol_id) {
                        current_object = ObjectHolder::Variable(variable.clone());
                    } else if !matches!(current_object, ObjectHolder::None) {
                        match &current_object {
                            ObjectHolder::Variable(v) => {
                                current_object = ObjectHolder::Variable(v.clone());
                            }
                            _ => {
                                self.error(line!(), "Can't access member of non instance", format!("Type `{}` is not an instance to access member from", identifier.token.value).as_str(), &identifier.token.location);
                                return Err(());
                            }
                        }
                    } else {
                        self.error(line!(), "Variable not found", format!("Variable `{}` not found", identifier.token.value).as_str(), &identifier.token.location);
                        return Err(());
                    }
                }
            } else if let NodeType::Indexer(indexer) = identifier_expression.expression.node.as_ref() {
                cant_be_colon = true;
                current_location = indexer.object.token.location.clone();
                todo!()
            } else if let NodeType::ObjectInstantiation(instantiation) = identifier_expression.expression.node.as_ref() {
                cant_be_colon = true;
                current_location = instantiation.object_type.location.clone();
                todo!()
                
            } else if let NodeType::TupleExpression(tuple) = identifier_expression.expression.node.as_ref() {
                cant_be_colon = true;
                current_location = tuple.token.location.clone();
                todo!()
                
            } else if let NodeType::FunctionCall(function_call) = identifier_expression.expression.node.as_ref() {
                cant_be_colon = true;
                current_location = function_call.name.location.clone();
                todo!()

            }
            scope_index += 1;
        }


        match current_object { 
            ObjectHolder::Variable(v) => {
                compare_types!(self, expected, Some(v.ttype.type_holder.clone()), &current_location);
                return Ok(());
            }
            ObjectHolder::Function(f) => {
                compare_types!(self, expected, Some(f.ttype.type_holder.clone()), &current_location);
                return Ok(());
            }
            ObjectHolder::EnumVariant(e) => {
                compare_types!(self, expected, Some(e.parent.ttype.clone()), &current_location);
                return Ok(());
            }
            ObjectHolder::Enum(_) | ObjectHolder::Struct(_) | ObjectHolder::Trait(_) | ObjectHolder::Type(_) => {
                self.error(line!(), "Expected instance but found type", "Expected instance but found type", &current_location);
                return Err(());
            }
            ObjectHolder::Module(_) | ObjectHolder::None => {
                self.error(line!(), "Expected instance but found module or nothing", "Expected instance but found module or nothing", &current_location);
                return Err(());
            }
        }
    }


    fn get_parameter_holder(&mut self, parameter: &DefinedNodeParameter) -> Result<ParameterHolder, ()> {
        let symbol = self.get_symbol(&parameter.symbol_id, &parameter.name.location)?;

        let type_holder = self.get_type_value_holder(&parameter.ty)?;

        if let Some(default) = &parameter.default_value {
            self.ast_node(&default, Some(type_holder.type_holder.clone()))?;
        }

        let parameter_holder = ParameterHolder {
            is_const: parameter.is_const,
            location: parameter.name.location.clone(),
            is_params: parameter.params,
            symbol: symbol.clone(),
            ttype: type_holder,
            default_value: parameter.default_value.clone(),
        };

        Ok(parameter_holder)
    }

    fn function_declaration(&mut self, function_declaration: &FunctionDeclaration, expected: Option<Rc<TypeHolder>>) -> Result<(), ()> {
        compare_types!(self, &expected, None::<TypeHolder>, &function_declaration.name.location);

        let symbol = self.get_symbol(&function_declaration.symbol_id, &function_declaration.name.location)?;
        let type_holder = self.get_type_value_holder(&function_declaration.return_type)?;

        let mut parameter_holders = vec![];
        for parameter in function_declaration.parameters.iter() {
            parameter_holders.push(self.get_parameter_holder(parameter)?);
        }

        if let Some(body) = &function_declaration.body {
            self.scope.increase();
            
            for p in parameter_holders.iter() {
                let var_holder = VariableHolder {
                    access_modifier: vec![],
                    has_value: true,
                    location: p.location.clone(),
                    requires_free: false,
                    tags: vec![],
                    symbol: p.symbol.clone(),
                    ttype: p.ttype.clone(),
                };
                self.transpiler.table.variables.push(Rc::new(var_holder));
            }

            for node in body.body.iter() {
                if let NodeType::ReturnExpression(_) = node.node.as_ref() {
                    self.ast_node(node, Some(type_holder.type_holder.clone()))?;
                } else {
                    self.ast_node(node, None)?;
                }
            }
            self.scope.decrease();
        }

        let function_holder = FunctionHolder {
            access_modifier: function_declaration.access_modifier.clone(),
            has_body: function_declaration.body.is_some(),
            location: function_declaration.name.location.clone(),
            tags: function_declaration.tags.clone(),
            symbol: symbol.clone(),
            type_parameters: vec![], // TODO
            ttype: type_holder,
            parameters: parameter_holders,
        };

        self.transpiler.table.functions.push(Rc::new(function_holder));
        Ok(())
    }

    fn constant(&mut self, constant: &ConstantNode, expected: Option<Rc<TypeHolder>>) -> Result<(), ()> {
        if expected.is_none() {
            self.error(line!(), "Unexpected constant", format!("Expected `()` but found constant `{}`", constant.value.value).as_str(), &constant.value.location);
            return Err(());
        }
        let expected = expected.unwrap();
        
        if constant.constant_type == ConstantType::Bool && expected.symbol.name == "bool" {
            return Ok(())
        }
        if constant.constant_type == ConstantType::Char && expected.symbol.name == "char" {
            return Ok(())
        }
        if constant.constant_type == ConstantType::String && expected.symbol.name == "string" {
            return Ok(())
        }
        if constant.constant_type.is_number() && matches!(expected.symbol.name.as_str(), "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64") {
            if  (expected.symbol.name == "i64" && matches!(constant.constant_type.to_string().as_str(), "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
                (expected.symbol.name == "i32" && matches!(constant.constant_type.to_string().as_str(), "i32" | "i16" | "i8" | "u8" | "u32" | "u16")) ||
                (expected.symbol.name == "i16" && matches!(constant.constant_type.to_string().as_str(), "i16" | "i8" | "u8" | "u16")) ||
                (expected.symbol.name == "i8" && matches!(constant.constant_type.to_string().as_str(), "i8" | "u8")) ||
                (expected.symbol.name == "u64" && matches!(constant.constant_type.to_string().as_str(), "u64" | "u32" | "u16" | "u8")) ||
                (expected.symbol.name == "u32" && matches!(constant.constant_type.to_string().as_str(), "u32" | "u16" | "u8")) ||
                (expected.symbol.name == "u16" && matches!(constant.constant_type.to_string().as_str(), "u16" | "u8")) ||
                (expected.symbol.name == "f64" && matches!(constant.constant_type.to_string().as_str(), "f32" | "f64" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
                (expected.symbol.name == "f32" && matches!(constant.constant_type.to_string().as_str(), "f32" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) {
                    return Ok(());
                }
        }

        self.error(line!(), "Unexpected constant", format!("Expected `{}` but found constant `{}`", expected.symbol.name, constant.value.value).as_str(), &constant.value.location);
        return Err(());
    }

    fn get_type_holder(&mut self, ttype: &ScopedType) -> Result<Rc<TypeHolder>, ()> {
        if ttype.scope.is_empty() {
            panic!("Empty scoped type");
        }

        let mut symbol = self.get_symbol(&ttype.scope[0].symbol_id, &ttype.scope[0].name.location)?;

        if ttype.scope.len() == 1 {
            if let Some(type_holder) = self.transpiler.table.types.iter().find(|x| x.symbol.id == symbol.id) {
                return Ok(type_holder.clone());
            } else {
                self.error(line!(), "Type not found", format!("Type `{}` not found", symbol.name).as_str(), &ttype.scope[0].name.location);
                return Err(());
            }
        }

        for (index, node) in ttype.scope.iter().enumerate() {
            if node.scope_type.as_ref().is_some_and(|x| *x != ScopeType::DoubleColon) {
                self.error(line!(), "Unexpected scope type", "Expected types to be scoped with `::`, for example: `Module::Struct`", &node.name.location);
                return Err(());
            }
            if let Some(next_node) = ttype.scope.get(index + 1) {
                if let Some(v) = self.transpiler.table.structs.iter().find(|x| x.symbol.id == symbol.id) {
                    if let Some(e) = v.enums.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = e.symbol.clone();
                    } else if let Some(f) = v.members.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = f.symbol.clone();
                    } else if let Some(m) = v.methods.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = m.symbol.clone();
                    } else if let Some(s) = v.structs.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = s.symbol.clone();
                    } else {
                        self.error(line!(), "Type not found", format!("Type `{}` not found in struct `{}`", next_node.name.value, symbol.name).as_str(), &next_node.name.location);
                        return Err(());
                    }
                }
                if let Some(v) = self.transpiler.table.traits.iter().find(|x| x.symbol.id == symbol.id) {
                    if let Some(f) = v.members.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = f.symbol.clone();
                    } else if let Some(m) = v.methods.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = m.symbol.clone();
                    } else {
                        self.error(line!(), "Type not found", format!("Type `{}` not found in trait `{}`", next_node.name.value, symbol.name).as_str(), &next_node.name.location);
                        return Err(());
                    }
                }
                if let Some(v) = self.transpiler.table.enums.iter().find(|x| x.symbol.id == symbol.id) {
                    if let Some(e) = v.members.iter().find(|x| x.symbol.id == next_node.symbol_id) {
                        symbol = e.symbol.clone();
                    } else {
                        self.error(line!(), "Type not found", format!("Enum variant `{}` not found in enum `{}`", next_node.name.value, symbol.name).as_str(), &next_node.name.location);
                        return Err(());
                    }
                }
            } else {
                symbol = self.get_symbol(&node.symbol_id, &node.name.location)?;
            }
        }
        
        for type_holder in self.transpiler.table.types.iter() {
            if type_holder.symbol.id == symbol.id {
                return Ok(type_holder.clone());
            }
        }

        self.error(line!(), "Type not found", format!("Type `{}` not found", symbol.name).as_str(), &ttype.scope[ttype.scope.len() - 1].name.location);
        Err(())
    }

    fn get_type_value_holder(&mut self, ttype: &ASTNode) -> Result<TypeValueHolder, ()> {
        match ttype.node.as_ref() {
            NodeType::ScopedType(v) => {
                let type_holder = self.get_type_holder(&v)?;
                let type_modifiers = v.type_modifiers.clone();
                Ok(TypeValueHolder {
                    type_holder,
                    type_modifiers
                })
            }
            _ => {
                self.error(line!(), "INTERNAL ERROR: Unimplemented resolver node", format!("Resolver for node type `{}` is not implemented yet", ttype.node.to_string()).as_str(), &ttype.token.location);
                self.output.print_messages();
                todo!();
            }
        }
    }

    fn variable_declaration(&mut self, var_declaration: &VariableDeclaration, expected_type: Option<Rc<TypeHolder>>) -> Result<(), ()> {
        compare_types!(self, expected_type, None::<TypeHolder>, &var_declaration.name.location);

        let symbol = self.get_symbol(&var_declaration.symbol_id, &var_declaration.name.location)?;
        
        let type_holder = self.get_type_value_holder(&var_declaration.var_type)?;
        
        if let Some(value) = &var_declaration.value {
            self.ast_node(value, Some(type_holder.type_holder.clone()))?;
        }

        let variable_holder = VariableHolder {
            access_modifier: var_declaration.access_modifier.clone(),
            has_value: var_declaration.value.is_some(),
            location: var_declaration.name.location.clone(),
            requires_free: false,
            tags: var_declaration.tags.clone(),
            symbol: symbol,
            ttype: type_holder,
        };

        self.transpiler.table.variables.push(Rc::new(variable_holder));
        Ok(())
    }

    fn equivalent_type(&mut self, a: &String, b: &String) -> bool {
        if a == b {
            return true;
        }

        if a == "i64" && matches!(b.as_str(), "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16") {
            return true; 
        } else if a == "i32" && matches!(b.as_str(), "i16" | "i8" | "u8" | "u32" | "u16") {
            return true; 
        } else if a == "i16" && matches!(b.as_str(), "i8" | "u8" | "u16") {
            return true; 
        } else if a == "i8" && matches!(b.as_str(), "u8") {
            return true; 
        } else if a == "u64" && matches!(b.as_str(), "u32" | "u16" | "u8") {
            return true; 
        } else if a == "u32" && matches!(b.as_str(), "u16" | "u8") {
            return true; 
        } else if a == "u16" && matches!(b.as_str(), "u8") {
            return true; 
        } else if a == "f64" && matches!(b.as_str(), "f32" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16") {
            return true; 
        } else if a == "f32" && matches!(b.as_str(), "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16") {
            return true; 
        }

        false
    }

    fn add_default_types(&mut self) {
        let top_scope = Scope::new();
        let i8_symbol = self.transpiler.get_symbol_by_name(&"i8".to_string(), &top_scope).expect("Could not find symbol with the name `i8` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i16_symbol = self.transpiler.get_symbol_by_name(&"i16".to_string(), &top_scope).expect("Could not find symbol with the name `i16` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i32_symbol = self.transpiler.get_symbol_by_name(&"i32".to_string(), &top_scope).expect("Could not find symbol with the name `i32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i64_symbol = self.transpiler.get_symbol_by_name(&"i64".to_string(), &top_scope).expect("Could not find symbol with the name `i164` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u8_symbol = self.transpiler.get_symbol_by_name(&"u8".to_string(), &top_scope).expect("Could not find symbol with the name `u8` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u16_symbol = self.transpiler.get_symbol_by_name(&"u16".to_string(), &top_scope).expect("Could not find symbol with the name `u16` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u32_symbol = self.transpiler.get_symbol_by_name(&"u32".to_string(), &top_scope).expect("Could not find symbol with the name `u32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u64_symbol = self.transpiler.get_symbol_by_name(&"u64".to_string(), &top_scope).expect("Could not find symbol with the name `u64` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let f32_symbol = self.transpiler.get_symbol_by_name(&"f32".to_string(), &top_scope).expect("Could not find symbol with the name `f32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let f64_symbol = self.transpiler.get_symbol_by_name(&"f64".to_string(), &top_scope).expect("Could not find symbol with the name `f64` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let bool_symbol = self.transpiler.get_symbol_by_name(&"bool".to_string(), &top_scope).expect("Could not find symbol with the name `bool` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let string_symbol = self.transpiler.get_symbol_by_name(&"string".to_string(), &top_scope).expect("Could not find symbol with the name `string` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let void_symbol = self.transpiler.get_symbol_by_name(&"void".to_string(), &top_scope).expect("Could not find symbol with the name `void` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");

        let i8_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: i8_symbol, parent_id: None });
        let i16_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: i16_symbol, parent_id: None });
        let i32_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: i32_symbol, parent_id: None });
        let i64_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: i64_symbol, parent_id: None });
        let u8_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: u8_symbol, parent_id: None });
        let u16_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: u16_symbol, parent_id: None });
        let u32_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: u32_symbol, parent_id: None });
        let u64_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: u64_symbol, parent_id: None });
        let f32_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: f32_symbol, parent_id: None });
        let f64_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: f64_symbol, parent_id: None });
        let bool_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: bool_symbol, parent_id: None });
        let string_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: string_symbol, parent_id: None });
        let void_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: void_symbol, parent_id: None });

        self.transpiler.table.types.push(i8_type_holder.clone());
        self.transpiler.table.types.push(i16_type_holder);
        self.transpiler.table.types.push(i32_type_holder);
        self.transpiler.table.types.push(i64_type_holder);
        self.transpiler.table.types.push(u8_type_holder);
        self.transpiler.table.types.push(u16_type_holder);
        self.transpiler.table.types.push(u32_type_holder);
        self.transpiler.table.types.push(u64_type_holder);
        self.transpiler.table.types.push(f32_type_holder);
        self.transpiler.table.types.push(f64_type_holder);
        self.transpiler.table.types.push(bool_type_holder);
        self.transpiler.table.types.push(string_type_holder);
        self.transpiler.table.types.push(void_type_holder.clone());

        let test_symbol = self.transpiler.get_symbol_by_name(&"Test".to_string(), &top_scope).expect("Could not find symbol with the name `Test` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let inside_symbol = self.transpiler.get_symbol_by_name(&"Inside".to_string(), &top_scope).expect("Could not find symbol with the name `Inside` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let function_symbol = self.transpiler.get_symbol_by_name(&"function".to_string(), &top_scope).expect("Could not find symbol with the name `function` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let variable_symbol = self.transpiler.get_symbol_by_name(&"variable".to_string(), &top_scope).expect("Could not find symbol with the name `variable` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");

        let test_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: test_symbol.clone(), parent_id: None });
        let inside_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: inside_symbol.clone(), parent_id: Some(test_type_holder.clone()) });

        self.transpiler.table.types.push(test_type_holder.clone());
        self.transpiler.table.types.push(inside_type_holder.clone());

        let function_holder = Rc::new(FunctionHolder {
            access_modifier: vec![],
            has_body: true,
            location: Location::new_empty(),
            tags: vec![],
            symbol: function_symbol,
            type_parameters: vec![],
            ttype: TypeValueHolder {
                type_holder: void_type_holder.clone(),
                type_modifiers: vec![],
            },
            parameters: vec![],
        });

        let variable_holder = Rc::new(VariableHolder {
            access_modifier: vec![],
            has_value: false,
            location: Location::new_empty(),
            requires_free: false,
            tags: vec![],
            symbol: variable_symbol.clone(),
            ttype: TypeValueHolder {
                type_holder: i8_type_holder.clone(),
                type_modifiers: vec![],
            },
        });

        let inside_struct = Rc::new(StructHolder {
            access_modifier: vec![],
            enums: vec![],
            members: vec![variable_holder],
            location: Location::new_empty(),
            symbol: inside_symbol,
            ttype: inside_type_holder,
            inherits: vec![],
            methods: vec![function_holder],
            structs: vec![],
            tags: vec![],
            type_parameters: vec![],
        });

        let test_struct = Rc::new(StructHolder {
            access_modifier: vec![],
            enums: vec![],
            members: vec![],
            location: Location::new_empty(),
            symbol: test_symbol,
            ttype: test_type_holder,
            inherits: vec![],
            methods: vec![],
            structs: vec![inside_struct],
            tags: vec![],
            type_parameters: vec![],
        });

        self.transpiler.table.structs.push(test_struct);
    }
}