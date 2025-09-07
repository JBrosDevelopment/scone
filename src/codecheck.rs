#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};

pub fn check_ast(transpiler: &mut Transpiler) {
    let mut checker = Checker::new(transpiler.clone());
    checker.check();
    transpiler.output = checker.transpiler.output.clone();
    transpiler.table = checker.transpiler.table.clone();
}

macro_rules! err {
    () => {
        Err(())
    };
}
macro_rules! ok {
    () => {
        Ok(())
    };
}

macro_rules! assert {
    ($conditional:expr) => {
        if !$conditional {
            return err!();
        }
    };
}

macro_rules! assert_err {
    ($expression:expr, $self:expr, $title:expr, $message:expr, $location:expr) => {
        if !$expression { 
            $self.error(line!(), $title, $message, $location);
            return err!();
        }
    };
}

macro_rules! assert_return {
    ($expression:expr, $returns:expr) => {
        if !$expression {
            return $returns;
        }
    };
}

macro_rules! check_unexpected_type {
    ($self:expr, $location:expr, $type_id:expr, $found_type_id:expr) => {
        if let Some(__type_id__) = $type_id {
            if let Some(__found_type_id__) = $found_type_id {
                if $self.transpiler.table.check_types_compatibility(__type_id__, __found_type_id__) == false {
                    let expected_type = $self.get_type($location, $type_id)?;
                    let found_type = if $found_type_id.is_some() { $self.get_type($location, $found_type_id)? } else { "()".to_string() };
                    $self.error(line!(), "Unexpected type", format!("Expected type `{expected_type}` but found `{found_type}` instead").as_str(), $location);
                    return err!();
                }
            } else {
                let expected_type = $self.get_type($location, $type_id)?;
                let found_type = if $found_type_id.is_some() { $self.get_type($location, $found_type_id)? } else { "()".to_string() };
                $self.error(line!(), "Unexpected type", format!("Expected type `{expected_type}` but found `{found_type}` instead").as_str(), $location);
                return err!();
            }
        }
    };
}

struct Checker {
    pub transpiler: Transpiler,
}

impl Checker {
    pub fn new(transpiler: Transpiler) -> Checker {
        Checker { transpiler }
    }
    pub fn error(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra errors on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.error("transpiler checking error", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.transpiler.output.error("transpiler checking error", message, help, location);
        }
    }
    pub fn warning(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra warnings on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.warning("transpiler checking warning", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.transpiler.output.warning("transpiler checking warning", message, help, location);
        }
    }
    pub fn message(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.transpiler.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra messages on the same line
            return;
        }
        if DEBUGGING {
            self.transpiler.output.message("transpiler checking message", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        } else {
            self.transpiler.output.message("transpiler checking message", message, help, location);
        }
    }

    fn get_type(&mut self, location: &Location, type_id: Option<TypeId>) -> Result<String, ()> {
        if let Some(type_id) = type_id {
            if let Ok(ty) = self.transpiler.table.get_type_id(type_id) {
                Ok(self.transpiler.table.get_type_enum_name(&ty))
            } else {
                self.error(line!(), "Type does not exist", format!("Type with the id `{}` does not exist", type_id).as_str(), location);
                return err!();
            }
        } else {
            self.error(line!(), "No current type", "Expected to return a specific type here", location);
            return err!();
        }
    }
    pub fn check(&mut self) {
        for node in self.transpiler.ast.clone() {
            _ = self.check_node(&node, None, false);
        }
    }

    fn check_if_node_is_constant(&mut self, node: &ASTNode, type_id: Option<TypeId>) -> Result<(), ()> {
        todo!()
    }

    fn check_node(&mut self, node: &ASTNode, type_id: Option<TypeId>, constant: bool) -> Result<(), ()> {
        if constant {
            return self.check_if_node_is_constant(node, type_id);
        }
        match node.node.as_ref() {
            NodeType::Constant(ref v) => self.constant(v, type_id),
            NodeType::VariableDeclaration(ref v) => self.variable_declaration(v, type_id),
            _ => {
                let message = format!("Node Type `{}` is not supported yet", node.node.to_string());
                self.error(line!(), message.as_str(), message.as_str(), &node.token.location);
                self.transpiler.output.print_messages();
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
                self.transpiler.output.print_messages();
                todo!();
            }
        }
    }

    fn scoped_type(&mut self, node: &ScopedType) -> Result<TypeId, ()> {
        if node.scope.is_empty() {
            self.error(line!(), "No scope in type", "Expected type to have a scope", &node.token.location);
            return err!();
        }
        
        // check for scoping errors
        if node.scope.iter().skip(1).enumerate().any(|(_, x)| x.scope_type != Some(ScopeType::DoubleColon)) {
            // all scoping needs to be done with double colons
            self.error(line!(), "Invalid scoping", "Expected all type scoping to use `::`, for example `A::B::C", &node.token.location);
            return err!();
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
                self.error(line!(), "Type does not exist", format!("Type `{}` does not exist", root_name).as_str(), &node.token.location);
                self.transpiler.table.scope = capture_table_scope;
                return err!();
            }
            
            // get the id of the root type if it is a module, ie. struct or enum 
            let root_type_module_id = self.transpiler.table.get_module_identifier_enum_id_from_type_id(root_type_id).ok();
            
            if root_type_module_id.is_none() {
                if scope.len() > 1 {
                    // an error occurs because the root type is not a module, meaning it can't scope into anything because it doesn't know where to scope into
                    self.error(line!(), "Invalid scoping", "Expected the root type to be a module, eg. `struct`, `enum`", &node.token.location);
                    self.transpiler.table.scope = capture_table_scope;
                    return err!();
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
                    return err!();
                }
            } else {
                // could not find the module
                self.error(line!(), "Could not find module", "Could not find type with this name", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return err!();
            }
    
            // get module id
            let module_type_id: TypeId;
            match &module_identifier_enum {
                IdentifierEnum::Function(_) | IdentifierEnum::Variable(_) => {
                    self.error(line!(), "Invalid scoping", "Expected the root type to be a module, eg. `struct`, `enum`", &scope[0].name.location);
                    self.transpiler.table.scope = capture_table_scope;
                    return err!();
                }
                IdentifierEnum::Struct(x) => module_type_id = x.type_id,
                IdentifierEnum::Enum(x) => module_type_id = x.type_id,
                IdentifierEnum::Trait(x) => module_type_id = x.type_id,
            }
    
            // make sure the type_id's match
            if module_type_id != root_type_id {
                self.error(line!(), "Invalid scoping", "The root type and module have a mismatching type id.", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return err!();
            }
    
            // scope has a length of 1, so we are at the end of the scope
            if scope.len() == 1 {
                break;
            }
    
            // if module type is an enum, then we can not scope any farther
            if let IdentifierEnum::Enum(_) = module_identifier_enum {
                self.error(line!(), "Invalid scoping", "Can not scope into an enum as if were a type", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return err!();
            }
    
            // if the module is a trait, then we can not scope any farther
            if let IdentifierEnum::Trait(_) = module_identifier_enum {
                self.error(line!(), "Invalid scoping", "Can not scope into a trait as if were a type", &scope[0].name.location);
                self.transpiler.table.scope = capture_table_scope;
                return err!();
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
                        return err!();
                    } else {
                        self.error(line!(), "Could not find type", "Could not find type with this name", &scope[1].name.location);
                        self.transpiler.table.scope = capture_table_scope;
                        return err!();
                    }
                }
                _ => unreachable!()
            }
        }

        self.transpiler.table.scope = capture_table_scope;
        Ok(root_type_id)
    }

    fn variable_declaration(&mut self, node: &VariableDeclaration, type_id: Option<TypeId>) -> Result<(), ()> {
        check_unexpected_type!(self, &node.var_name.location, type_id, None::<TypeId>);

        // evaluate and get type id
        let type_id = self.evaluate_type(&node.var_type)?;

        // check if variable name is unique
        let name = node.var_name.value.clone();
        if !self.transpiler.table.name_is_used_in_scope(&name) {
            self.error(line!(), "Variable name is not unique", format!("Variable name `{}` is not unique", name).as_str(), &node.var_name.location);
            return err!();
        }
        
        // evaluate value
        let value_id: Option<TypeId>;
        if let Some(value) = &node.var_value {
            value_id = Some(self.evaluate_type(value)?);
        } else {
            value_id = None;
        }

        // check value type is compatible with type
        check_unexpected_type!(self, &node.var_value.clone().unwrap().token.location, Some(type_id), value_id);

        // add variable to scope
        let variable = self.transpiler.table.generate_variable(name, type_id, node.var_value.is_some(), false, node.access_modifier.clone(), node.tags.clone(), node.var_name.location.clone());
        self.transpiler.table.add_identifier_scope(variable);

        ok!()
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
                err!()
            }
        }
    }
    
}