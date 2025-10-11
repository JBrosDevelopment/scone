// the purpose of this step is to go over the AST using symbols from the symbol table and identifing types adding them to codegen table and resolving any errors

#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};
use std::rc::Rc;

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
            if et != ft {
                $self.error(line!(), "Unexpected type", format!("Expected `{}` but found `{}`", et, ft).as_str(), $location);
            }
        }
    };
}

#[derive(Debug)]
struct Resolver<'a> {
    pub transpiler: &'a mut Transpiler,
    pub output: &'a mut ErrorHandling
}

impl<'a> Resolver<'a> {
    pub fn new(transpiler: &'a mut Transpiler, error_handling: &'a mut ErrorHandling) -> Resolver<'a> {
        Resolver { transpiler, output: error_handling }
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
            _ => {
                self.error(line!(), "Unimplemented resolver node", format!("Resolver for node type `{}` is not implemented yet", node.node.to_string()).as_str(), &node.token.location);
                self.output.print_messages();
                todo!();
            }
        }
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

        self.transpiler.table.variables.push(variable_holder);
        Ok(())
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

        self.transpiler.table.types.push(i8_type_holder);
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
        self.transpiler.table.types.push(void_type_holder);

        let test_symbol = self.transpiler.get_symbol_by_name(&"Test".to_string(), &top_scope).expect("Could not find symbol with the name `Test` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let inside_symbol = self.transpiler.get_symbol_by_name(&"Inside".to_string(), &top_scope).expect("Could not find symbol with the name `Inside` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");

        let test_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: test_symbol.clone(), parent_id: None });
        let inside_type_holder = Rc::new(TypeHolder { is_generic: false, symbol: inside_symbol.clone(), parent_id: Some(test_type_holder.clone()) });

        self.transpiler.table.types.push(test_type_holder.clone());
        self.transpiler.table.types.push(inside_type_holder.clone());

        let inside_struct = StructHolder {
            access_modifier: vec![],
            enums: vec![],
            members: vec![],
            location: Location::new_empty(),
            symbol: inside_symbol,
            ttype: inside_type_holder,
            inherits: vec![],
            methods: vec![],
            structs: vec![],
            tags: vec![],
            type_parameters: vec![],
        };

        let test_struct = StructHolder {
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
        };

        self.transpiler.table.structs.push(test_struct);
    }
}