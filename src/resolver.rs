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

    pub fn resolve_pass(&mut self) {
        let ast = std::mem::take(&mut self.transpiler.ast);
    
        for node in ast.iter() {
            _ = self.ast_node(node, None);
        }
    
        self.transpiler.ast = ast;
    }

    fn ast_node(&mut self, node: &ASTNode, type_id: Option<TypeId>) -> Result<(), ()> {
        match node.node.as_ref() {
            NodeType::VariableDeclaration(v) => self.variable_declaration(v, type_id),
            _ => {
                self.error(line!(), "Unimplemented resolver node", format!("Resolver for node type `{}` is not implemented yet", node.node.to_string()).as_str(), &node.token.location);
                self.output.print_messages();
                todo!();
            }
        }
    }

    fn variable_declaration(&mut self, var_declaration: &VariableDeclaration, type_id: Option<TypeId>) -> Result<(), ()> {
        //check_unexpected_type!(self, &var_declaration.name.location, type_id, None);

        todo!()
    }

    fn add_default_types(&mut self) {
        let top_scope = Scope::new();
        let i8_symbol = self.transpiler.get_symbol(&"i8".to_string(), &top_scope).expect("Could not find symbol with the name `i8` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i16_symbol = self.transpiler.get_symbol(&"i16".to_string(), &top_scope).expect("Could not find symbol with the name `i16` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i32_symbol = self.transpiler.get_symbol(&"i32".to_string(), &top_scope).expect("Could not find symbol with the name `i32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let i64_symbol = self.transpiler.get_symbol(&"i64".to_string(), &top_scope).expect("Could not find symbol with the name `i164` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u8_symbol = self.transpiler.get_symbol(&"u8".to_string(), &top_scope).expect("Could not find symbol with the name `u8` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u16_symbol = self.transpiler.get_symbol(&"u16".to_string(), &top_scope).expect("Could not find symbol with the name `u16` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u32_symbol = self.transpiler.get_symbol(&"u32".to_string(), &top_scope).expect("Could not find symbol with the name `u32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let u64_symbol = self.transpiler.get_symbol(&"u64".to_string(), &top_scope).expect("Could not find symbol with the name `u64` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let f32_symbol = self.transpiler.get_symbol(&"f32".to_string(), &top_scope).expect("Could not find symbol with the name `f32` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let f64_symbol = self.transpiler.get_symbol(&"f64".to_string(), &top_scope).expect("Could not find symbol with the name `f64` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let bool_symbol = self.transpiler.get_symbol(&"bool".to_string(), &top_scope).expect("Could not find symbol with the name `bool` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let string_symbol = self.transpiler.get_symbol(&"string".to_string(), &top_scope).expect("Could not find symbol with the name `string` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");
        let void_symbol = self.transpiler.get_symbol(&"void".to_string(), &top_scope).expect("Could not find symbol with the name `void` in transpiler's symbols member. Make sure to use `add_default_symbols` in declarer pass to add symbols to tranpiler.");

        let i8_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: i8_symbol };
        let i16_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: i16_symbol };
        let i32_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: i32_symbol };
        let i64_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: i64_symbol };
        let u8_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: u8_symbol };
        let u16_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: u16_symbol };
        let u32_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: u32_symbol };
        let u64_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: u64_symbol };
        let f32_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: f32_symbol };
        let f64_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: f64_symbol };
        let bool_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: bool_symbol };
        let string_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: string_symbol };
        let void_type_holder = TypeHolder { compatible_types: vec![], is_generic: false, symbol: void_symbol };
        
        self.transpiler.table.push_type(Rc::new(i8_type_holder));
        self.transpiler.table.push_type(Rc::new(i16_type_holder));
        self.transpiler.table.push_type(Rc::new(i32_type_holder));
        self.transpiler.table.push_type(Rc::new(i64_type_holder));
        self.transpiler.table.push_type(Rc::new(u8_type_holder));
        self.transpiler.table.push_type(Rc::new(u16_type_holder));
        self.transpiler.table.push_type(Rc::new(u32_type_holder));
        self.transpiler.table.push_type(Rc::new(u64_type_holder));
        self.transpiler.table.push_type(Rc::new(f32_type_holder));
        self.transpiler.table.push_type(Rc::new(f64_type_holder));
        self.transpiler.table.push_type(Rc::new(bool_type_holder));
        self.transpiler.table.push_type(Rc::new(string_type_holder));
        self.transpiler.table.push_type(Rc::new(void_type_holder));
    }
}