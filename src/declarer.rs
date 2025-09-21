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
    pub output: &'a mut ErrorHandling
}

impl<'a> Declarer<'a> {
    pub fn new(transpiler: &'a mut Transpiler, error_handling: &'a mut ErrorHandling) -> Declarer<'a> {
        Declarer { transpiler, output: error_handling }
    }

    #[allow(dead_code)]
    fn error(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_error("declarer", line, file!(), message, title, location);
    }

    #[allow(dead_code)]
    fn warning(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_warning("declarer", line, file!(), message, title, location);
    }

    #[allow(dead_code)]
    fn message(&mut self, line: u32, title: &str, message: &str, location: &Location) {
        self.output.add_instance_message("declarer", line, file!(), message, title, location);
    }

    pub fn declare_pass(&mut self) {

    }
}