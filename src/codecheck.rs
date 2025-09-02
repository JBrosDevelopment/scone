#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};

pub fn check_ast(transpiler: &mut Transpiler) {
    let mut checker = Checker::new(transpiler.clone());
    checker.check();
    transpiler.output = checker.transpiler.output.clone();
}


struct Checker {
    pub transpiler: Transpiler,
}

impl Checker {
    pub fn new(transpiler: Transpiler) -> Checker {
        Checker { transpiler }
    }
    pub fn check(&mut self) {
        
    }
}