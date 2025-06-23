use serde::Serialize;
#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, error_handling::{ErrorHandling, DEBUGGING, Message}};
#[allow(unused_imports)]
use crate::debug;

pub fn codegen(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let mut transpiler = Transpiler::new(ast, code, path, macros);
    let code = transpiler.transpile();

    transpiler.output.print_messages();
    (code, transpiler.output)
}

pub struct Transpiler {
    ast: Vec<ASTNode>,
    output: ErrorHandling,
    macros: Macros,
    
}

impl Transpiler {
    pub fn new(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> Transpiler {
        Transpiler { ast, 
            output: ErrorHandling::new(path.clone(), 
            code.clone()), 
            macros 
        }
    }
    pub fn transpile(&mut self) -> String { 
        todo!()
    }
}