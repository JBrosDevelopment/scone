// The purpose of this step is to go over the AST and grab the symbols of each type, and then add them to the symbol table


#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}, debug};

pub fn declarer_pass_on_ast(transpiler: &mut Transpiler) {
}