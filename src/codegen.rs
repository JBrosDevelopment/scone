#[allow(unused_imports)]
use crate::{ast::*, macros::*, lexer::*, transpiler::*, error_handling::{ErrorHandling, DEBUGGING, Message}};
#[allow(unused_imports)]
use crate::debug;

pub fn generate_code(transpiler: &Transpiler) -> String {
    todo!();


    // testing stuff
    let mut codegen  = CodegenTable::new();
    codegen.increase_scope();
    let string_type = codegen.generate_type("String".to_string());
    codegen.add_type_scope(string_type);
    let empty_enum = codegen.generate_enum("EmptyEnum".to_string(), vec![], vec![]);
    codegen.add_identifier_scope(empty_enum);
    codegen.decrease_scope();
}