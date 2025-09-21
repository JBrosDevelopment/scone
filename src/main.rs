pub mod error_handling;
pub mod macros;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod transpiler;
pub mod codegen;
pub mod declarer;
pub mod resolver;

#[macro_export]
macro_rules! debut_out_contents {
    ($path:expr, $contents:expr) => {
        if error_handling::DEBUGGING {
            let json = serde_json::to_string_pretty(&$contents).unwrap();
            std::fs::write($path, json).unwrap();
        }
    };
}

fn main() {
    let path = "src/testing/test_code.sx".to_string();
    let code = std::fs::read_to_string(&path).unwrap();

    let mut error_handling = error_handling::ErrorHandling::new(Some(path.clone()), code.clone());
    let mut macros = macros::Macros::new();
    
    // lexer
    let tokens = lexer::lex(&mut error_handling, &mut macros);

    debut_out_contents!("src/testing/lexer.out.json", &tokens);
    crate::check_if_can_continue!(error_handling, true, ());

    // parser
    let ast = parser::parse(tokens, &mut error_handling);

    let ast_string = ast::ast_as_string(&ast);
    std::fs::write("src/testing/ast.out.sx", ast_string).unwrap();

    debut_out_contents!("src/testing/parser.out.json", &ast);
    crate::check_if_can_continue!(error_handling, true, ());

    // transpiler
    let out_code = transpiler::transpile(ast, &mut error_handling, macros);

    std::fs::write("src/testing/transpiler.out.c", out_code.clone()).unwrap();

    crate::check_if_can_continue!(error_handling, true, ());
}
