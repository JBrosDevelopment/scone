pub mod error_handling;
pub mod macros;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod transpiler;
pub mod codegen;

fn main() {
    let path = "src/testing/test_code.sx".to_string();
    let code = std::fs::read_to_string(&path).unwrap();
    
    // lexer
    let (tokens, output, macros) = lexer::lex(&code, Some(path.clone()), None);

    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/testing/lexer.out.json", json).unwrap();

    if output.has_errors() {
        error_handling::ErrorHandling::print_unable_to_continue_message();
        return;
    }

    // parser
    let (ast, output) = parser::parse(tokens, &code, Some(path.clone()));

    let mut ast_string = String::new();
    for node in &ast {
        ast_string.push_str(parser::Parser::node_expr_to_string(node, 0).as_str());
        ast_string.push(';');
        ast_string.push('\n');
    }
    ast_string = ast_string.replace(" ;", ";").replace(";;", ";");
    std::fs::write("src/testing/ast.out.sx", ast_string).unwrap();

    let fmt_json = serde_json::to_string_pretty(&ast).unwrap();
    std::fs::write("src/testing/parser.out.json", fmt_json).unwrap();
    
    if output.has_errors() {
        error_handling::ErrorHandling::print_unable_to_continue_message();
        return;
    }

    // transpiler
    let (out_code, output) = transpiler::transpile(ast, &code, Some(path.clone()), macros);

    std::fs::write("src/testing/transpiler.out.c", out_code.clone()).unwrap();

    if output.has_errors() {
        error_handling::ErrorHandling::print_unable_to_continue_message();
        return;
    }
}
