pub mod error_handling;
pub mod macros;
pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    let path = "src/testing/test_code.scn".to_string();
    let code = std::fs::read_to_string(&path).unwrap();
    
    // lexer
    let (tokens, output, macros) = lexer::lex(&code, Some(path.clone()), None);

    println!("{:#?}", macros);
    
    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/testing/lexer.out.json", json).unwrap();

    if output.has_errors() {
        error_handling::ErrorHandling::print_unable_to_continue_message();
        return;
    }

    // parser
    let (ast, _output) = parser::parse(tokens, &code, Some(path.clone()));

    let mut ast_string = String::new();
    for node in &ast {
        ast_string.push_str(parser::Parser::node_expr_to_string(node, 0).as_str());
        ast_string.push(';');
        ast_string.push('\n');
    }
    std::fs::write("src/testing/ast.out.scn", ast_string).unwrap();

    let fmt_json = serde_json::to_string_pretty(&ast).unwrap();
    std::fs::write("src/testing/parser.out.json", fmt_json).unwrap();
}