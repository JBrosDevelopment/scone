pub mod error_handling;
pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    let path = "src/testing/test_code.scn".to_string();
    let code = std::fs::read_to_string(&path).unwrap();
    
    // lexer
    let tokens = lexer::lex(&code, &path);
    
    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/testing/lexer.out.json", json).unwrap();

    // parser    
    let ast = parser::parse(tokens, &path, &code);

    let fmt_json = serde_json::to_string_pretty(&ast).unwrap();
    std::fs::write("src/testing/parser.out.json", fmt_json).unwrap();
}