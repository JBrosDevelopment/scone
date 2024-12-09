pub mod error_handling;
pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    let path = "src/test_code.scn".to_string();
    let code = std::fs::read_to_string(&path).unwrap();
    
    // lexer
    let mut lexer = lexer::Lexer::new(&code, &path);
    let tokens = lexer.lex();
    
    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/lexer.json", json).unwrap();

    // parser    
    let ast = parser::parse(tokens, &path, &code).unwrap();

    let fmt_json = serde_json::to_string_pretty(&ast).unwrap();
    std::fs::write("src/parser.json", fmt_json).unwrap();
}