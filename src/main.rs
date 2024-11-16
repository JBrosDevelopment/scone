pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    // lexer

    let code = std::fs::read_to_string("src/test_code.scn").unwrap();
    let tokens = lexer::lex(code.as_str()).unwrap();
    
    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/lexer.json", json).unwrap();

    // parser
    
    let ast = parser::generate_ast(tokens).unwrap();

    let fmt_json = format!("{:#?}", ast);
    std::fs::write("src/parser.json", fmt_json).unwrap();
}

