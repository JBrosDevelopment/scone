mod lexer;
mod ast;

fn main() {
    // lexer

    let code = std::fs::read_to_string("src/test_code.scn").unwrap();
    let tokens = lexer::lex(code.as_str()).unwrap();
    
    let json = serde_json::to_string_pretty(&tokens).unwrap();
    std::fs::write("src/lexer.json", json).unwrap();

    // parser
    

}

