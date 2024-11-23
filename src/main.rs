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

/*
use new_ast::*;
fn generate_new_ast() -> Vec<ASTNode> {
    let variable_name_token = Box::new(lexer::Token {
        token_type: lexer::TokenType::Identifier,
        value: "my_var".to_string(),
        location: lexer::Location { line: 1, column: 1, length: 6 },
    });

    let variable_type_token = Box::new(lexer::Token {
        token_type: lexer::TokenType::Identifier,
        value: "i32".to_string(),
        location: lexer::Location { line: 1, column: 8, length: 3 },
    });

    let variable_value_token = Box::new(lexer::Token {
        token_type: lexer::TokenType::NumberConstant,
        value: "42".to_string(),
        location: lexer::Location { line: 1, column: 12, length: 2 },
    });

    // ASTNode representing the value assigned to the variable
    let variable_value_node = ASTNode {
        token: variable_value_token.clone(),
        children: vec![],
        node: Box::new(NodeType::Constant(ConstantNode {
            value: variable_value_token,
            constant_type: ConstantType::I32,
        })),
    };

    // NodeType::VariableDeclaration
    let variable_declaration_node = ASTNode {
        token: variable_name_token.clone(),
        children: vec![],
        node: Box::new(NodeType::VariableDeclaration(VariableDeclaration {
            var_type: Box::new(NodeType::TypeIdentifier(ScopeToIdentifier {
                child: None,
                identifier: variable_type_token.clone(),
            })),
            var_name: variable_name_token,
            var_value: Box::new(variable_value_node),
            description: None,
            access_modifier: Some(vec![AccessModifier::None]),
        })),
    };

    vec![variable_declaration_node]
}
*/