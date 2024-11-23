use crate::lexer::{Token, TokenType, Location};
use crate::new_ast::*;
use crate::error_handling::ErrorHandling;

pub(crate) fn parse(tokens: Vec<Token>, file: &String, code: &String) -> Result<Vec<ASTNode>, String> {
    let mut parser = Parser::new(Some(file.clone()), code.clone());
    parser.generate_ast(tokens);
    parser.output.print_messages();
    Ok(parser.ast)
}

#[derive(Debug, Clone)]
pub(crate) struct Parser {
    output: ErrorHandling,
    ast: Vec<ASTNode>
}

impl Parser {
    pub fn new(file: Option<String>, full_code: String) -> Parser {
        Parser { output: ErrorHandling::new(file, full_code), ast: Vec::new() }
    }
    pub fn generate_ast(&mut self, tokens: Vec<Token>) {
        // self.error("this is a test parsing error", "this should help", &Location { line: 3, column: 6, length: 5 });
        // self.warning("this is a test parsing error over 10 lines", "right here", &Location { line: 13, column: 18, length: 4 });
        // self.message("this is a test parsing error over 100 lines", "here", &Location { line: 125, column: 3, length: 4 });
        
        self.ast = ASTGenerator::generate(&tokens, self);
    }

    pub fn error(&mut self, message: &str, help: &str, location: &Location) {
        self.output.error("parsing error", message, help, location);
    }
    pub fn warning(&mut self, message: &str, help: &str, location: &Location) {
        self.output.warning("parsing warning", message, help, location);
    }
    pub fn message(&mut self, message: &str, help: &str, location: &Location) {
        self.output.message("parsing message", message, help, location);
    }
}

pub(crate) struct ASTGenerator {}

impl ASTGenerator {
    pub fn generate(tokens: &Vec<Token>, parser: &mut Parser) -> Vec<ASTNode> {
        let mut ast: Vec<ASTNode> = Vec::new();
        let lines = Self::split_tokens_into_lines(&tokens);

        let mut line_index = 0;
        while line_index < lines.len() {
            let tokens = lines[line_index].clone();

            let mut is_func = 0;
            let mut is_assign = false;
            for token in &tokens {
                match token.token_type {
                    TokenType::Assign => {
                        line_index += 1;
                        is_assign = true;
                        break;
                    }
                    TokenType::Colon => is_func = 1,
                    TokenType::Identifier => if is_func == 1 { is_func = 2 } else { is_func = 0 },
                    TokenType::LParen => if is_func == 2 { is_func = 3 } else { is_func = 0 },
                    TokenType::LessThan => if is_func == 2 { is_func = 3 } else { is_func = 0 },
                    _ => is_func = 0
                }
            }

            if is_assign {
                continue;
            }

            if is_func == 3 {

                line_index += 1;
                continue;
            }

            match tokens[0].token_type {
                TokenType::Identifier => {
                    match tokens[1].token_type {
                        TokenType::Colon => { // Declaration
                            parser.error("Unexpected finding", "Expected declaration to be found with patterns: '=', ': IDENT <'', or ': IDENT ('", &tokens[0].location);
                        }
                        TokenType::Assign => { // Assign Variable
                            parser.error("Unexpected finding", "Expected declaration to be found with patterns: '=', ': IDENT <'', or ': IDENT ('", &tokens[0].location);
                        }
                        TokenType::LParen => { // Function call
                            
                        }
                        TokenType::DoubleColon => { // Traverse Scope
                            
                        }
                        _ => {
                            parser.error("Unexpected token", "Unexpected token after identifier", &tokens[0].location);
                        }
                    }
                }
                _ => {
                    parser.error("Unexpected token", "Did not expect token to begin line", &tokens[0].location);
                }
            }

            line_index += 1;
        }

        ast
    }

    pub fn split_tokens_into_lines(tokens: &Vec<Token>) -> Vec<Vec<Box<Token>>> {
        tokens.split(|t| t.token_type == TokenType::EndOfLine)
            .map(|l| l.iter().map(|t| Box::new(t.clone())).collect::<Vec<Box<Token>>>())
            .filter(|x| !x.is_empty())
            .collect()
    }
}