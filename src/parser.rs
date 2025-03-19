// eventually need to remove [] and instead use get
// remove all unwrapping and replace with error handling
// go over and make sure no unneciary cloning is happening

use crate::lexer::{Token, TokenType, Location};
use crate::{ast::*, error_handling::ErrorHandling};

#[allow(unused_imports)]
use crate::debug;

pub fn parse(tokens: Vec<Token>, file: &String, code: &String) -> Vec<ASTNode> {
    let mut parser = Parser::new(Some(file.clone()), code.clone(), tokens);
    parser.generate_ast();

    parser.output.print_messages();
    parser.ast
}

#[derive(Debug, Clone)]
pub struct Parser {
    output: ErrorHandling,
    ast: Vec<ASTNode>,
    lines: Vec<Vec<Box<Token>>>,
    __curent_parsing_line: usize,
}

impl Parser {
    pub fn new(file: Option<String>, full_code: String, tokens: Vec<Token>) -> Parser {
        Parser {
            output: ErrorHandling::new(file, full_code),
            ast: Vec::new(),
            lines: Self::split_tokens_into_lines(&tokens),
            __curent_parsing_line: 0,
        }
    }

    pub fn generate_ast(&mut self) {
        self.ast = self.generate_from_tokens();

        // for testing
        for _node in self.ast.iter() {
            //Self::print_node(node);
        }
    }

    pub fn error(&mut self, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra errors
            return;
        }
        self.output.error("parsing error", message, help, location);
    }
    pub fn warning(&mut self, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra warnings
            return;
        }
        self.output.warning("parsing warning", message, help, location);
    }
    pub fn message(&mut self, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra messages
            return;
        }
        self.output.message("parsing message", message, help, location);
    }

    fn generate_from_tokens(&mut self) -> Vec<ASTNode> {
        let mut ast: Vec<ASTNode> = Vec::new();

        while self.__curent_parsing_line < self.lines.len() {
            let mut tokens = self.lines[self.__curent_parsing_line].clone();
            if let Ok(node) = self.get_ast_node(&mut tokens) {
                println!("{}", Self::node_expr_to_string(&node));
                ast.push(node);
                println!();
            }
            self.__curent_parsing_line += 1;
        }

        ast
    }

    fn get_ast_node(&mut self, tokens: &mut Vec<Box<Token>>) -> Result<ASTNode, ()> {
        if tokens.len() == 0 {
            return Err(());
        }

        match tokens[0].token_type {
            TokenType::Break => {
                return Ok(ASTNode {
                    token: tokens[0].clone(),
                    node: Box::new(NodeType::Break(tokens[0].clone())),
                });
            }
            TokenType::Continue => {
                return Ok(ASTNode {
                    token: tokens[0].clone(),
                    node: Box::new(NodeType::Continue(tokens[0].clone())),
                });
            }
            TokenType::Return => {
                let mut tokens_after_return = tokens[1..].to_vec();
                let return_node = self.get_entire_expression(&mut tokens_after_return);
                return Ok(ASTNode {
                    token: tokens[0].clone(),
                    node: Box::new(NodeType::ReturnExpression(return_node))
                });
            }
            TokenType::If | TokenType::While => {
                return Ok(ASTNode {
                    token: tokens[0].clone(),
                    node: Box::new(NodeType::While(self.parse_conditional_statement(tokens, &mut 0))),
                });
            }
            TokenType::Else => {
                self.error("Else statement is by itself", "Else statement must be after if statement: `if EXPR {} else {}`", &tokens[0].location);
                return Err(());
            }
            TokenType::For => {
                return Ok(self.parse_for(tokens));
            }
            TokenType::Match => {
                return Ok(self.parse_match(tokens, &mut 0));
            }
            _ => { } // continue
        }

        // for matching function declarations `: a < b` and `: a ( b` as `type: name(paramters)` or `type: name<types>(parameters)`
        // 0 = not declaring, 1 = declaring, 2 = found identifier, 3 = found `(` or `<` and that means declaring function.
        let mut is_declaring_func = 0;

        let mut assign_index = -1;
        for (index, token) in tokens.iter().enumerate() {
            match token.token_type {
                TokenType::Assign => { assign_index = index as i32; break; },
                TokenType::Colon => is_declaring_func = 1,
                TokenType::Identifier => if is_declaring_func == 1 { is_declaring_func = 2 } else { is_declaring_func = 0 },
                TokenType::LParen => if is_declaring_func == 2 { is_declaring_func = 3 } else { is_declaring_func = 0 },
                TokenType::LessThan => if is_declaring_func == 2 { is_declaring_func = 3 } else { is_declaring_func = 0 },
                TokenType::LBrace => break,
                _ => is_declaring_func = 0
            }
        }

        if assign_index != -1 {
            let mut lhs = tokens[0..assign_index as usize].to_vec();
            let mut rhs = tokens[assign_index as usize + 1..].to_vec();

            if lhs.len() == 1 && lhs[0].token_type == TokenType::Underscore {
                // is an underscore assignment,
                *tokens = rhs;
            } else if lhs.len() >= 2 && lhs[lhs.len() - 2].token_type == TokenType::Colon {
                // declaring variable
                let var_name: Box<Token> = lhs[lhs.len() - 1].clone();

                let mut accessing_end_index = 0;
                let mut access_modifier: Vec<AccessModifier> = Vec::new();
                for token in lhs.iter() {
                    match token.token_type {
                        TokenType::Pub => access_modifier.push(AccessModifier::Public),
                        TokenType::Priv => access_modifier.push(AccessModifier::Private),
                        TokenType::Override => access_modifier.push(AccessModifier::Override),
                        TokenType::Virtual => access_modifier.push(AccessModifier::Virtual),
                        TokenType::Static => access_modifier.push(AccessModifier::Static),
                        TokenType::Const => access_modifier.push(AccessModifier::Const),
                        TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                        TokenType::Abstract => access_modifier.push(AccessModifier::Abstract),
                        _ => break
                    }
                    accessing_end_index += 1;
                }

                let description: Option<Box<Token>> = rhs.iter().position(|x| x.token_type == TokenType::RightArrow).and_then(|arrow_index| rhs.get(arrow_index + 1).cloned());
                if let Some(des) = description.clone() {
                    if des.token_type != TokenType::StringConstant {
                        self.error("variable description must be a string", "Description must be a string. Try placing quotes arounf it", &des.location);
                    }
                }

                let mut type_tokens = lhs[accessing_end_index..lhs.len() - 2].to_vec();
                let var_type: Box<ASTNode> = self.get_type_idententifier(&mut type_tokens, &mut 0);

                let var_value: Box<ASTNode> = self.get_entire_expression(&mut rhs);

                let var_decl = VariableDeclaration {
                    access_modifier,
                    var_name: var_name.clone(),
                    description,
                    var_value,
                    var_type,
                };
                return Ok(ASTNode {
                    token: var_name.clone(),
                    node: Box::new(NodeType::VariableDeclaration(var_decl)),
                });
            }
            else {
                // assigning variable
                let mut tokens_after_return = lhs[1..].to_vec();

                if lhs.get(0).map_or(false, |t| t.token_type == TokenType::Return) {
                    let left = self.get_entire_expression(&mut tokens_after_return);
                    let right = self.get_entire_expression(&mut rhs);

                    let assign = ASTNode {
                        token: lhs[0].clone(),
                        node: Box::new(NodeType::Assignment(Assignment { left, right }))
                    };
                    return Ok(ASTNode {
                        token: lhs[0].clone(),
                        node: Box::new(NodeType::ReturnExpression(Box::new(assign)))
                    });
                }
                else {
                    let left = self.get_entire_expression(&mut lhs);
                    let right = self.get_entire_expression(&mut rhs);

                    let assign = Assignment { left, right };
                    return Ok(ASTNode{
                        token: lhs[0].clone(),
                        node: Box::new(NodeType::Assignment(assign))
                    });
                }
            }
        }

        if is_declaring_func == 3 { // declaring function
            return Err(());
        }

        // anything else
        return Ok(*self.get_entire_expression(tokens));
    }

    fn get_condition_and_body_for_if(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> (Box<ASTNode>, BodyRegion) {
        if tokens[*i].token_type != TokenType::If && tokens[*i].token_type != TokenType::While { 
            self.error("Expected `if` or `while`", "Parser error, expected `if` or `while`", &tokens[*i].location);
        } else {
            *i += 1; // skip if
        }

        let condition = self.get_expression(tokens, i, None);
        let body = self.get_code_block(tokens, i, false);
        
        return (condition, body);
    }

    fn parse_match_case(&mut self, mut pattern_tokens: Vec<Box<Token>>, mut case_lines: Vec<Vec<Box<Token>>>, err_token: &Box<Token>) -> MatchCase {
        let err = MatchCase{ body: BodyRegion { body: vec![] }, pattern: Box::new(ASTNode::err()) };

        let pattern = self.get_entire_expression(&mut pattern_tokens);

        if case_lines.len() < 1 {
            self.error("Incorrect match case grammer", "Expected match case body to have either a single line, or a braced body: `EXPR => LINE,` or `EXPR => { BODY }`", &err_token.location);
            return err
        } else if case_lines[0].len() < 1 {
            self.error("Incorrect match case grammer", "Expected match case body to have either a single line, or a braced body: `EXPR => LINE,` or `EXPR => { BODY }`", &err_token.location);
            return err
        }

        let body = if case_lines[0][0].token_type == TokenType::LBrace {
            // body { }
            let temp_lines = self.lines.clone();
            let temp_index = self.__curent_parsing_line;

            let tokens = &mut case_lines[0].clone();
            self.lines = case_lines;
            self.__curent_parsing_line = 0;
            let returns = self.get_code_block(tokens, &mut 0, false);

            self.lines = temp_lines;
            self.__curent_parsing_line = temp_index;
            
            returns
        } else {
            // line 
            if case_lines.len() > 1 {
                self.error("Incorrect match case grammer, expected single line", "Expected match case to have a single line or a body: `EXPR => LINE,` or `EXPR => { BODY }`", &case_lines[0][0].location);
                return err;
            }
            BodyRegion {
                body: vec![self.get_entire_expression(&mut case_lines[0])]
            }
        };

        MatchCase {
            pattern,
            body
        }
    }

    fn parse_match(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode {
        if tokens[0].token_type != TokenType::Match {
            self.error("Expected `match`", "Parser error, expected `match`", &tokens[0].location);
            return ASTNode::err();
        }
        let original_token = tokens[*i].clone();

        *i += 1;
        let match_value = self.get_expression(tokens, i, None);

        let mut brace_level = 0;
        let mut parenthesis_level = 0;
        let mut bracket_level = 0;
        
        let mut comma_count = 0;
        let mut semicolon_count = 0;
        let mut patterns = vec![vec![]]; // cases [ patterns [ ] ]
        let mut cases_lines_tokens = vec![vec![vec![]]]; // cases [ lines [ tokens [ ] ] ]
        let mut has_arrow_happened = false;
        let mut done = false;
        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                if has_arrow_happened {
                    cases_lines_tokens[comma_count][semicolon_count].push(tokens[*i].clone());
                } else {
                    patterns[comma_count].push(tokens[*i].clone());
                }
                match tokens[*i].token_type {
                    TokenType::LParen => parenthesis_level += 1,
                    TokenType::RParen => parenthesis_level -= 1,
                    TokenType::LBracket => bracket_level += 1,
                    TokenType::RBracket => bracket_level -= 1,
                    TokenType::LBrace => { 
                        if brace_level == 0 {
                            patterns[comma_count].pop();
                        }
                        brace_level += 1;
                    },
                    TokenType::RBrace => {
                        brace_level -= 1;
                        if brace_level == 0 {
                            cases_lines_tokens[comma_count][semicolon_count].pop();
                            done = true; 
                            // allow for: `match A { case B => C, }` *NOTICE* comma at the end
                            // if last token was a comma, remove it from the last case
                            if *i >= 1 && tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::Comma) {
                                cases_lines_tokens.pop(); 
                                patterns.pop();
                            }
                            break;
                        } else if brace_level == 1 && self.lines.get(self.__curent_parsing_line + 1).map_or(false, |l| l.get(0).map_or(false, |t| t.token_type != TokenType::Comma && t.token_type != TokenType::LBrace && t.token_type != TokenType::RBrace)) { 
                            // check the next line if it has a comma, if it does, let the comma split it, otherwise split by `}`
                            comma_count += 1;
                            semicolon_count = 0;
                            has_arrow_happened = false;
                            patterns.push(vec![]);
                            cases_lines_tokens.push(vec![vec![]]);
                        }
                    },
                    TokenType::Comma => {
                        if brace_level == 1 && parenthesis_level == 0 && bracket_level == 0 {
                            cases_lines_tokens[comma_count][semicolon_count].pop();
                            comma_count += 1;
                            semicolon_count = 0;
                            has_arrow_happened = false;
                            patterns.push(vec![]);
                            cases_lines_tokens.push(vec![vec![]]);
                        } 
                    },
                    TokenType::DoubleArrow => {
                        if brace_level == 1 && parenthesis_level == 0 && bracket_level == 0 {
                            has_arrow_happened = true;
                            patterns[comma_count].pop();
                            semicolon_count = 0;
                        }
                    }
                    _ => { }
                }
                *i += 1;
            }

            if done {
                break;
            }

            self.__curent_parsing_line += 1;
            if self.__curent_parsing_line >= self.lines.len() {
                break;
            }
            if has_arrow_happened {
                semicolon_count += 1;
                cases_lines_tokens[comma_count].push(vec![]);
            }
            *i = 0;
            *tokens = self.lines[self.__curent_parsing_line].clone();
        }
        
        if cases_lines_tokens.len() == 1 && cases_lines_tokens[0].len() == 1 && cases_lines_tokens[0][0].len() == 0 {
            cases_lines_tokens = vec![];
        }

        let mut match_cases = vec![];
        for index in 0..cases_lines_tokens.len() {
            match_cases.push(self.parse_match_case(patterns[index].clone(), cases_lines_tokens[index].clone(), &original_token));
        }

        ASTNode {
            token: original_token,
            node: Box::new(NodeType::Match(MatchRegion {
                match_value,
                match_cases
            }))
        }
    }

    fn parse_for_statement(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, index_segment: Option<Box<Token>>, mut set_tokens: Vec<Box<Token>>, mut condition_tokens: Vec<Box<Token>>, mut increment_tokens: Vec<Box<Token>>) -> ForLoop {
        let set_segment = Box::new(self.get_ast_node(&mut set_tokens).unwrap_or_else(|_| {
            self.error("Error with for loop set segment", "For loop does not have set segment. An example would be: `for i32: i = 0, i < 10, i += 1 { ... }`", &set_tokens[0].location);
            ASTNode::err()
        }));
        let condition_segment = self.get_entire_expression(&mut condition_tokens);
        let increment_segment = self.get_entire_expression(&mut increment_tokens);
        let body = self.get_code_block(tokens, i, false);
        
        ForLoop {
            index_segment,
            set_segment,
            condition_segment,
            increment_segment,
            body
        }
    }

    fn parse_foreach_statement(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, index_segment: Option<Box<Token>>, for_tokens: Vec<Box<Token>>) -> ForEachLoop {
        let mut name_tokens = vec![];
        let mut in_tokens = vec![];
        
        let mut in_has_happenend = false;
        for token in for_tokens {
            match token.token_type {
                TokenType::In => in_has_happenend = true,
                _ => {
                    if in_has_happenend {
                        in_tokens.push(token.clone());
                    }
                    else {
                        name_tokens.push(token.clone());
                    }
                }
            }
        }

        let iter_value = self.get_entire_expression(&mut name_tokens);
        let iter_range = self.get_entire_expression(&mut in_tokens);

        let body = self.get_code_block(tokens, i, false);

        ForEachLoop {
            iter_value,
            iter_range,
            index_segment,
            body
        }
    }

    fn parse_for(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        if tokens[0].token_type != TokenType::For {
            self.error("Expected `for`", "Parser error, expected `for`", &tokens[0].location);
            return ASTNode::err();
        }

        let mut segment_tokens = vec![vec![]];
        let mut comma_index = 0;
        let mut parenthesis_level = 0;
        let mut bracket_level = 0;
        let mut brace_level = 0;

        let mut i = 1;
        while i < tokens.len() {
            segment_tokens[comma_index].push(tokens[i].clone());
            match tokens[i].token_type {
                TokenType::LParen => parenthesis_level += 1,
                TokenType::RParen => parenthesis_level -= 1,
                TokenType::LBracket => bracket_level += 1,
                TokenType::RBracket => bracket_level -= 1,
                TokenType::LBrace => {
                    brace_level += 1;
                    if brace_level == 1 && parenthesis_level == 0 && bracket_level == 0 {
                        segment_tokens[comma_index].pop();
                        break;
                    }
                }
                TokenType::RBrace => brace_level -= 1,
                TokenType::Comma => {
                    if parenthesis_level == 0 && bracket_level == 0 && brace_level == 0 {
                        segment_tokens[comma_index].pop(); // remove the comma
                        comma_index += 1;
                        segment_tokens.push(vec![]);
                    }
                }
                _ => {},
            }
            i += 1;
        }

        if segment_tokens.len() == 0 {
            self.error("Could not parse for statement", "No parameters were found for the for statement: `for X in Y { ... }` or `for VAR, CONDITION, INC { ... }`", &tokens[i].location);
            return ASTNode::err();
        } else if segment_tokens.len() == 1 {
            // for X in Y
            let node = self.parse_foreach_statement(tokens, &mut i, None, segment_tokens[0].clone());
            return ASTNode { 
                token: tokens[0].clone(), 
                node: Box::new(NodeType::ForEach(node)) 
            };
        } else if segment_tokens.len() == 2 {
            // for index, X in Y
            let index_token = if segment_tokens[0].len() != 1 {
                self.error("Could not parse for statement", "The first parameter of the for statement must be a single identifier: `for index, X in Y { ... }`", &tokens[i].location);
                None
            } else {
                Some(segment_tokens[0][0].clone())
            };
            let node = self.parse_foreach_statement(tokens, &mut i, index_token, segment_tokens[1].clone());
            return ASTNode { 
                token: tokens[0].clone(), 
                node: Box::new(NodeType::ForEach(node)) 
            };
        } else if segment_tokens.len() == 3 {
            // for VAR, CONDITION, INC
            let node = self.parse_for_statement(tokens, &mut i, None, segment_tokens[0].clone(), segment_tokens[1].clone(), segment_tokens[2].clone());
            return ASTNode { 
                token: tokens[0].clone(), 
                node: Box::new(NodeType::For(node)) 
            };
        } else if segment_tokens.len() == 4 {
            // for index, VAR, CONDITION, INC
            let index_token = if segment_tokens[0].len() != 1 {
                self.error("Could not parse for statement", "The first parameter of the for statement must be a single identifier: `for index, X in Y { ... }`", &tokens[i].location);
                None
            } else {
                Some(segment_tokens[0][0].clone())
            };
            let node = self.parse_for_statement(tokens, &mut i, index_token, segment_tokens[1].clone(), segment_tokens[2].clone(), segment_tokens[3].clone());
            return ASTNode { 
                token: tokens[0].clone(), 
                node: Box::new(NodeType::For(node)) 
            };
        } else {
            self.error("Could not parse for statement", "Too many parameters were found for the for statement: `for X in Y { ... }` or `for VAR, CONDITION, INC { ... }`", &tokens[i].location);
            return ASTNode::err();
        }
    }

    fn parse_conditional_statement(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ConditionalRegion {
        let is_while = tokens[*i].token_type == TokenType::While;
        let (condition, body) = self.get_condition_and_body_for_if(tokens, i);

        let mut else_region: Option<BodyRegion> = None;
        let mut else_if_regions: Option<Vec<Box<ConditionalRegion>>> = None;

        *i += 1;
        while tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Else) {
            *i += 1;
            if tokens.get(*i).is_some() && (tokens[*i].token_type == TokenType::If || tokens[*i].token_type == TokenType::While) {
                // else if 
                let is_while = tokens[*i].token_type == TokenType::While;
                let (else_if_condition, else_if_body) = self.get_condition_and_body_for_if(tokens, i);
                
                if else_if_regions.is_none() {
                    else_if_regions = Some(vec![]);
                }

                else_if_regions.as_mut().unwrap().push(Box::new(ConditionalRegion {
                    body: else_if_body,
                    condition: else_if_condition,
                    else_region: None,
                    else_if_regions: None,
                    is_while
                }));

                *i += 1;
            } else if else_region.is_none() {
                // else
                else_region = Some(self.get_code_block(tokens, i, false));
                *i += 1;
            } else {
                // double else
                self.error("Else statement is by itself", "Else statement must be after if statement: `if EXPR {} else {}`", &tokens[*i - 1].location);
                break;
            }
        }

        ConditionalRegion {
            condition,
            body,
            else_region,
            else_if_regions,
            is_while
        }
    }

    fn function_call(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, last_punc: Option<ScopeType>) -> ScopedIdentifier {
        let mut scope = scope;
        *i -= 1;

        let mut node_parameters: Vec<Box<ASTNode>> = vec![];
        let mut type_parameters: Vec<Box<ASTNode>> = vec![];
        let name = tokens[*i].clone();

        if let Some(next_token) = tokens.clone().get(*i + 1) {
            if next_token.token_type == TokenType::LessThan {
                *i += 1;
                type_parameters = self.get_type_parameters(tokens, i).parameters;
                *i -= 1;
            }

            if tokens.get(*i + 1).is_some() && tokens[*i + 1].token_type == TokenType::LParen {
                *i += 1;
                node_parameters = self.get_parameters_for_function(tokens, i);
            }
            else {
                self.error("Invalid token for funciton call", format!("Expected either `<` or `(` for function call, got: `{}`", next_token.value).as_str(), &tokens[*i].location);
            }
        }

        let function_call = FunctionCall {
            parameters: NodeParameters { parameters: node_parameters },
            name: name.clone(),
            type_parameters: type_parameters.is_empty().then(|| None).unwrap_or(Some(type_parameters)),
        };

        let identifier = Identifier {
            expression: Box::new(ASTNode {
                token: name.clone(),
                node: Box::new(NodeType::FunctionCall(function_call.clone())),
            }),
            scope_type: last_punc,
            type_parameters: None
        };

        if let Some(s) = scope.as_mut() {
            s.scope.push(identifier.clone());
        }

        let mut scope = scope.unwrap_or(ScopedIdentifier {
            scope: vec![identifier],
        });

        if tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::Dot { // is chaining
            /*  I've changed the way function chaining works:
                Now I will store the scope as a vector of identifiers, where the first one is the root and every index is apart of the scope or chain.
                The new `Identifier` struct will have an optional `expression` field, which will be the expression that is chained to the identifier.
                This will allow for a more dynamic way of chaining, where the scope can be changed at any point in the chain.
            */
            *i += 1;

            let chained_expression = self.get_expression(tokens, i, None);

            scope.scope.push(Identifier {
                expression: chained_expression.clone(),
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });

            scope
        }
        else if tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::LBracket { // is indexing
            scope = self.get_indexer_expression(tokens, Some(scope), i);
            *i += 1;
            scope
        }
        else {
            /*if *i >= 1 {
                *i -= 1;
            }*/
            scope
        }
    }

    fn scope_call(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedIdentifier {
        *i -= 1;

        let mut last_punc = None;
        let (mut scope, valid_lt_as_type_parameter) = self.get_ident_scope(tokens, i, &mut last_punc); // `valid_lt_as_type_parameter`
        *i += 1;

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) && valid_lt_as_type_parameter {
            scope = self.function_call(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::Identifier) && !tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope.scope.push(Identifier {
                scope_type: last_punc,
                expression: Box::new(ASTNode {
                    token: tokens[*i - 1].clone(),
                    node: Box::new(NodeType::Identifier(tokens[*i - 1].clone())),
                }),
                type_parameters: None
            });
        }

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope = self.get_indexer_expression(tokens, Some(scope), i)
        }

        if *i >= 1 {
            *i -= 1;
        }
        return scope
    }

    fn scope_call_with_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, mut scope: ScopedIdentifier, last_punc: Option<ScopeType>) -> ScopedIdentifier {
        *i += 1;

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) {
            scope = self.function_call(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::Dot) && tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && (tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen) || tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LessThan)) {
            *i += 1;
            scope = self.function_call(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && !tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope.scope.push(Identifier {
                scope_type: last_punc,
                expression: Box::new(ASTNode {
                    token: tokens[*i].clone(),
                    node: Box::new(NodeType::Identifier(tokens[*i].clone())),
                }),
                type_parameters: None
            });
            *i += 1;
        }

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope = self.get_indexer_expression(tokens, Some(scope), i)
        }

        if *i >= 1 {
            *i -= 1;
        }
        return scope
    }
    
    fn get_ternary(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> NodeType {
        #[derive(PartialEq)]
        enum TernaryState {
            Condition,
            Then,
            Else,
        }
    
        struct TernaryType {
            condition: Vec<Box<Token>>,
            then: Vec<Box<Token>>,
            else_then: Vec<Box<Token>>,
        }
    
        let original_i = *i;
    
        let mut ternary_state = TernaryState::Condition;
        let mut ternary_tokens = TernaryType {
            condition: vec![],
            then: vec![],
            else_then: vec![],
        };
    
        let mut delimiter_level = 0;
        let mut question_mark_level = 0;
    
        let mut current_tokens: Vec<Box<Token>> = vec![];
    
        while *i < tokens.len() {
            match tokens[*i].token_type {
                TokenType::LParen | TokenType::LBrace | TokenType::LBracket => {
                    let start_delimiter = tokens[*i].token_type.clone();
                    let end_delimiter = match start_delimiter {
                        TokenType::LParen => TokenType::RParen,
                        TokenType::LBrace => TokenType::RBrace,
                        TokenType::LBracket => TokenType::RBracket,
                        _ => unreachable!(),
                    };
    
                    let mut insides = vec![];
                    Self::get_tokens_in_delimeter(tokens, i, start_delimiter, end_delimiter, &mut insides);

                    for token in insides {
                        current_tokens.push(token);
                    }
                }
                TokenType::QuestionMark => {
                    question_mark_level += 1;
                    if delimiter_level == 0 && ternary_state == TernaryState::Condition {
                        ternary_tokens.condition = current_tokens;
                        current_tokens = vec![];
                        ternary_state = TernaryState::Then;
                    } else {
                        current_tokens.push(tokens[*i].clone());
                    }
                }
                TokenType::Colon => {
                    question_mark_level -= 1;
                    if delimiter_level == 0 && question_mark_level == 0 && ternary_state == TernaryState::Then {
                        ternary_tokens.then = current_tokens;
                        current_tokens = vec![];
                        ternary_state = TernaryState::Else;
                    } else {
                        current_tokens.push(tokens[*i].clone());
                    }
                }
                TokenType::RBrace | TokenType::RBracket | TokenType::RParen => {
                    if delimiter_level == 0 {
                        if *i > 0 {
                            *i -= 1;
                        }
                        break;
                    }
                    delimiter_level -= 1;
                    current_tokens.push(tokens[*i].clone());
                }
                _ => {
                    current_tokens.push(tokens[*i].clone());
                }
            }
    
            if delimiter_level < 0 {
                self.error("Unmatched delimiters", "Mismatched parentheses, braces, or brackets in ternary expression.", &tokens[original_i].location);
                return NodeType::None;
            }
    
            *i += 1;
        }
    
        if ternary_state != TernaryState::Else {
            self.error("Incomplete ternary expression", "Expected `:` to complete the ternary expression: `a ? b : c`", &tokens[original_i].location);
            return NodeType::None;
        }
    
        ternary_tokens.else_then = current_tokens;
    
        let condition = self.get_entire_expression(&mut ternary_tokens.condition);
        let then = self.get_entire_expression(&mut ternary_tokens.then);
        let else_then = self.get_entire_expression(&mut ternary_tokens.else_then);
    
        let ternary = TernaryConditional {
            condition,
            then,
            else_then,
        };
    
        NodeType::TernaryOperator(ternary)
    }
    
    fn get_indexer_expression(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize) -> ScopedIdentifier {
        let array_nodes = self.array_nodes(tokens, i);

        if scope.is_none() {
            self.error("Couldn't parse indexing", "Expected an indexer, but no object was provided for the index", &tokens[*i].location);
        }

        let mut scope = scope.unwrap();
        let pop_wrapped = scope.scope.pop();

        if pop_wrapped.is_none() {
            self.error("Couldn't parse indexing", "Expected an indexer, but no object was provided for the index", &tokens[*i].location);
        }
        let pop = pop_wrapped.unwrap();
        let expression = pop.expression.clone();

        let indexing = IndexingExpression {
            object: expression,
            index: array_nodes,
        };

        let expression = Box::new(ASTNode {
            token: pop.expression.token,
            node: Box::new(NodeType::Indexer(indexing))
        });

        let scope_type = pop.scope_type;

        let identifier = Identifier { 
            expression, 
            scope_type, 
            type_parameters: None 
        };
        let mut went_through_lbracket = false;

        scope.scope.push(identifier);
        *i += 1;

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope = self.get_indexer_expression(tokens, Some(scope), i);
            went_through_lbracket = true;
        }
        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
            *i += 2;
            let call = self.scope_call(tokens, i);
            let mut push = vec![];
            for (i, c) in call.scope.iter().enumerate() {
                if i == 0 {
                    push.push(Identifier {
                        expression: c.expression.clone(),
                        scope_type: Some(ScopeType::Dot),
                        type_parameters: None
                    });
                } else {
                    push.push(Identifier {
                        expression: c.expression.clone(),
                        scope_type: c.scope_type.clone(),
                        type_parameters: None
                    });
                }
            }
            for p in push {
                scope.scope.push(p);
            }
        }
        else if !went_through_lbracket {
            if *i >= 1 {
                *i -= 1;
            }
        }

        ScopedIdentifier {
            scope: scope.scope
        }
    }

    fn get_array_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedIdentifier {
        let first_token = tokens[*i].clone();
        let array_nodes = self.array_nodes(tokens, i);

        // Update the last identifier's expression if possible
        let mut scope = ScopedIdentifier {
            scope: vec![Identifier {
                expression: Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::ArrayExpression(NodeParameters { parameters: array_nodes.clone() })),
                }),
                scope_type: None,
                type_parameters: None
            }],
        };

        // Handle chaining if the next token is a dot
        if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
            *i += 2;
            let chained_expression = self.get_expression(tokens, i, None);
            scope.scope.push(Identifier {
                expression: chained_expression,
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });
            if *i >= 1 {
                *i -= 1;
            }
        }

        scope
    }

    fn array_nodes(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Box<ASTNode>> {
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        let mut comma_vec_index = 0;
        let mut parenthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;

        // this will go up if it finds `<` and will increase as many times there are `<` before there is the `>`. This is slightly different then the other ones
        let mut last_was_comma = 0;
        let mut angle_bracket_level_count = 0;

        while *i < tokens.len() {
            last_was_comma -= 1;
            match tokens[*i].token_type {
                TokenType::LBracket => {
                    bracket_level += 1;
                    if bracket_level > 1 {
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                }
                TokenType::RBracket => {
                    bracket_level -= 1;
                    if bracket_level == 0 && brace_level == 0 && parenthesis_level == 0 && angle_bracket_level_count <= 0 {
                        if last_was_comma > 0 {
                            all_tokens.pop();
                        }
                        break;
                    }
                    else {
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                }
                TokenType::Comma => {
                    if bracket_level == 1 && parenthesis_level == 0 && brace_level == 0 && angle_bracket_level_count <= 0 {
                        last_was_comma = 2;
                        all_tokens.push(vec![]);
                        comma_vec_index += 1;
                    }
                    else {
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                }
                TokenType::LessThan => {
                    if angle_bracket_level_count <= 0 {
                        let mut j = *i;
                        let mut temp_angle_bracket_level = 0;
                        let mut highest = 0;
                        let mut is_angle_bracket = true; // if is operator: `X < Y` or `Type<T>`
                        while j < tokens.len() {
                            match tokens[j].token_type {
                                TokenType::GreaterThan => {
                                    temp_angle_bracket_level -= 1;
                                    if temp_angle_bracket_level == 0 {
                                        if tokens.get(j + 1).is_some().then(|| tokens[j + 1].token_type != TokenType::LParen && tokens[j + 1].token_type != TokenType::DoubleColon).unwrap_or(false) {
                                            is_angle_bracket = false;
                                        }
                                        break;
                                    }
                                }
                                TokenType::LessThan => {
                                    temp_angle_bracket_level += 1;
                                    highest = temp_angle_bracket_level;
                                }
                                _ => {
                                    if tokens[j].token_type.is_operator() {
                                        is_angle_bracket = false;
                                    }
                                }

                            }
                            j += 1;
                        }
                        if is_angle_bracket && temp_angle_bracket_level == 0 {
                            angle_bracket_level_count = highest;
                        }
                    }
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                TokenType::GreaterThan => {
                    if angle_bracket_level_count > 0 {
                        angle_bracket_level_count -= 1;
                    }
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                TokenType::LBrace => {
                    brace_level += 1;
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                TokenType::RBrace => {
                    brace_level -= 1;
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                TokenType::LParen => {
                    parenthesis_level += 1;
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                TokenType::RParen => {
                    parenthesis_level -= 1;
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
                _ => {
                    all_tokens[comma_vec_index].push(tokens[*i].clone());
                }
            }
            *i += 1;
        }

        let mut array_nodes = vec![];
        for token_vec in all_tokens {
            let mut token_vec= token_vec.clone();
            let node = self.get_entire_expression(&mut token_vec);
            array_nodes.push(node);
        }

        array_nodes
    }

    fn get_object_instantiation(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, last_punc: Option<ScopeType>) -> ScopedIdentifier {
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        let mut comma_vec_index = 0;
        let mut parenthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;
        let name_token = tokens[*i - 1].clone();

        let mut last_was_comma = 0;
        let mut angle_bracket_level_count = 0;

        let mut done = false;
        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                last_was_comma -= 1;
                match tokens[*i].token_type {
                    TokenType::LBracket => {
                        bracket_level += 1;
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    TokenType::RBracket => {
                        bracket_level -= 1;
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    TokenType::LParen => {
                        parenthesis_level += 1;
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    TokenType::RParen => {
                        parenthesis_level -= 1;
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    TokenType::LBrace => {
                        brace_level += 1;
                        if brace_level > 1 {
                            all_tokens[comma_vec_index].push(tokens[*i].clone());
                        }
                    }
                    TokenType::RBrace => {
                        brace_level -= 1;
                        if bracket_level == 0 && brace_level == 0 && parenthesis_level == 0 && angle_bracket_level_count <= 0 {
                            if last_was_comma > 0 {
                                all_tokens.pop();
                            }
                            done = true;
                            break;
                        }
                        else {
                            all_tokens[comma_vec_index].push(tokens[*i].clone());
                        }
                    }
                    TokenType::Comma => {
                        if brace_level == 1 && parenthesis_level == 0 && bracket_level == 0 {
                            all_tokens.push(vec![]);
                            comma_vec_index += 1;
                            last_was_comma = 2;
                        }
                        else {
                            all_tokens[comma_vec_index].push(tokens[*i].clone());
                        }
                    }
                    TokenType::LessThan => {
                        if angle_bracket_level_count <= 0 {
                            let mut j = *i;
                            let mut temp_angle_bracket_level = 0;
                            let mut highest = 0;
                            let mut is_angle_bracket = true;
                            while j < tokens.len() {
                                match tokens[j].token_type {
                                    TokenType::GreaterThan => {
                                        temp_angle_bracket_level -= 1;
                                        if temp_angle_bracket_level == 0 {
                                            if tokens.get(j + 1).is_some().then(|| tokens[j + 1].token_type != TokenType::LParen && tokens[j + 1].token_type != TokenType::DoubleColon).unwrap_or(false) {
                                                is_angle_bracket = false;
                                            }
                                            break;
                                        }
                                    }
                                    TokenType::LessThan => {
                                        temp_angle_bracket_level += 1;
                                        highest = temp_angle_bracket_level;
                                    }
                                    _ => {
                                        if tokens[j].token_type.is_operator() {
                                            is_angle_bracket = false;
                                        }
                                    }

                                }
                                j += 1;
                            }
                            if is_angle_bracket && temp_angle_bracket_level == 0 {
                                angle_bracket_level_count = highest;
                            }
                        }
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    TokenType::GreaterThan => {
                        if angle_bracket_level_count > 0 {
                            angle_bracket_level_count -= 1;
                        }
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                    _ => {
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                }
                *i += 1;
            }

            if done {
                break;
            }
            
            self.__curent_parsing_line += 1;
            if self.__curent_parsing_line >= self.lines.len() {
                break;
            }

            *i = 0;
            *tokens = self.lines[self.__curent_parsing_line].clone();
        }

        let mut properties: Vec<NodeProperty> = vec![];

        for prop_tokens in all_tokens {
            if prop_tokens.len() == 0 {
                continue;
            }
            let mut prop_tokens = prop_tokens.clone();
            let prop_name = prop_tokens[0].clone();
            if prop_tokens.len() <= 2 {
                self.error("Property has no value", "Property must have a value: `name = value`", &prop_name.location);
                continue;
            }
            prop_tokens.remove(0); // remove name
            prop_tokens.remove(0); // remove `=`
            let prop_value = self.get_entire_expression(&mut prop_tokens);
            properties.push(NodeProperty {
                name: prop_name,
                value: prop_value,
            });
        }

        let object_instantiation = ObjectInstantiation {
            properties,
            object_type: Box::new(ASTNode {
                token: name_token.clone(),
                node: Box::new(NodeType::Identifier(name_token.clone()))
            }),
        };

        let mut scope = scope.unwrap_or(ScopedIdentifier {
            scope: vec![]
        });

        scope.scope.push(Identifier {
            expression: Box::new(ASTNode {
                token: name_token,
                node: Box::new(NodeType::ObjectInstantiation(object_instantiation)),
            }),
            scope_type: last_punc,
            type_parameters: None
        });

        if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) || self.lines.get(self.__curent_parsing_line + 1).map_or(false, |l| l.get(0).map_or(false, |t| t.token_type == TokenType::Dot)) {
            if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                *i += 2;
            }
            else {
                *i = 1;
                self.__curent_parsing_line += 1;
                *tokens = self.lines[self.__curent_parsing_line].clone();
            }
            let chained_expression = self.get_expression(tokens, i, None);
            scope.scope.push(Identifier {
                expression: chained_expression,
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });

            if *i >= 1 {
                *i -= 1;
            }
            scope
        } else {
            scope
        }
    }

    fn check_and_return_tuple(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> (bool, Box<ASTNode>) {
        let first_token = tokens[*i].clone();
        let mut j = i.clone();
        let current_line = self.__curent_parsing_line;
        let current_output = self.output.messages.clone();

        let nodes = self.get_parameters_for_function(tokens, &mut j);
        let is_tuple = nodes.len() > 1;

        let tuple = Box::new(ASTNode {
            token: first_token,
            node: Box::new(NodeType::TupleExpression(NodeParameters {
                parameters: nodes
            })),
        });

        if is_tuple {
            *i = j;
        } else {
            self.__curent_parsing_line = current_line;
            *tokens = self.lines[self.__curent_parsing_line].clone();
            self.output.messages = current_output;
        }

        (is_tuple, tuple)
    }

    fn get_code_block(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, last_is_return: bool) -> BodyRegion {
        let mut brace_level = 0;
        let mut body = vec![];
        let mut semicolon_count = 0;
        let mut current_tokens = vec![vec![]];
        let original_token = tokens.get(*i).unwrap_or(&tokens[0]).clone();

        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                match tokens[*i].token_type {
                    TokenType::LBrace => {
                        if brace_level == 0 {
                            brace_level += 1;
                        } else {
                            current_tokens[semicolon_count].push(tokens[*i].clone());
                        } 
                    }
                    TokenType::RBrace => {
                        brace_level -= 1;
                        if brace_level == 0 {
                            if let Ok(node) = self.get_ast_node(&mut current_tokens[semicolon_count]) {
                                body.push((Box::new(node), self.__curent_parsing_line));
                            }
                            if last_is_return {
                                if let Some(last) = body.last_mut() {
                                    if let NodeType::ReturnExpression(_) = last.0.node.as_ref() {
                                        // nothing
                                    } else {
                                        last.0.node = Box::new(NodeType::ReturnExpression(Box::new(ASTNode {
                                            token: tokens[*i].clone(),
                                            node: last.0.as_ref().node.clone(),
                                        })));
                                    }
                                }
                            }

                            // sort by line number
                            body.sort_by(|a, b| a.1.cmp(&b.1));
                            
                            return BodyRegion { body: body.iter().map(|b| b.0.clone()).collect() };
                        }
                        else {
                            current_tokens[semicolon_count].push(tokens[*i].clone());
                        }
                    }
                    _ => {
                        current_tokens[semicolon_count].push(tokens[*i].clone());
                    }
                }
                *i += 1;
            }

            if let Ok(node) = self.get_ast_node(&mut current_tokens[semicolon_count]) {
                body.push((Box::new(node), self.__curent_parsing_line));
            }
            current_tokens.push(vec![]);
            semicolon_count += 1;

            self.__curent_parsing_line += 1;
            *i = 0;
            if self.__curent_parsing_line >= self.lines.len() {
                break;
            }
            *tokens = self.lines[self.__curent_parsing_line].clone();
        }

        self.error("No end of Body Region", "Code block does not have ending brace `{ ... }` ", &tokens.get(*i).unwrap_or(&original_token).location);
        return BodyRegion { body: body.iter().map(|b| b.0.clone()).collect() };
    }

    fn get_entire_expression(&mut self, tokens: &mut Vec<Box<Token>>) -> Box<ASTNode> {
        let mut __ = usize::MAX;
        self.get_expression(tokens, &mut __, None)
    }

    const PARSRING_FOR_FUNCTION: u8 = 1;
    const PARSING_FOR_FUNCTION_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::RParen];
    const PARSRING_FOR_INSIDE_PARENTHESIS: u8 = 2;
    const PARSRING_FOR_INSIDE_PARENTHESIS_UNTIL: [TokenType; 1] = [TokenType::RParen];

    fn get_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, until: Option<u8>) -> Box<ASTNode> {
        let mut expr_stack: Vec<(Box<ASTNode>, usize)> = Vec::new();
        let mut op_stack: Vec<(Box<Token>, usize)> = Vec::new();
        let mut last_was_ident = false;
        let mut last_was_unary_operator: bool = false;
        let mut paran_index = 0;
        let mut is_1_expression = until.is_none();
        let start_index = *i;
        
        if *i == usize::MAX {
            is_1_expression = false;
            *i = 0;
        }
        let mut starting_index_for_ternary = *i;

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            let mut inc_i = true;

            if let Some(until) = until.clone() {
                if Self::PARSRING_FOR_FUNCTION == until && Self::PARSING_FOR_FUNCTION_UNTIL.iter().any(|t| t == &token.token_type) {
                    if *i > 0 {
                        *i -= 1;
                    }
                    break;
                } else if Self::PARSRING_FOR_INSIDE_PARENTHESIS == until && Self::PARSRING_FOR_INSIDE_PARENTHESIS_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                }
            }

            if token.token_type == TokenType::RightArrow {
                break;
            }
            else if token.token_type == TokenType::QuestionMark {
                // ternary operator a ? b : c
                *i = starting_index_for_ternary;
                for op in op_stack.clone() {
                    if op.1 >= *i {
                        op_stack.remove(op_stack.iter().position(|x| x.1 == op.1).unwrap_or_else(|| {
                            self.error("Couldn't parse expression", "Operator stack is empty in ternary expression", &token.location);
                            return 0;
                        }));
                    }
                }
                for expr in expr_stack.clone() {
                    if expr.1 >= *i {
                        expr_stack.remove(expr_stack.iter().position(|x| x.1 == expr.1).unwrap_or_else(|| {
                            self.error("Couldn't parse expression", "Expression stack is empty in ternary expression", &token.location);
                            return 0;
                        }));
                    }
                }

                let ternary = self.get_ternary(tokens, i);
                starting_index_for_ternary = *i;
                expr_stack.push((Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(ternary),
                }), *i));
            }
            else if token.token_type == TokenType::Identifier {
                last_was_unary_operator = false;
                if tokens.get(*i + 1).is_some() && (tokens[*i + 1].token_type == TokenType::DoubleColon || tokens[*i + 1].token_type == TokenType::LBrace || tokens[*i + 1].token_type == TokenType::Dot || tokens[*i + 1].token_type == TokenType::LParen || tokens[*i + 1].token_type == TokenType::LessThan) && !until.clone().map_or(false, |t| (t == Self::PARSRING_FOR_FUNCTION && Self::PARSING_FOR_FUNCTION_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type) || t == Self::PARSRING_FOR_INSIDE_PARENTHESIS && Self::PARSRING_FOR_INSIDE_PARENTHESIS_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type))) {
                    if tokens[*i + 1].token_type == TokenType::LessThan {
                        let mut j = *i;
                        let mut angle_bracket_level = 0;
                        while j < tokens.len() {
                            if tokens[j].token_type == TokenType::LessThan {
                                angle_bracket_level += 1;
                            }
                            else if tokens[j].token_type == TokenType::GreaterThan {
                                angle_bracket_level -= 1;
                                if angle_bracket_level == 0 {
                                    last_was_ident = tokens.get(j + 1).map_or(false, |t| t.token_type == TokenType::LParen || tokens[j + 1].token_type == TokenType::DoubleColon || tokens[j + 1].token_type == TokenType::Dot);
                                    break;
                                }
                            }
                            j += 1;
                        }
                        if !last_was_ident {
                            let node = Box::new(ASTNode {
                                token: token.clone(),
                                node: Box::new(NodeType::Identifier(token.clone())),
                            });
                            expr_stack.push((node, *i));
                        }
                    }
                    else {
                        last_was_ident = true;
                    }
                }
                else {
                    if let Some(check_token) = tokens.get(*i + 1) {
                        if check_token.token_type == TokenType::LBracket {
                            last_was_ident = true;
                        }
                    }
                    let node = Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::Identifier(token.clone())),
                    });
                    expr_stack.push((node, *i));
                }
            }
            else if token.token_type.is_operator() {
                // To check for unary operators, the operator must be the first token in the expression, or the previous token must be an operator: `-0` or `0+-1`
                let mut handle_as_operator = true;
                if last_was_ident && token.token_type == TokenType::LessThan && tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::Identifier) {
                    // part of function call: function<TYPE>()
                    expr_stack.push((Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i))),
                    }), *i));
                    handle_as_operator = false;
                }
                else if last_was_unary_operator {
                    handle_as_operator = false;
                }
                else if token.token_type.is_unary_operator() && tokens.get(*i + 1).is_some().then(|| !tokens[*i + 1].token_type.is_operator()).unwrap_or(false) {
                    handle_as_operator = false;
                    if *i == 0 || (is_1_expression && *i == start_index) {
                        last_was_unary_operator = false;

                        *i += 1;
                        let next_expression = self.get_expression(tokens, i, None);
                        *i -= 1;

                        let unary = UnaryExpression {
                            operator: token.clone(),
                            operand: next_expression.clone(),
                        };

                        expr_stack.push((Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::UnaryOperator(unary)),
                        }), *i));
                    }
                    else if tokens.get(*i - 1).is_some().then(|| tokens[*i - 1].token_type.is_operator() || tokens[*i - 1].token_type == TokenType::LParen || tokens[*i - 1].token_type == TokenType::LBracket || tokens[*i - 1].token_type == TokenType::Comma || tokens[*i - 1].token_type == TokenType::LBrace).unwrap_or(false) {
                        last_was_unary_operator = false;

                        *i += 1;
                        let next_expression = self.get_expression(tokens, i, None);
                        *i -= 1;

                        let unary = UnaryExpression {
                            operator: token.clone(),
                            operand: next_expression.clone(),
                        };

                        expr_stack.push((Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::UnaryOperator(unary)),
                        }), *i));
                    }
                    else {
                        handle_as_operator = true;
                    }
                } 
                else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::As || t.token_type == TokenType::Is) {
                    *i += 1;
                    let ty = self.get_type_idententifier(tokens, i);
                    *i -= 1;
                    expr_stack.push((ty, *i));
                } 

                last_was_ident = false;
                if handle_as_operator {
                    last_was_unary_operator = false;
                    while let Some(top_op) = op_stack.last() {
                        if token.token_type.precedence() <= top_op.0.token_type.precedence() {
                            let operator = op_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "Operator stack is empty", &token.location);
                                return (Box::new(Token::new_empty()), 0);
                            }).0;
                            let right = expr_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return (Box::new(ASTNode::err()), 0);
                            }).0;
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return (Box::new(ASTNode::err()), 0);
                            }).0;

                            let node: Box<ASTNode> = Box::new(ASTNode {
                                token: operator.clone(),
                                node: Box::new(NodeType::Operator(Expression{
                                    left,
                                    right,
                                    operator
                                })),
                            });

                            expr_stack.push((node, *i));
                        } else {
                            break;
                        }
                    }
                    op_stack.push((token.clone(), *i));
                }
            }
            else if token.token_type == TokenType::DoubleColon {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push((Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i))),
                    }), *i));
                }
                else {
                    self.error("Expected identifier before double colon", "Expected identifier before double colon: `IDENT::IDENT`", &token.location);
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::Dot {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push((Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i))),
                    }), *i));
                }
                else {
                    self.error("Expected identifier before dot", "Expected a correct expression before scoping: `expression.member`", &token.location);
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::LParen {
                last_was_unary_operator = false;
                if last_was_ident {
                    last_was_ident = false;
                    // part of function call
                    expr_stack.push((Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i))),
                    }), *i));
                }
                else {
                    let (is_tuple, tuple) = self.check_and_return_tuple(tokens, i);
                    if is_tuple { // is a tuple
                        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
                            // scope traversal
                            let tuple_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: tuple,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };

                            let scope = self.scope_call_with_scope(tokens, i, tuple_scope, Some(ScopeType::Dot));

                            expr_stack.push((Box::new(ASTNode {
                                token: tokens[*i - 1].clone(),
                                node: Box::new(NodeType::ScopedExpression(scope)),
                            }), *i));
                        }
                        else {
                            expr_stack.push((tuple, *i));
                        }

                        last_was_ident = false;
                    }
                    else { // not a tuple
                        // if not then it is apart of expression 
                        *i += 1;

                        // this parses weird
                        // function( (a + b).c )   parses as:   function( (a + b) ).c
                        // this is because it parses until `)` and then removes 1 from the `i`.
                        // so `i` would be at `b`. So when it checks for `i + 1` it would be at `)` and not `.`
                        // to fix this, we need to instead use an `until` parsing, use a enum where it would parse for function or for expression

                        let until = Some(Self::PARSRING_FOR_INSIDE_PARENTHESIS);
                        let node = self.get_expression(tokens, i, until);

                        if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                            // scope traversal
                            *i += 1;
                            let parenthesis_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: node,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };

                            let scope = self.scope_call_with_scope(tokens, i, parenthesis_scope, Some(ScopeType::Dot));

                            expr_stack.push((Box::new(ASTNode {
                                token: tokens[*i - 1].clone(),
                                node: Box::new(NodeType::ScopedExpression(scope)),
                            }), *i));
                        }
                        else {
                            expr_stack.push((node, *i));
                        }
                    }
                }
            }
            else if token.token_type == TokenType::RParen {
                last_was_unary_operator = false;
                paran_index -= 1;
                while let Some(top_op) = op_stack.pop() {
                    let right = expr_stack.pop().unwrap_or_else(|| {
                        self.error("Couldn't parse expression in parenthesis", "Right hand side of expression is empty", &token.location);
                        return (Box::new(ASTNode::err()), 0);
                    }).0;
                    let left = expr_stack.pop().unwrap_or_else(|| {
                        self.error("Couldn't parse expression in parenthesis", "Left hand side of expression is empty", &token.location);
                        return (Box::new(ASTNode::err()), 0);
                    }).0;

                    let node = Box::new(ASTNode {
                        token: top_op.0.clone(),
                        node: Box::new(NodeType::Operator(Expression{
                            left,
                            right,
                            operator: top_op.0,
                        })),
                    });

                    expr_stack.push((node, *i));
                }

                if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                    // scope traversal
                    *i += 1;
                    let given_scope = ScopedIdentifier {
                        scope: vec![Identifier {
                            expression: expr_stack.pop().map(|x| x.0).unwrap_or_else(|| {
                                self.error("Couldn't parse scope expression in parenthesis", "Expression is empty while trying to get scope", &token.location);
                                return Box::new(ASTNode::err());
                            }),
                            scope_type: None,
                            type_parameters: None
                        }]
                    };
                    let scope = self.scope_call_with_scope(tokens, i, given_scope, Some(ScopeType::Dot));

                    expr_stack.push((Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(scope)),
                    }), *i));
                }
            }
            else if token.token_type == TokenType::LBracket {
                if last_was_ident {
                    // indexing
                    let temp_scope = ScopedIdentifier {
                        scope: vec![Identifier {
                            expression: expr_stack.pop().map(|x| x.0).unwrap_or(
                                Box::new(ASTNode {
                                    token: tokens[*i - 1].clone(),
                                    node: Box::new(NodeType::Identifier(tokens[*i - 1].clone())),
                                })),
                            scope_type: None,
                            type_parameters: None
                        }]
                    };
                    let indexing_expresion = self.get_indexer_expression(tokens, Some(temp_scope), i);
                    expr_stack.push((Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(indexing_expresion)),
                    }), *i));
                }
                else {
                    // array expression
                    let array_expression = self.get_array_expression(tokens, i);
                    expr_stack.push((Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(array_expression)),
                    }), *i));
                }
            }
            else if token.token_type == TokenType::RBracket {
                self.error("Missing delimeter", "Expected opening bracket `[`", &tokens[*i].location);
            }
            else if token.token_type == TokenType::LBrace {
                if last_was_ident {
                    let expr_token = tokens[*i].clone();
                    let obj_instantiation = self.get_object_instantiation(tokens, None, i, None);

                    expr_stack.push((Box::new(ASTNode {
                        token: expr_token,
                        node: Box::new(NodeType::ScopedExpression(obj_instantiation))
                    }), *i));
                }
                else {
                    // code block
                    let code_block = self.get_code_block(tokens, i, false);
                    expr_stack.push((Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::CodeBlock(code_block)),
                    }), *i));
                }
                if tokens.len() >= *i && self.__curent_parsing_line + 1 < self.lines.len() {
                    if self.lines[self.__curent_parsing_line + 1].get(0).map_or(false, |t| t.token_type.is_operator() || matches!(t.token_type, TokenType::Dot | TokenType::LBrace | TokenType::LBracket | TokenType::LParen | TokenType::RParen | TokenType::RBracket | TokenType::RBrace)) {
                        self.__curent_parsing_line += 1;
                        *i = 0;
                        *tokens = self.lines[self.__curent_parsing_line].clone();
                        inc_i = false;
                    }
                }
            }
            else if token.token_type == TokenType::RBrace {
                self.error("Missing delimeter", "Expected opening brace `{`", &tokens[*i].location);
            }
            else if token.token_type.is_constant() {
                last_was_unary_operator = false;
                let mut node = Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::None),
                });

                if let Ok(constant_type) = Self::constant_type(&token) {
                    node.node = Box::new(NodeType::Constant(ConstantNode {
                        value: token.clone(),
                        constant_type,
                    }));
                } else {
                    self.error("Could not parse constant", "Couldn't decide type from constant", &token.location);
                    return Box::new(ASTNode::err());
                }

                expr_stack.push((node, *i));
            }
            else if token.token_type == TokenType::DoubleArrow {
                let lambda = self.get_lambda(tokens, i, &mut expr_stack, &mut inc_i);

                expr_stack.push((Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::LambdaExpression(lambda)),
                }), *i));
            } else if token.token_type == TokenType::Underscore {
                expr_stack.push((Box::new(ASTNode {
                    node: Box::new(NodeType::Discard(token.clone())),
                    token
                }), *i));
            }
            else if token.token_type == TokenType::Comma {
                self.error("Unexpected token in expression", "Didn't expect this token in expression. Maybe you meant to use a comma in a tuple, if that's so, put inside parentheises", &token.location);
                return Box::new(ASTNode::err());
            }
            else {
                self.error("Unexpected token in expression", "Didn't expect this token in expression", &token.location);
                return Box::new(ASTNode::err());
            }

            if inc_i {
                *i += 1;
            }

            if is_1_expression && paran_index == 0 && ((last_was_ident && tokens[*i - 1].token_type == TokenType::RParen) || !last_was_ident) && !(tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::QuestionMark) {
                break;
            }
        }

        let expression_node = self.expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack);

        return expression_node.unwrap_or(Box::new(ASTNode::err()));
    }

    fn get_tokens_in_delimeter(tokens: &Vec<Box<Token>>, i: &mut usize, start: TokenType, end: TokenType, return_stack: &mut Vec<Box<Token>>) {
        let mut level = 0;
        while *i < tokens.len() {
            return_stack.push(tokens[*i].clone());
            if tokens[*i].token_type == start {
                level += 1;
            } else if tokens[*i].token_type == end {
                level -= 1;
                if level == 0 {
                    break;
                }
            }
            *i += 1;
        }
    }

    fn expression_stacks_to_ast_node(&mut self, op_stack: &mut Vec<(Box<Token>, usize)>, expr_stack: &mut Vec<(Box<ASTNode>, usize)>) -> Option<Box<ASTNode>> {
        while let Some(operator) = op_stack.pop() {
            if operator.0.token_type == TokenType::LParen && expr_stack.len() < 2 {
                break;
            }

            let right = expr_stack.pop();
            let left = expr_stack.pop();

            let node = Box::new(ASTNode {
                token: operator.0.clone(),
                node: Box::new(NodeType::Operator(Expression {
                    left: left.unwrap_or_else(|| {
                        self.error("Couldn't parse expression", "Left hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::err()), 0)
                    }).0,
                    right: right.unwrap_or_else(|| {
                        self.error("Couldn't parse expression", "Right hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::err()), 0)
                    }).0,
                    operator: operator.0.clone(),
                })),
            });

            expr_stack.push((node, 0));
        }

        if expr_stack.len() != 1 && expr_stack.len() > 0 {
            self.error("Couldn't parse expression", "The expression does not have a root. This usually occurs with the typo: `a b`", &expr_stack.last().unwrap().0.token.location);
        }

        return expr_stack.pop().map(|x| x.0);
    }

    fn get_lambda(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, expr_stack: &mut Vec<(Box<ASTNode>, usize)>, inc_i: &mut bool) -> LambdaExpression {
        let token = tokens[*i].clone();

        if expr_stack.is_empty() {
            self.error("Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
        }
        let parameters_node = expr_stack.pop().unwrap_or_else(|| {
            self.error("Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
            return (Box::new(ASTNode::err()), 0);
        }).0;

        let parameters = NodeParameters {
            parameters: {
                if let NodeType::TupleExpression(ref value) = parameters_node.node.as_ref() {
                    value.parameters.clone()
                } else {
                    vec![parameters_node]
                }
            }
        };

        // check if body is code block or expression: `x => x + 1` or `x => { x + 1 }`
        let body = if tokens.get(*i + 1).is_some().then(|| tokens[*i + 1].token_type == TokenType::LBrace).unwrap_or(false) {
            // code block
            *i += 1;
            let block = self.get_code_block(tokens, i, true);
            
            *inc_i = false;
            if *i + 1 >= tokens.len() && self.__curent_parsing_line + 1 < self.lines.len() {
                self.__curent_parsing_line += 1;
                *i = 0;
                *tokens = self.lines[self.__curent_parsing_line].clone();
            }
            
            block
        } else {
            // expression
            let mut lambda_tokens = vec![];
            *i += 1;
            let mut parenthesis_level = 0;
            let mut brace_level = 0;
            let mut bracket_level = 0;

            while self.__curent_parsing_line < self.lines.len() {
                let mut done = false;
                while *i < tokens.len() {
                    match tokens[*i].token_type {
                        TokenType::LParen => {
                            parenthesis_level += 1;
                            lambda_tokens.push(tokens[*i].clone());
                        }
                        TokenType::RParen => {
                            parenthesis_level -= 1;
                            lambda_tokens.push(tokens[*i].clone());
                            if parenthesis_level <= 0 && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBracket) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBrace) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                                done = true;
                                break;
                            }
                        }
                        TokenType::LBrace => {
                            brace_level += 1;
                            lambda_tokens.push(tokens[*i].clone());
                        }
                        TokenType::RBrace => {
                            brace_level -= 1;
                            lambda_tokens.push(tokens[*i].clone());
                            if brace_level <= 0  && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBracket) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBrace) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                                done = true;
                                break;
                            }
                        }
                        TokenType::LBracket => {
                            bracket_level += 1;
                            lambda_tokens.push(tokens[*i].clone());
                        }
                        TokenType::RBracket => {
                            bracket_level -= 1;
                            lambda_tokens.push(tokens[*i].clone());
                            if bracket_level <= 0 && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBracket) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LBrace) && !tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                                done = true;
                                break;
                            }
                        }
                        _ => {
                            lambda_tokens.push(tokens[*i].clone());
                        }
                    }
                    *i += 1;
                }

                if done {
                    break;
                }

                self.__curent_parsing_line += 1;
                if self.__curent_parsing_line >= self.lines.len() {
                    break;
                }
                *tokens = self.lines[self.__curent_parsing_line].clone();
                *i = 0;
            }

            let expression = self.get_entire_expression(&mut lambda_tokens);
            BodyRegion {
                body: vec![Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::ReturnExpression(expression)),
                })]
            }
        };

        LambdaExpression {
            parameters,
            body,
        }
    }

    fn get_parameters_for_function(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Box<ASTNode>> {
        /*
            It needs to loop through tokens until it finds the ending `)`. The problem I was having is that it doesn't take into account new lines.
            So like `func(a => {a;b})` wouldn't parse normally. So I got to fix this.
            I'm thinking it will parse until it hits a comma, and then it would add it to a stack. If it hits a `)` then it would add it to the stack and return.
        */

        let mut parameters = vec![];
        *i += 1;

        let until = Some(Self::PARSRING_FOR_FUNCTION);
        parameters.push(self.get_expression(tokens, i, until));

        if tokens.get(*i).map_or(false, |x| x.token_type != TokenType::Comma) {
            *i += 1;
        }

        let mut done = false;
        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                if tokens[*i].token_type == TokenType::RParen {
                    // ends
                    *i += 1;
                    done = true;
                    break;
                }
                else if tokens[*i].token_type == TokenType::Comma {
                    // add to stack
                    *i += 1;
                    let expression = self.get_expression(tokens, i, until);
                    parameters.push(expression);
                }
                else {
                    // error
                    self.error("Error getting parameters for function", format!("Expected `,` or `)` but got `{}` while parsing function", tokens[*i].value).as_str(), &tokens[*i].location);
                    done = true;
                    break;
                }
                *i += 1;
            }

            if done {
                break;
            }
            
            self.__curent_parsing_line += 1;
            if self.__curent_parsing_line >= self.lines.len() {
                break;
            }
            *i = 0;
            *tokens = self.lines[self.__curent_parsing_line].clone();
        }

        parameters
    }

    fn get_tuple_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Box<ASTNode>> {
        // (Tuple, Type)
        let all_tokens = self.get_node_parameters_for_tuple(tokens, i);

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_type_idententifier(&mut tokens, i));
        }

        return_tokens
    }

    fn get_type_idententifier(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Box<ASTNode> {
        if let Some(first_token) = tokens.iter().skip(*i).map(|t| t.clone()).collect::<Vec<_>>().first() {
            if first_token.token_type == TokenType::Identifier || first_token.token_type == TokenType::Auto {
                if tokens.iter().skip(*i).len() == 1 {
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(ScopedType {
                            scope: vec![TypeIdentifier {
                                name: first_token.clone(),
                                type_parameters: None,
                                scope_type: None,
                            }],
                            is_array: false,
                            is_ptr_or_ref: vec![],
                        }))
                    });
                }
                else if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::DoubleColon || t.token_type == TokenType::Dot || t.token_type == TokenType::LessThan || t.token_type == TokenType::LBracket || t.token_type == TokenType::Star || t.token_type == TokenType::Ampersand) {
                    let scope_and_types = self.get_scoped_typed(tokens, i);
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(scope_and_types)),
                    });
                }
                else if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::Comma) {
                    self.error("Incorrect type", "Multiple variables in one declaration is not supported, try instead using a tuple by putting parenthesis around types", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                    return Box::new(ASTNode::err());
                }
                else {
                    self.error("Incorrect type", "Expected a tuple or type before the `::` or  `.`", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                    return Box::new(ASTNode::err());
                }
            }
            else if first_token.token_type == TokenType::LParen {
                let tuple = self.get_tuple_node_parameters(tokens, i);
                return Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::TupleDeclaration(NodeParameters {
                        parameters: tuple
                    }))
                });
            }
            else if first_token.token_type == TokenType::LBrace {
                self.error("Incorrect type", "Incorrect type declaration `type: name`, objects `{}` are not supported as a type", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                return Box::new(ASTNode::err());
            }
            else if first_token.token_type == TokenType::LBracket {
                self.error("Incorrect type", "Incorrect type declaration. If you were trying to create an array, do: `type[]`", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                return Box::new(ASTNode::err());
            }
            else {
                self.error("Variable declaration has incorrect type", format!("Expected tuple or type, found: `{}`", first_token.value).as_str(), &first_token.location);
                return Box::new(ASTNode::err());
            }
        }
        else if tokens.iter().skip(*i).len() > 0 {
            self.error("Variable declaration has no type", "Expected a type before the `:`, no type was provided", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
            return Box::new(ASTNode::err());
        }
        else {
            return Box::new(ASTNode::err());
        }
    }

    fn get_node_parameters_for_tuple(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Vec<Box<Token>>> { 
        let mut comma_count = 0;
        let mut angle_bracket_level = 0;
        let mut all_tokens = vec![vec![]];
        let mut paranthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;

        if let Some(token) = tokens.get(*i) {
            if token.token_type != TokenType::LParen {
                self.error("Error getting node parameters", "Expected to start with `(`", &token.location);
            }
        }

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            match token.token_type {
                TokenType::LBrace => {
                    brace_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RBrace => {
                    brace_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LBracket => {
                    bracket_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RBracket => {
                    bracket_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LessThan => {
                    angle_bracket_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::GreaterThan => {
                    angle_bracket_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LParen => {
                    paranthesis_level += 1;
                    if paranthesis_level != 1 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::RParen => {
                    paranthesis_level -= 1;
                    if paranthesis_level != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::Comma => {
                    if paranthesis_level == 1 && angle_bracket_level == 0 && brace_level == 0 && bracket_level == 0 {
                        comma_count += 1;
                        all_tokens.push(vec![]);
                    }
                    else {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                _ => {
                    all_tokens[comma_count].push(token.clone());
                }
            }
            *i += 1;

            if paranthesis_level <= 0 && angle_bracket_level <= 0 && brace_level <= 0 && bracket_level <= 0 {
                break;
            }

            // continues to next line
            if *i >= tokens.len() {
                *i = 0;
                self.__curent_parsing_line += 1;
                let mut err = false;
                let v = Vec::new();

                *tokens = self.lines.get(self.__curent_parsing_line).unwrap_or_else(|| {
                    err = true;
                    &v
                }).clone();

                if err {
                    self.error("Error getting node parameters", "Could not find next line", &token.location);
                    break;
                }
            }
        }
        all_tokens
    }

    fn get_scoped_typed(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedType {
        let scope = self.get_type_scope(tokens, i);
        let is_array = self.next_is_array_for_type(tokens, i);
        let mut is_ptr_or_ref = vec![];

        while *i < tokens.len() {
            if tokens[*i].token_type == TokenType::Star {
                is_ptr_or_ref.push(TypeSuffix::Ptr);
            } else if tokens[*i].token_type == TokenType::Ampersand {
                is_ptr_or_ref.push(TypeSuffix::Ref);
            } else {
                break;
            }
            *i += 1;
        }

        ScopedType {
            scope,
            is_array,
            is_ptr_or_ref,
        }
    }

    fn get_type_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> NodeParameters {
        // <Type, With, Parameters>
        let mut comma_count = 0;
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        let mut angle_bracket_level = 0;
        let mut parenthesis_level = 0;
        let mut bracket_level = 0;
        let mut brace_level = 0;

        if let Some(token) = tokens.get(*i) {
            if token.token_type != TokenType::LessThan {
                self.error("Error getting type parameters", "Expected to start with `<`", &token.location);
                return NodeParameters {
                    parameters: vec![]
                };
            }
        }

        for token in tokens.iter().skip(*i) {
            match token.token_type {
                TokenType::LBrace => {
                    brace_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RBrace => {
                    brace_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LBracket => {
                    bracket_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RBracket => {
                    bracket_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LParen => {
                    parenthesis_level += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RParen => {
                    parenthesis_level -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::LessThan => {
                    angle_bracket_level += 1;
                    if angle_bracket_level != 1 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::GreaterThan => {
                    angle_bracket_level -= 1;
                    if angle_bracket_level != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::Comma => {
                    if parenthesis_level == 0 && angle_bracket_level == 1 && brace_level == 0 && bracket_level == 0 {
                        comma_count += 1;
                        all_tokens.push(Vec::new());
                    }
                    else {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                _ => {
                    all_tokens[comma_count].push(token.clone());
                }
            }
            *i += 1;
            if angle_bracket_level == 0 && parenthesis_level == 0 && brace_level == 0 && bracket_level == 0 {
                break;
            }
        }

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_type_idententifier(&mut tokens, &mut 0));
        }

        NodeParameters {
            parameters: return_tokens
        }
    }

    fn get_ident_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, last_punc: &mut Option<ScopeType>) -> (ScopedIdentifier, bool) { // (Identifier, i_is_not_less_than /*not including type parameters*/)
        let mut scope = ScopedIdentifier { scope: vec![] };
        let mut first_token = true;
        let mut keep_last_punc = None;
        let mut last_identifier_index = *i;

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        self.error("Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return (scope, true);
                    }
                    *last_punc = Some(ScopeType::DoubleColon);
                    keep_last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        self.error("Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return (scope, true);
                    }
                    *last_punc = Some(ScopeType::Dot);
                    keep_last_punc = Some(ScopeType::Dot);
                }
                TokenType::Identifier => {
                    last_identifier_index = *i;
                    *i += 1;
                    let mut type_parameters = Some(NodeParameters { parameters: vec![] });

                    if tokens.get(*i).is_some_and(|t| t.token_type == TokenType::LessThan) {
                        let mut j = *i;
                        let mut angle_bracket_level = 0;
                        while j < tokens.len() {
                            if tokens[j].token_type == TokenType::LessThan {
                                angle_bracket_level += 1;
                            }
                            else if tokens[j].token_type == TokenType::GreaterThan {
                                angle_bracket_level -= 1;
                                if angle_bracket_level == 0 {
                                    if tokens.get(j + 1).map_or(false, |t| t.token_type == TokenType::LParen || tokens[j + 1].token_type == TokenType::DoubleColon || tokens[j + 1].token_type == TokenType::Dot) {
                                        let tp = self.get_type_parameters(tokens, i);
                                        type_parameters = Some(tp);
                                    }
                                    break;
                                }
                            }
                            j += 1;
                        }
                    }
                    
                    if last_punc.is_none() && !first_token {
                        self.error("Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
                        return (scope, true);
                    }
                    first_token = false;

                    scope.scope.push(Identifier {
                        expression: Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Identifier(token.clone())),
                        }),
                        scope_type: last_punc.clone(),
                        type_parameters
                    });
                    *last_punc = None;
                }
                TokenType::LBracket => {
                    *i = last_identifier_index;
                    *last_punc = keep_last_punc;
                    return (scope, true);
                }
                TokenType::LessThan => {
                    *i = last_identifier_index;
                    *last_punc = keep_last_punc;
                    scope.scope.pop();
                    
                    // the false basically means that the next token is not the start of a type parameter list, even though it is a less than operator
                    // if it were, it would have been handled above in the TokenType::Identifier case
                    return (scope, false); 
                }
                _ => {
                    *i = last_identifier_index;
                    *last_punc = keep_last_punc;
                    scope.scope.pop();
                    return (scope, true);
                }
            }
        }

        if last_punc.is_some() {
            self.error("Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
            return (scope, true);
        }

        return (scope, true);
    }

    fn get_type_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<TypeIdentifier> {
        let mut scope = vec![];
        let mut last_punc: Option<ScopeType> = None;
        let mut first_token = true;

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() {
                        self.error("Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                    first_token = false;
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() {
                        self.error("Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::Dot);
                    first_token = false;
                }
                TokenType::Identifier => {
                    let mut maybe_type_parameters = NodeParameters { parameters: vec![] };
                    if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::LessThan) {
                        *i += 1;
                        let mut insides = vec![];
                        Self::get_tokens_in_delimeter(tokens, i, TokenType::LessThan, TokenType::GreaterThan, &mut insides);

                        let mut j = 0;
                        maybe_type_parameters = self.get_type_parameters(&mut insides, &mut j);
                    } 
                    *i += 1;

                    if last_punc.is_none() && !first_token {
                        self.error("Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
                        return scope;
                    }

                    scope.push(TypeIdentifier {
                        name: token.clone(),
                        scope_type: last_punc.clone(),
                        type_parameters: if maybe_type_parameters.parameters.len() > 0 { Some(maybe_type_parameters) } else { None },
                    });
                    last_punc = None;
                    first_token = false;
                }
                _ => {
                    break;
                }
            }
        }

        if last_punc.is_some() {
            self.error("Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
            return scope;
        }

        return scope
    }

    fn next_is_array_for_type(&mut self, tokens: &Vec<Box<Token>>, i: &mut usize) -> bool {
        if tokens.get(*i).is_some_and(|t| t.token_type == TokenType::LBracket) {
            if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::RBracket) {
                *i += 2;
                return true;
            }
            else {
                self.error("Error setting type parameters", "Expected type to be array, but only `[` was given with no closing `]`. Expected: `type[]`", &tokens[*i].location);
                return false;
            }
        }
        return false;
    }

    #[allow(dead_code)]
    fn print_node(node: &ASTNode) {
        println!("{}", Self::node_expr_to_string(node));
    }

    fn node_expr_to_string(node: &ASTNode) -> String {
        match *node.node {
            NodeType::VariableDeclaration(ref value) => {
                let var_type = Self::node_expr_to_string(&value.var_type);
                let var_name = value.var_name.value.clone();
                let var_value = Self::node_expr_to_string(&value.var_value);
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
                if value.description.is_some() {
                    format!("{}{}: {} = {} -> \"{}\"", access_modifiers, var_type, var_name, var_value, value.description.clone().unwrap().value)
                }
                else {
                    format!("{}{}: {} = {}", access_modifiers, var_type, var_name, var_value)
                }
            }
            NodeType::TypeIdentifier(ref value) => {
                let array = value.is_array.then(|| "[]").unwrap_or("");
                let mut pointer = "".to_string();
                for suffix in value.is_ptr_or_ref.iter() {
                    pointer += match suffix {
                        TypeSuffix::Ptr => "*",
                        TypeSuffix::Ref => "&",
                    };
                }
                let mut scope = "".to_string();
                for ident in value.scope.iter() {
                    let scope_type = ident.scope_type.clone().is_some().then(|| ident.scope_type.clone().unwrap().to_string()).unwrap_or("".to_string());
                    let identifier = ident.name.value.clone();
                    if let Some(type_parameters) = &ident.type_parameters {
                        let mut parameters = "".to_string();
                        for (index, param) in type_parameters.parameters.iter().enumerate() {
                            parameters += format!("{}{}", Self::node_expr_to_string(param).as_str(), type_parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                        }
                        scope += format!("{}{}<{}>", scope_type, identifier, parameters).as_str();
                    }
                    else {
                        scope += format!("{}{}", scope_type, identifier).as_str();
                    }
                }
                format!("{}{}{}", scope, array, pointer)
            }
            NodeType::TupleDeclaration(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("({})", tuple)
            }
            NodeType::Operator(ref value) => {
                let left = Self::node_expr_to_string(&value.left);
                let right = Self::node_expr_to_string(&value.right);

                format!("({} {} {})", left, value.operator.value, right)
            }
            NodeType::UnaryOperator(ref value) => {
                let operand = Self::node_expr_to_string(&value.operand);

                format!("({}{})", value.operator.value, operand)
            }
            NodeType::FunctionCall(ref value) => {
                let mut parameters = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    parameters += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut type_parameters = "".to_string();
                if value.type_parameters.is_some() {
                    type_parameters += "<";
                    for (index, param) in value.type_parameters.clone().unwrap().iter().enumerate() {
                        type_parameters += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.type_parameters.clone().unwrap().get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                    }
                    type_parameters += ">";
                }
                format!("{}{}({})", value.name.value, type_parameters, parameters)
            }
            NodeType::Identifier(ref value) => {
                value.value.clone()
            }
            NodeType::ScopedExpression(ref value) => {
                let mut scope = "".to_string();
                for ident in value.scope.iter() {
                    let scope_type = ident.scope_type.clone().is_some().then(|| ident.scope_type.clone().unwrap().to_string()).unwrap_or("".to_string());
                    
                    let mut type_parameters = "".to_string();
                    if ident.type_parameters.is_some() && ident.type_parameters.clone().unwrap().parameters.len() > 0 {
                        type_parameters += "<";
                        for (index, param) in ident.type_parameters.clone().unwrap().parameters.iter().enumerate() {
                            type_parameters += format!("{}{}", Self::node_expr_to_string(param).as_str(), ident.type_parameters.clone().unwrap().parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                        }
                        type_parameters += ">";
                    }
                    scope += format!("{}{}{}", scope_type, Self::node_expr_to_string(ident.expression.as_ref()), type_parameters).as_str();
                }
                scope
            }
            NodeType::TupleExpression(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("({})", tuple)
            }
            NodeType::Constant(ref value) => {
                if value.constant_type == ConstantType::String {
                    format!("\"{}\"", value.value.value)
                }
                else {
                    value.value.value.clone()
                }
            }
            NodeType::TernaryOperator(ref value) => {
                let condition = Self::node_expr_to_string(&value.condition);
                let then = Self::node_expr_to_string(&value.then);
                let else_then = Self::node_expr_to_string(&value.else_then);

                format!("({} ? {} : {})", condition, then, else_then)
            }
            NodeType::ArrayExpression(ref value) => {
                let mut array = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    array += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("[{}]", array)
            }
            NodeType::Indexer(ref value) => {
                let object = Self::node_expr_to_string(&value.object);
                let mut index = "".to_string();
                for (i, param) in value.index.iter().enumerate() {
                    index += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.index.get(i + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("{}[{}]", object, index)
            }
            NodeType::ObjectInstantiation(ref value) => {
                let mut object = "".to_string();
                let object_type = Self::node_expr_to_string(&value.object_type);
                for (index, param) in value.properties.iter().enumerate() {
                    object += format!("{} = {}{}", param.name.value, Self::node_expr_to_string(&param.value).as_str(), value.properties.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                if object.is_empty() {
                    format!("{} {{}}", object_type)
                }
                else {
                    format!("{} {{ {} }}", object_type, object)
                }
            }
            NodeType::CodeBlock(ref value) => {
                let mut body = "".to_string();
                for param in value.body.iter() {
                    body += format!("{}{}", Self::node_expr_to_string(&param).as_str(), "; ").as_str();
                }
                format!("{{ {}}}", body)
            }
            NodeType::ReturnExpression(ref value) => {
                format!("return {}", Self::node_expr_to_string(&value))
            }
            NodeType::LambdaExpression(ref value) => {
                let mut parameters = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    parameters += format!("{}{}", Self::node_expr_to_string(param).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut body = "".to_string();
                for param in value.body.body.iter() {
                    body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                }
                format!("({}) => {{ {}}}", parameters, body)
            }
            NodeType::Assignment(ref value) => {
                format!("{} = {}", Self::node_expr_to_string(&value.left), Self::node_expr_to_string(&value.right))
            }
            NodeType::If(ref value) | NodeType::While(ref value) => {
                let condition = Self::node_expr_to_string(&value.condition);
                let mut body = "".to_string();
                for param in value.body.body.iter() {
                    body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                }
                let mut else_if_string = "".to_string();
                if let Some(else_if) = value.else_if_regions.clone() {
                    for region in else_if.iter() {
                        let condition = Self::node_expr_to_string(&region.condition);
                        let mut body = "".to_string();
                        for param in region.body.body.iter() {
                            body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                        }
                        let regional_if_name = region.is_while.then(|| "while").unwrap_or("if");
                        else_if_string += format!("\nelse {} {} {{ {} }}", regional_if_name, condition, body).as_str();
                    }
                }

                let mut else_string = "".to_string();
                if let Some(else_region) = value.else_region.clone() {
                    let mut body = "".to_string();
                    for param in else_region.body.iter() {
                        body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                    }
                    else_string += format!("\nelse {{ {} }}", body).as_str();
                }
                let if_name = value.is_while.then(|| "while").unwrap_or("if");

                format!("{} {} {{ {} }}{}{}", if_name, condition, body, else_if_string, else_string)
            }
            NodeType::ForEach(ref value) => {
                let index = value.index_segment.clone().map_or("".to_string(), |t| t.value.clone());
                let val = Self::node_expr_to_string(&value.iter_value);
                let range = Self::node_expr_to_string(&value.iter_range);

                let mut body = "".to_string();
                for param in value.body.body.iter() {
                    body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                }

                format!("for {}{} in {} {{ {} }}", value.index_segment.clone().map_or("".to_string(), |_| index + ", "), val, range, body)
            }
            NodeType::For(ref value) => {
                let index = value.index_segment.clone().map_or("".to_string(), |t| t.value.clone());
                let set = Self::node_expr_to_string(&value.set_segment);
                let cond = Self::node_expr_to_string(&value.condition_segment);
                let inc = Self::node_expr_to_string(&value.increment_segment);

                let mut body = "".to_string();
                for param in value.body.body.iter() {
                    body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                }

                format!("for {}{}, {}, {} {{ {} }}", value.index_segment.clone().map_or("".to_string(), |_| index + ", "), set, cond, inc, body)
            }
            NodeType::Match(ref value) => {
                let expression = Self::node_expr_to_string(&value.match_value);
                let mut cases = "".to_string();

                for case in value.match_cases.clone() {
                    let mut c = Self::node_expr_to_string(&case.pattern);
                    c += " => { ";
                    
                    let mut body = "".to_string();
                    for param in case.body.body.iter() {
                        body += format!("{}; ", Self::node_expr_to_string(&param).as_str()).as_str();
                    }

                    c += &body;
                    c += "}";

                    cases = cases + &(c + ", ");
                }

                format!("match {} {{ {} }}", expression, cases)
            }
            NodeType::Discard(_) => {
                "_".to_string()
            }
            _ => {
                node.token.value.clone()
            }
        }
    }

    fn constant_type(token: &Token) -> Result<ConstantType, &str> {
        match token.token_type {
            TokenType::BoolConstant => Ok(ConstantType::Bool),
            TokenType::StringConstant => Ok(ConstantType::String),
            TokenType::CharConstant => Ok(ConstantType::Char),
            TokenType::NumberConstant => {
                let number = token.value.to_lowercase().replace("_", "");
                if number.ends_with("u8") { return Ok(ConstantType::U8) }
                else if number.ends_with("i8") { return Ok(ConstantType::I8) }
                else if number.ends_with("u16") { return Ok(ConstantType::U16) }
                else if number.ends_with("i16") { return Ok(ConstantType::I16) }
                else if number.ends_with("u32") { return Ok(ConstantType::U32) }
                else if number.ends_with("i32") { return Ok(ConstantType::I32) }
                else if number.ends_with("u64") { return Ok(ConstantType::U64) }
                else if number.ends_with("i64") { return Ok(ConstantType::I64) }
                else if number.ends_with("u128") { return Ok(ConstantType::U128) }
                else if number.ends_with("i128") { return Ok(ConstantType::I128) }
                else if number.ends_with("f32") { return Ok(ConstantType::F32) }
                else if number.ends_with("f64") { return Ok(ConstantType::F64) }
                else if let Ok(_) = number.parse::<u8>() { return Ok(ConstantType::U8) }
                else if let Ok(_) = number.parse::<i8>() { return Ok(ConstantType::I8)}
                else if let Ok(_) = number.parse::<u16>() { return Ok(ConstantType::U16) }
                else if let Ok(_) = number.parse::<i16>() { return Ok(ConstantType::I16) }
                else if let Ok(_) = number.parse::<u32>() { return Ok(ConstantType::U32) }
                else if let Ok(_) = number.parse::<i32>() { return Ok(ConstantType::I32) }
                else if let Ok(_) = number.parse::<u64>() { return Ok(ConstantType::U64) }
                else if let Ok(_) = number.parse::<i64>() { return Ok(ConstantType::I64) }
                else if let Ok(_) = number.parse::<u128>() { return Ok(ConstantType::U128) }
                else if let Ok(_) = number.parse::<i128>() { return Ok(ConstantType::I128) }
                else if let Ok(_) = number.parse::<f32>() { if number.len() <= 7 { return Ok(ConstantType::F32) } else { return Ok(ConstantType::F64) } }
                else if let Ok(_) = number.parse::<f64>() { return Ok(ConstantType::F64) }
                else { Err("could not get constant type") }
            }
            _ => Err("could not get constant type"),
        }
    }

    fn split_tokens_into_lines(tokens: &Vec<Token>) -> Vec<Vec<Box<Token>>> {
        tokens.split(|t| t.token_type == TokenType::EndOfLine)
            .map(|l| l.iter().map(|t| Box::new(t.clone())).collect::<Vec<Box<Token>>>())
            .filter(|x| !x.is_empty())
            .collect()
    }
}