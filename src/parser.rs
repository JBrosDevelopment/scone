// eventually need to remove [] and instead use get
// remove all unwrapping and replace with error handling
// go over and make sure no unneciary cloning is happening

use crate::lexer::{Token, TokenType, Location};
use crate::operator_tokens;

#[allow(unused_imports)]
use crate::{ast::*, error_handling::{ErrorHandling, DEBUGGING}};
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

    pub fn error(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra errors on the same line
            return;
        }
        if DEBUGGING {
            self.output.error("parsing error", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.output.error("parsing error", message, help, location);
        }
    }
    pub fn warning(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra warnings on the same line
            return;
        }
        if DEBUGGING {
            self.output.warning("parsing warning", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        }
        else {
            self.output.warning("parsing warning", message, help, location);
        }
    }
    pub fn message(&mut self, debug_line: u32, message: &str, help: &str, location: &Location) {
        if self.output.errors().iter().any(|t| t.location.line == location.line) { // prevent extra messages on the same line
            return;
        }
        if DEBUGGING {
            self.output.message("parsing message", format!("[DEBUG {}:{}]: {}", file!(), debug_line, message).as_str(), help, location);
        } else {
            self.output.message("parsing message", message, help, location);
        }
    }

    fn inc(i: &mut usize) {
        if *i != Parser::SET_I_TO_ZERO && i.checked_add(1).is_some() {
            *i += 1;
        }
    }
    fn dec(i: &mut usize) {
        if *i != Parser::SET_I_TO_ZERO && i.checked_sub(1).is_some() {
            *i -= 1;
        }
    }

    fn generate_from_tokens(&mut self) -> Vec<ASTNode> {
        let mut ast: Vec<ASTNode> = Vec::new();

        while self.__curent_parsing_line < self.lines.len() {
            let mut tokens = self.lines[self.__curent_parsing_line].clone();
            let node = self.get_ast_node(&mut tokens);
            
            println!("{}", Self::node_expr_to_string(&node, 0));
            ast.push(node);
            
            self.__curent_parsing_line += 1;
        }

        ast
    }

    fn get_ast_node(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        if tokens.len() == 0 {
            return ASTNode::err();
        }

        match tokens[0].token_type {
            TokenType::For => { // this is so that `for i = 0, i < 10, i += 1` doesn't parse as a variable assignment with the `=`
                return *self.get_entire_expression(tokens);
            }
            TokenType::Use => {
                if tokens.len() != 2 {
                    self.error(line!(), "Error parsing `use` statement", "`use` statement must have exactly one argument", &tokens[0].location);
                    return ASTNode::err();
                }
                let name = tokens[1].clone();
                return ASTNode {
                    node: Box::new(NodeType::Use(name)),
                    token: tokens[0].clone(),
                };
            }
            _ => {}
        }

        // anything else
        return *self.get_entire_expression(tokens);
    }

    fn declaring_variable(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, is_in_for: bool) -> ASTNode { // `tokens` fpr tokens to parse, `i` for current token index only used if `is_in_for` is true, `is_in_for` whether or not we are in a for loop
        let starting_index: usize;
        if is_in_for {
            let position_of_for: i32 = tokens.iter().position(|t| t.token_type == TokenType::For).map_or(-1, |index| index as i32);
            if position_of_for == -1 {
                starting_index = 1;
            } else {
                if tokens.get((position_of_for + 1) as usize).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get((position_of_for + 2) as usize).map_or(false, |t| t.token_type == TokenType::Comma) {
                    // for INDEXER, SET, CONDITION, INCREMENT {}
                    starting_index = (position_of_for + 3) as usize;
                } else {
                    // for SET, CONDITION, INCREMENT {}
                    starting_index = 1;
                }
            }
        } else {
            starting_index = 0;
        }

        let assign_index = tokens.iter().position(|t| t.token_type == TokenType::Assign).map_or(-1, |index| index as i32);

        if assign_index != -1 {
            let lhs = tokens[starting_index..assign_index as usize].to_vec();
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

                let mut type_tokens = lhs[accessing_end_index..lhs.len() - 2].to_vec();
                let var_type: Box<ASTNode> = self.get_type_idententifier(&mut type_tokens, &mut 0, false);

                let var_value = if is_in_for {
                    self.get_expression(tokens, i, Self::PARSING_FOR_FOR_LOOP_STATEMENT)
                } else {
                    self.get_entire_expression(&mut rhs)
                };

                let var_decl = VariableDeclaration {
                    access_modifier,
                    var_name: var_name.clone(),
                    var_value,
                    var_type,
                };
                return ASTNode {
                    token: var_name.clone(),
                    node: Box::new(NodeType::VariableDeclaration(var_decl)),
                };
            }

            return *self.get_entire_expression(tokens);
        }
        else {
            return ASTNode::err();
        }
    }

    fn declaring_function(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode {        
        Self::dec(i); // move `i` to `:`
        let before_colon: Vec<Box<Token>> = tokens.iter().take(*i).map(|t| t.clone()).collect(); // get tokens before `:`
        
        Self::inc(i);
        let name: Box<Token> = tokens[*i].clone();

        let mut access_modifier: Vec<AccessModifier> = Vec::new();
        let mut start_of_type = 0;
        for token in before_colon.iter() {
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
            start_of_type += 1;
        }
        
        let mut type_tokens = before_colon[start_of_type..].to_vec();
        
        let return_type: Box<ASTNode> = self.get_type_idententifier(&mut type_tokens, &mut 0, false);
        Self::inc(i); // skip name

        let type_parameters: Option<AnonymousTypeParameters> = if tokens[*i].token_type == TokenType::LessThan { // has type parameters
            Some(self.get_anonymous_type_parameters(tokens, i))
        } else {
            None
        };

        let parameters: Vec<DefinedNodeParameter> = self.get_defined_node_parameters(tokens, i);
        
        let body = if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            Some(self.get_code_block(tokens, i, false))
        } else {
            None
        };

        let function = FunctionDeclaration {
            access_modifier,
            return_type,
            name: name.clone(),
            type_parameters,
            parameters,
            body,
        };

        return ASTNode {
            token: name.clone(),
            node: Box::new(NodeType::FunctionDeclaration(function)),
        };
    }

    fn get_condition_and_body_for_if(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> (Box<ASTNode>, BodyRegion) {
        if tokens[*i].token_type != TokenType::If && tokens[*i].token_type != TokenType::While { 
            self.error(line!(), "Expected `if` or `while`", "Parser error, expected `if` or `while`", &tokens[*i].location);
        } else {
            Self::inc(i); // skip if
        }

        let condition = self.get_expression(tokens, i, Self::PARSING_FOR_STATEMENT);
        Self::dec(i);
        let body = self.get_code_block(tokens, i, false);
        
        return (condition, body);
    }

    fn parse_match_case(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> MatchCase {
        MatchCase { 
            pattern: self.get_expression(tokens, i, Self::PARSING_FOR_MATCH_CASE), 
            body: self.get_expression(tokens, i, Self::PARSING_FOR_MATCH_STATEMENT)
        }
    }

    fn parse_match(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode {
        if tokens[0].token_type != TokenType::Match {
            self.error(line!(), "Expected `match`", "Parser error, expected `match`", &tokens[0].location);
            return ASTNode::err();
        }
        let original_token = tokens[*i].clone();

        Self::inc(i); // skip `match`
        let match_value = self.get_expression(tokens, i, Self::PARSING_FOR_STATEMENT);
        Self::inc(i); // skip `{`

        if *i <= tokens.len() && self.__curent_parsing_line <= self.lines.len() {
            self.__curent_parsing_line += 1;
            *tokens = self.lines[self.__curent_parsing_line].clone();
            *i = 0;
        }

        let first_expression = self.parse_match_case(tokens, i);
        let mut match_cases: Vec<MatchCase> = vec![first_expression];
        Self::inc(i);

        let mut done = false;
        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                //debug!(tokens[*i]);
                if tokens[*i].token_type == TokenType::RBrace {
                    // ends
                    Self::inc(i);
                    done = true;
                    break;
                }
                else if tokens[*i].token_type == TokenType::Comma {
                    // add to stack
                    Self::inc(i);
                    let case = self.parse_match_case(tokens, i);
                    match_cases.push(case);
                }
                else if matches!(tokens[*i].token_type, TokenType::Identifier | TokenType::Underscore) {
                    // add to stack
                    let case = self.parse_match_case(tokens, i);
                    match_cases.push(case);
                }
                else {
                    //debug!("ERR?", tokens[*i]);
                    done = true;
                    break;
                }
                
                if *i == Parser::SET_I_TO_ZERO {
                    *i = 0;
                } else if *i != 0 {
                    Self::inc(i);
                }
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

        ASTNode {
            token: original_token,
            node: Box::new(NodeType::Match(MatchRegion {
                match_value,
                match_cases
            }))
        }
    }

    fn parse_for(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        if tokens[0].token_type != TokenType::For {
            self.error(line!(), "Expected `for`", "Parser error, expected `for`", &tokens[0].location);
            return ASTNode::err();
        }
        let for_token = tokens[0].clone();
        
        let mut i = 1;
        let segments = self.get_node_parameters(tokens, &mut i, Self::PARSING_FOR_STATEMENT);
        let body = self.get_code_block(tokens, &mut i, false);

        if segments.len() == 0 {
            self.error(line!(), "Could not parse for statement", "No parameters were found for the for statement: `for X in Y { ... }` or `for VAR, CONDITION, INC { ... }`", &for_token.location);
            return ASTNode::err();
        } else if segments.len() == 1 || segments.len() == 2 {
            // for X in Y
            let mut index_segment = None;
            if segments.len() == 2 {
                if let NodeType::Identifier(ref v) = segments[0].node.as_ref() {
                    index_segment = Some(v.clone());
                } else {
                    self.error(line!(), "Unable to parse `for` statement, incorrect indexer expression", "Expected a single identifier token as the indexer for the `for` statement: `for I, X in Y {}`", &for_token.location);
                }
            }

            let iter_value;
            let iter_range;
            if let NodeType::Operator(ref in_expression) = segments[if segments.len() == 1 {0} else {1}].node.as_ref() {
                iter_value = in_expression.left.clone();
                iter_range = in_expression.right.clone();
                if let NodeType::Identifier(_) = in_expression.left.node.as_ref() { } else {
                    self.error(line!(), "Unable to parse `for` statement, incorrect value for iterator", "Expected a single identifier token as the iterator value for the `for` statement: `for VALUE in RANGE {}`", &in_expression.operator.location);
                    return ASTNode::err();
                }
                if in_expression.operator.token_type != TokenType::In {
                    self.error(line!(), "Unable to parse `for` statement, incorrect operator", "Expected an `in` operator for `for` statement: `for X in Y {}`", &in_expression.operator.location);
                    return ASTNode::err();
                } 
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect operator", "Expected an `in` operator for `for` statement: `for X in Y {}`", &for_token.location);
                return ASTNode::err();
            }

            let node = ForEachLoop { 
                index_segment,
                iter_value,
                iter_range,
                body
            };

            return ASTNode { 
                token: for_token.clone(), 
                node: Box::new(NodeType::ForEach(node)) 
            };
        } else if segments.len() == 3 || segments.len() == 4 {
            let mut index_segment = None;
            if segments.len() == 4 {
                if let NodeType::Identifier(ref v) = segments[0].node.as_ref() {
                    index_segment = Some(v.clone());
                } else {
                    self.error(line!(), "Unable to parse `for` statement, incorrect indexer expression", "Expected a single identifier token as the indexer for the `for` statement: `for INDEX, SET, CONDITION, INCREMENT {}`", &for_token.location);
                }
            }
            let start = if segments.len() == 3 {0} else {1};

            let set_segment;
            let condition_segment;
            let increment_segment;
            if let NodeType::VariableDeclaration(_) = segments[start].node.as_ref() {
                set_segment = segments[start].clone();
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect set expression", "Expected a variable declaration for the set segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                return ASTNode::err();
            }
            if let NodeType::Operator(ref v) = segments[start + 1].node.as_ref() {
                if !v.operator.token_type.operator_boolean() {
                    self.error(line!(), "Unable to parse `for` statement, incorrect condition expression", "Expected a valid expression for the condition segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                    return ASTNode::err();
                }
                condition_segment = segments[start + 1].clone();
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect condition expression", "Expected a valid expression for the condition segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                return ASTNode::err();
            }
            if let NodeType::Operator(ref v) = segments[start + 2].node.as_ref() {
                if v.operator.token_type != TokenType::Assign {
                    self.error(line!(), "Unable to parse `for` statement, incorrect increment expression", "Expected a assignment expression for the increment segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                    return ASTNode::err();
                }
                increment_segment = Box::new(ASTNode { 
                    token: v.operator.clone(), 
                    node: Box::new(NodeType::Assignment(Assignment { 
                        left: v.left.clone(), 
                        right: v.right.clone() 
                    })) 
                });
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect increment expression", "Expected a assignment expression for the increment segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                return ASTNode::err();
            }

            let node = ForLoop { 
                index_segment,
                condition_segment,
                increment_segment,
                set_segment,
                body
            };

            return ASTNode { 
                token: for_token.clone(), 
                node: Box::new(NodeType::For(node)) 
            };
        } else {
            self.error(line!(), "Could not parse for statement", "Too many parameters were found for the for statement: `for X in Y { ... }` or `for VAR, CONDITION, INC { ... }`", &tokens[i].location);
            return ASTNode::err();
        }
    }

    fn parse_conditional_statement(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ConditionalRegion {
        let is_while = tokens[*i].token_type == TokenType::While;
        let (condition, body) = self.get_condition_and_body_for_if(tokens, i);

        let mut else_region: Option<BodyRegion> = None;
        let mut else_if_regions: Option<Vec<Box<ConditionalRegion>>> = None;

        while tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Else) {
            Self::inc(i);
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
            } else if else_region.is_none() {
                // else
                else_region = Some(self.get_code_block(tokens, i, false));
            } else {
                // double else
                self.error(line!(), "Else statement is by itself", "Else statement must be after if statement: `if EXPR {} else {}`", &tokens[*i - 1].location);
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

    fn function_call(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, last_punc: Option<ScopeType>, is_in_statement: bool) -> ScopedIdentifier {
        let mut scope = scope;
        Self::dec(i);

        let mut node_parameters: Vec<Box<ASTNode>> = vec![];
        let mut type_parameters: Vec<Box<ASTNode>> = vec![];
        let name = tokens[*i].clone();

        if let Some(next_token) = tokens.clone().get(*i + 1) {
            if next_token.token_type == TokenType::LessThan {
                Self::inc(i);
                type_parameters = self.get_type_parameters(tokens, i).parameters;
                Self::dec(i);
            }

            if tokens.get(*i + 1).is_some() && tokens[*i + 1].token_type == TokenType::LParen {
                Self::inc(i);
                node_parameters = self.get_node_parameters(tokens, i, Self::PARSING_FOR_FUNCTION);
            }
            else {
                self.error(line!(), "Invalid token for funciton call", format!("Expected either `<` or `(` for function call, got: `{}`", next_token.value).as_str(), &tokens[*i].location);
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
            Self::inc(i);

            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);

            scope.scope.push(Identifier {
                expression: chained_expression.clone(),
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });

            scope
        }
        else if tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::LBracket { // is indexing
            scope = self.get_indexer_expression(tokens, Some(scope), i, is_in_statement);
            Self::inc(i);
            scope
        }
        else {
            //Self::dec(i);
            scope
        }
    }

    fn scope_call(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, is_in_statement: bool) -> ScopedIdentifier {
        Self::dec(i);

        let mut last_punc = None;
        let (mut scope, valid_lt_as_type_parameter) = self.get_ident_scope(tokens, i, &mut last_punc); // `valid_lt_as_type_parameter`
        Self::inc(i);

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) && valid_lt_as_type_parameter {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) && !is_in_statement {
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
            scope = self.get_indexer_expression(tokens, Some(scope), i, is_in_statement)
        }

        Self::dec(i);
        return scope
    }

    fn scope_call_with_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, mut scope: ScopedIdentifier, last_punc: Option<ScopeType>, is_in_statement: bool) -> ScopedIdentifier {
        Self::inc(i);

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc)
        }
        else if tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::Dot) && tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && (tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen) || tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LessThan)) {
            Self::inc(i);
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
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
            Self::inc(i);
        }

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope = self.get_indexer_expression(tokens, Some(scope), i, is_in_statement)
        }

        Self::dec(i);
        return scope
    }
    
    fn get_ternary(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, first_part: Box<ASTNode>, inside_statement: bool) -> NodeType {
        Self::inc(i);

        let second_part = self.get_expression(tokens, i, Self::PARSING_FOR_TERNARY);
        Self::inc(i);
        let third_part = self.get_expression(tokens, i, if inside_statement { Self::PARSING_FOR_STATEMENT } else { Self::PARSING_FOR_TERNARY });

        NodeType::TernaryOperator(TernaryConditional {
            condition: first_part,
            then: second_part,
            else_then: third_part,
        })
    }
    
    fn get_indexer_expression(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, is_in_statement: bool) -> ScopedIdentifier {
        let array_nodes = self.get_node_parameters(tokens, i, Self::PARSING_FOR_ARRAY);

        if scope.is_none() {
            self.error(line!(), "Couldn't parse indexing", "Expected an indexer, but no object was provided for the index", &tokens[*i].location);
        }

        let mut scope = scope.unwrap();
        let pop_wrapped = scope.scope.pop();

        if pop_wrapped.is_none() {
            self.error(line!(), "Couldn't parse indexing", "Expected an indexer, but no object was provided for the index", &tokens[*i].location);
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

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) {
            scope = self.get_indexer_expression(tokens, Some(scope), i, is_in_statement);
            went_through_lbracket = true;
        }
        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
            *i += 2;
            let call = self.scope_call(tokens, i, is_in_statement);
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
            Self::dec(i);
        }

        ScopedIdentifier {
            scope: scope.scope
        }
    }

    fn get_array_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedIdentifier {
        let first_token = tokens[*i].clone();
        let array_nodes = self.get_node_parameters(tokens, i, Self::PARSING_FOR_ARRAY);

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
        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
            Self::inc(i);
            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);
            scope.scope.push(Identifier {
                expression: chained_expression,
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });
        }

        scope
    }

    fn get_object_instantiation(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, last_punc: Option<ScopeType>) -> ScopedIdentifier {
        let name_token = tokens.get(*i - 1).unwrap_or(&self.try_get_last_token(tokens)).clone();
        let scope_is_empty = scope.is_none(); // because if not, it's coming from scope_call_with_scope and there is another `Parser::dec(i)` in there that will cause issues
        
        if self.__curent_parsing_line >= self.lines.len() {
            self.error(line!(), "Couldn't parse object instantiation", "Expected closing brace for object instantiation: `NAME { PROPERTY = VALUE, }`", &tokens[*i].location);
        }
        self.__curent_parsing_line += 1;
        *tokens = self.lines[self.__curent_parsing_line].clone();
        *i = 0;

        let raw_properties = self.get_node_parameters(tokens, i, Self::PARSING_FOR_OBJECT_INSTANTIATION);

        let mut properties = vec![];
        for p in raw_properties {
            if ASTNode::err() == *p { continue; }
            if let NodeType::Operator(ref value) = *p.node {
                if value.operator.token_type == TokenType::Assign {
                    if let NodeType::Identifier(ref name) = *value.left.node {
                        let name = name.clone();
                        let value = value.right.clone();
                        properties.push(NodeProperty { name, value });
                    } else {
                        self.error(line!(), "Invalid property assignment for object instantiation", "Expected single identifier for property in object instantiation: `NAME { PROPERTY = VALUE, }`", &p.token.location);
                        break;
                    }
                } else {
                    self.error(line!(), "Invalid property assignment for object instantiation", format!("Expected `=` for object instantiation property assignment, but got `{}`", value.operator.value).as_str(), &value.operator.location);
                    break;
                }
            } else {
                self.error(line!(), "Invalid property assignment for object instantiation", "Expected `=` for object instantiation property assignment: `NAME { PROPERTY = VALUE, }`", &name_token.location);
                break;
            }
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

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) || self.lines.get(self.__curent_parsing_line + 1).map_or(false, |l| l.get(0).map_or(false, |t| t.token_type == TokenType::Dot)) {
            if self.lines.get(self.__curent_parsing_line + 1).map_or(false, |l| l.get(0).map_or(false, |t| t.token_type == TokenType::Dot)) {
                self.__curent_parsing_line += 1;
                *tokens = self.lines[self.__curent_parsing_line].clone();
                *i = 1;
            } else {
                Self::inc(i);
            }

            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);
            scope.scope.push(Identifier {
                expression: chained_expression,
                scope_type: Some(ScopeType::Dot),
                type_parameters: None
            });

            if scope_is_empty {
                Self::dec(i);
            }
            scope
        } else {
            if tokens.get(*i).is_none() && self.lines.get(self.__curent_parsing_line + 1).is_some() {
                self.__curent_parsing_line += 1;
                *tokens = self.lines[self.__curent_parsing_line].clone();
                *i = Parser::SET_I_TO_ZERO;
            } else if scope_is_empty {
                Self::dec(i);
            }
            scope
        }
    }
    
    fn check_and_return_tuple(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> (bool, Box<ASTNode>) {
        let first_token = tokens[*i].clone();
        let mut j = i.clone();
        let current_tokens = tokens.clone();
        let current_line = self.__curent_parsing_line;
        let current_output = self.output.messages.clone();

        let nodes = self.get_node_parameters(tokens, &mut j, Self::PARSING_FOR_FUNCTION);
        let is_tuple = nodes.len() > 1;

        let tuple = Box::new(ASTNode {
            token: first_token,
            node: Box::new(NodeType::TupleExpression(NodeParameters {
                parameters: nodes
            })),
        });

        if is_tuple {
            *i = j.checked_sub(1).unwrap_or(j);
        } else {
            self.__curent_parsing_line = current_line;
            *tokens = current_tokens;
            self.output.messages = current_output;
        }

        (is_tuple, tuple)
    }

    fn try_get_last_token(&self, tokens: &Vec<Box<Token>>) -> Box<Token> {
        let mut catch = 0;
        let mut current_line = self.__curent_parsing_line;
        let mut token = tokens.get(0);
        while token.is_none() {
            current_line -= 1;
            token = self.lines[current_line].get(0);

            catch += 1;
            if catch > 100 {
                break;
            }
        }
        return token.unwrap_or(&Box::new(Token::new_empty())).clone();
    }

    fn get_code_block(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, last_is_return: bool) -> BodyRegion {
        if self.lines.get(self.__curent_parsing_line + 1).is_some() {
            self.__curent_parsing_line += 1;
            *tokens = self.lines[self.__curent_parsing_line].clone();
            *i = 0;
        } else {
            let token = self.try_get_last_token(tokens);
            self.error(line!(), "Code block not closed", "Expected `}` to close code block, but found end of file", &token.location);
            return BodyRegion { body: vec![] };
        }

        let mut body = self.get_node_parameters(tokens, i, Self::PARSING_FOR_CODE_BLOCK);

        if last_is_return {
            if let Some(last) = body.last_mut() {
                if let NodeType::ReturnExpression(_) = last.node.as_ref() {
                    // nothing
                } else {
                    last.node = Box::new(NodeType::ReturnExpression(Box::new(ASTNode {
                        token: last.token.clone(),
                        node: last.as_ref().node.clone(),
                    })));
                }
            }
        }
        BodyRegion {
            body
        }
    }

    fn get_entire_expression(&mut self, tokens: &mut Vec<Box<Token>>) -> Box<ASTNode> {
        let mut __ = Parser::SET_I_TO_ZERO;
        self.get_expression(tokens, &mut __, Self::NORMAL_PARSING)
    }

    const SET_I_TO_ZERO: usize = usize::MAX - u8::MAX as usize; // if `i` is equal to this, set `i` to 0 instead of incrementing

    const NORMAL_PARSING: u8 = 0;
    const PARSING_FOR_FUNCTION: u8 = 1;
    const PARSING_FOR_FUNCTION_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::RParen];
    const PARSING_FOR_INSIDE_PARENTHESIS: u8 = 2;
    const PARSING_FOR_INSIDE_PARENTHESIS_UNTIL: [TokenType; 1] = [TokenType::RParen];
    const PARSING_FOR_ARRAY: u8 = 3;
    const PARSING_FOR_ARRAY_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::RBracket];
    const PARSING_FOR_TERNARY: u8 = 4;
    const PARSING_FOR_TERNARY_UNTIL: [TokenType; 1] = [TokenType::Colon];
    const PARSING_FOR_OBJECT_INSTANTIATION: u8 = 5;
    const PARSING_FOR_OBJECT_INSTANTIATION_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::RBrace];
    const PARSING_FOR_CODE_BLOCK: u8 = 6;
    const PARSING_FOR_CODE_BLOCK_UNTIL: [TokenType; 1] = [TokenType::RBrace];
    const PARSING_FOR_STATEMENT: u8 = 7;
    const PARSING_FOR_STATEMENT_UNTIL: [TokenType; 1] = [TokenType::LBrace];
    const PARSING_FOR_SCOPING: u8 = 8;
    const PARSING_FOR_FOR_LOOP_STATEMENT: u8 = 9;
    const PARSING_FOR_FOR_LOOP_STATEMENT_UNTIL: [TokenType; 1] = [TokenType::Comma];
    const PARSING_FOR_MATCH_CASE: u8 = 10;
    const PARSING_FOR_MATCH_CASE_UNTIL: [TokenType; 1] = [TokenType::DoubleArrow];
    const PARSING_FOR_MATCH_STATEMENT: u8 = Self::PARSING_FOR_OBJECT_INSTANTIATION;
    const PARSING_FOR_SHABANG: u8 = 11;
    const PARSING_FOR_SHABANG_UNTIL: [TokenType; 1] = [TokenType::RightArrow];
    const PARSING_FOR_TYPE_CONSTRAINT: u8 = 12;
    const PARSING_FOR_TYPE_CONSTRAINT_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::GreaterThan];

    fn get_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, until: u8) -> Box<ASTNode> {
        let mut expr_stack: Vec<Box<ASTNode>> = Vec::new();
        let mut op_stack: Vec<Box<Token>> = Vec::new();
        let mut last_was_ident = false;
        let mut last_was_unary_operator: bool = false;
        let mut is_1_expression = until == Self::NORMAL_PARSING;
        let start_index = *i;
        
        if *i == Self::SET_I_TO_ZERO {
            is_1_expression = false;
            *i = 0;
        }

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            let mut inc_i = true;

            if until != 0 {
                if Self::PARSING_FOR_FUNCTION == until && Self::PARSING_FOR_FUNCTION_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                } else if Self::PARSING_FOR_INSIDE_PARENTHESIS == until && Self::PARSING_FOR_INSIDE_PARENTHESIS_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                } else if Self::PARSING_FOR_ARRAY == until && Self::PARSING_FOR_ARRAY_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                } else if Self::PARSING_FOR_TERNARY == until && Self::PARSING_FOR_TERNARY_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                } else if Self::PARSING_FOR_OBJECT_INSTANTIATION == until && Self::PARSING_FOR_OBJECT_INSTANTIATION_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                } else if Self::PARSING_FOR_CODE_BLOCK == until && Self::PARSING_FOR_CODE_BLOCK_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                } else if Self::PARSING_FOR_STATEMENT == until && Self::PARSING_FOR_STATEMENT_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                } else if Self::PARSING_FOR_SCOPING == until && matches!(&token.token_type, operator_tokens!() | TokenType::LBrace) {
                    break;
                } else if Self::PARSING_FOR_FOR_LOOP_STATEMENT == until && Self::PARSING_FOR_FOR_LOOP_STATEMENT_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                } else if Self::PARSING_FOR_MATCH_CASE == until && Self::PARSING_FOR_MATCH_CASE_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::inc(i);
                    break;
                } else if Self::PARSING_FOR_SHABANG == until && Self::PARSING_FOR_SHABANG_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::inc(i);
                    break;
                } else if Self::PARSING_FOR_TYPE_CONSTRAINT == until && Self::PARSING_FOR_TYPE_CONSTRAINT_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                }
            }

            if token.token_type == TokenType::Colon || token.token_type.is_access_modifier() {
                if token.token_type.is_access_modifier() {
                    // set `i` equal to the `:` to match the `token.token_type == TokenType::Colon` match
                    if let Some(index) = tokens.iter().skip(*i).position(|x| x.token_type == TokenType::Colon) {
                        *i = index + *i;
                    } else {
                        self.error(line!(), "Unexpected token", "Didn't expect an access modifier here. Expects a declaration", &token.location);
                    }
                }

                // the parser has to decide if this is variable declaration or a function declaration
                // if the next token is an identifier and then the next after that is either a left parenthesis or a left angle bracket, then it's a function declaration.
                // else if the next token is an identifier and the next after that is an equal sign, then it's a variable declaration.
                // else it's probobly apart of a ternary expression and should break out of the loop.
                Self::inc(i); 

                if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::LParen || t.token_type == TokenType::LessThan) {
                    // it's a function declaration
                    
                    // remove any errors that occured before the variable declaration.
                    self.output.messages.retain(|x| x.location.line != token.location.line);

                    let function = self.declaring_function(tokens, i);
                    if function != ASTNode::err() {
                        if expr_stack.len() > 1 {
                            self.error(line!(), "Function declaration error", "Unable to parse funcion declaration, there is more than one expression in type declaration", &token.location);
                        }
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(function));
                    } else {
                        self.error(line!(), "Function declaration error", "Unable to parse function declaration", &token.location);
                    }

                    break;

                } else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Assign) {
                    // it's a variable declaration
                    
                    // remove any errors that occured before the variable declaration.
                    self.output.messages.retain(|x| x.location.line != token.location.line);

                    // is apart of a for loop: for i32: i = 0, ..., ... {}
                    let is_in_for = tokens.iter().take(*i).any(|t| t.token_type == TokenType::For);

                    // parse the variable
                    Self::inc(i); // is on `=`
                    Self::inc(i); // is on first token of expression
                    let variable = self.declaring_variable(tokens, i, is_in_for);
                    if variable != ASTNode::err() {
                        if expr_stack.len() > 1 {
                            self.error(line!(), "Variable declaration error", "Unable to parse variable declaration expression, there is more than one expression in type declaration", &token.location);
                        }
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(variable));
                    } else {
                        self.error(line!(), "Variable declaration error", "Unable to parse variable declaration expression", &token.location);
                    }
                    if !is_in_for {
                        if self.__curent_parsing_line >= self.lines.len() {
                            break;
                        }
                        self.__curent_parsing_line += 1;
                        *tokens = self.lines[self.__curent_parsing_line].clone();
                        *i = Parser::SET_I_TO_ZERO;
                    }
                    break;
                } else {
                    Self::dec(i);
                    break;
                }
            } else if token.token_type.is_constant() {
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
                    self.error(line!(), "Could not parse constant", "Couldn't decide type from constant", &token.location);
                    return Box::new(ASTNode::err());
                }

                expr_stack.push(node);
            } else if token.token_type == TokenType::Identifier {
                last_was_unary_operator = false;
                if tokens.get(*i + 1).is_some() && (tokens[*i + 1].token_type == TokenType::DoubleColon || (tokens[*i + 1].token_type == TokenType::LBrace && (until != Self::PARSING_FOR_STATEMENT && until != Self::PARSING_FOR_SCOPING)) || tokens[*i + 1].token_type == TokenType::Dot || tokens[*i + 1].token_type == TokenType::LParen || tokens[*i + 1].token_type == TokenType::LessThan) && !(until != 0 && (until == Self::PARSING_FOR_FUNCTION && Self::PARSING_FOR_FUNCTION_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type) || until == Self::PARSING_FOR_INSIDE_PARENTHESIS && Self::PARSING_FOR_INSIDE_PARENTHESIS_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type))) {
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
                                    last_was_ident = tokens.get(j + 1).map_or(false, |t| 
                                        t.token_type == TokenType::LParen || 
                                        t.token_type == TokenType::DoubleColon || 
                                        t.token_type == TokenType::Dot || 
                                        { // is apart of variable declaration.
                                            let position_of_colon = tokens.iter().position(|a| a.token_type == TokenType::Colon);
                                            if let Some(pos) = position_of_colon {
                                                pos > *i && tokens.get(pos + 2).map_or(false, |a| a.token_type == TokenType::Assign)
                                            } else {
                                                false
                                            }
                                        }
                                    );
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
                            expr_stack.push(node);
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
                    expr_stack.push(node);
                }
            } else if token.token_type.is_operator() {
                // To check for unary operators, the operator must be the first token in the expression, or the previous token must be an operator: `-0` or `0+-1`
                let mut handle_as_operator = true;

                // check if part of assignment
                if !matches!(until, Self::PARSING_FOR_STATEMENT | Self::PARSING_FOR_OBJECT_INSTANTIATION) {
                    if token.token_type == TokenType::Assign {
                        let mut copy_op_stack: Vec<Box<Token>> = op_stack.clone();
                        let mut copy_expr_stack = expr_stack.clone();
                        let left = self.expression_stacks_to_ast_node(&mut copy_op_stack, &mut copy_expr_stack);
                        let left = left.unwrap_or_else(|| {
                            self.error(line!(), "Error in parsing left operandS", "Assignment operator must have a left operand: `a = b`", &token.location);
                            Box::new(ASTNode::err())
                        });
                        
                        Self::inc(i);
                        let right = self.get_expression(tokens, i, Self::NORMAL_PARSING);
    
                        let assign = Assignment { left, right };
                        
                        if expr_stack.len() > 1 {
                            self.error(line!(), "Variable declaration error", "Unable to parse variable declaration expression, there is more than one expression in type declaration", &token.location);
                        }
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(ASTNode{
                            token: token.clone(),
                            node: Box::new(NodeType::Assignment(assign))
                        }));
                        if self.__curent_parsing_line >= self.lines.len() {
                            break;
                        }
                        self.__curent_parsing_line += 1;
                        *tokens = self.lines[self.__curent_parsing_line].clone();
                        *i = Parser::SET_I_TO_ZERO;
                        break;
                    }
                }
                // check if it is actually part of a variable declaration
                if last_was_ident {
                    let assign_position = tokens.iter().rposition(|t| t.token_type == TokenType::Assign);
                    if let Some(position) = assign_position {
                        if position > 2 && tokens.get(position - 1).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get(position - 2).map_or(false, |t| t.token_type == TokenType::Colon) {
                            // part of variable declaration: TYPE: NAME = VALUE
                            *i = position - 2;
                            continue;
                        }
                    }
                }

                if last_was_ident && token.token_type == TokenType::LessThan && tokens.get(*i - 1).map_or(false, |t: &Box<Token>| t.token_type == TokenType::Identifier) {
                    // part of function call: function<TYPE>()
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i, until == Self::PARSING_FOR_STATEMENT))),
                    }));
                    handle_as_operator = false;
                }
                else if last_was_unary_operator {
                    handle_as_operator = false;
                }
                else if token.token_type.is_unary_operator() && tokens.get(*i + 1).is_some().then(|| !tokens[*i + 1].token_type.is_operator()).unwrap_or(false) {
                    handle_as_operator = false;
                    if (expr_stack.is_empty() || (is_1_expression && *i == start_index)) || (*i > 0 && tokens.get(*i - 1).is_some().then(|| tokens[*i - 1].token_type.is_operator() || tokens[*i - 1].token_type == TokenType::LParen || tokens[*i - 1].token_type == TokenType::LBracket || tokens[*i - 1].token_type == TokenType::Comma || tokens[*i - 1].token_type == TokenType::LBrace).unwrap_or(false)) {
                        last_was_unary_operator = false;

                        Self::inc(i);
                        let next_expression = self.get_expression(tokens, i, until);
                        if let NodeType::VariableDeclaration(_) = next_expression.node.as_ref() {
                            expr_stack.clear();
                            op_stack.clear();
                            expr_stack.push(next_expression);
                            break;
                        }
                        else {
                            if !matches!(until, Self::PARSING_FOR_FUNCTION | Self::PARSING_FOR_ARRAY | Self::PARSING_FOR_OBJECT_INSTANTIATION | Self::PARSING_FOR_CODE_BLOCK | Self::PARSING_FOR_MATCH_CASE) {
                                Self::dec(i);
                            }
    
                            let unary = UnaryExpression {
                                operator: token.clone(),
                                operand: next_expression.clone(),
                            };
    
                            expr_stack.push(Box::new(ASTNode {
                                token: token.clone(),
                                node: Box::new(NodeType::UnaryOperator(unary)),
                            }));
                        }
                    }
                    else {
                        handle_as_operator = true;
                    }
                } 
                else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::As || t.token_type == TokenType::Is) {
                    Self::inc(i);
                    let ty = self.get_type_idententifier(tokens, i, true);
                    Self::dec(i);
                    expr_stack.push(ty);
                } 

                last_was_ident = false;
                if handle_as_operator {
                    last_was_unary_operator = false;
                    while let Some(top_op) = op_stack.last() {
                        if token.token_type.precedence() <= top_op.token_type.precedence() {
                            let operator = op_stack.pop().unwrap_or_else(|| {
                                self.error(line!(), "Couldn't parse expression", "Operator stack is empty", &token.location);
                                return Box::new(Token::new_empty());
                            });
                            let right = expr_stack.pop().unwrap_or_else(|| {
                                self.error(line!(), "Couldn't parse expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return Box::new(ASTNode::err());
                            });
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                self.error(line!(), "Couldn't parse expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return Box::new(ASTNode::err());
                            });

                            let node: Box<ASTNode> = Box::new(ASTNode {
                                token: operator.clone(),
                                node: Box::new(NodeType::Operator(Expression{
                                    left,
                                    right,
                                    operator
                                })),
                            });

                            expr_stack.push(node);
                        } else {
                            break;
                        }
                    }
                    op_stack.push(token.clone());
                }
            } else if token.token_type == TokenType::DoubleColon {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push(Box::new(ASTNode {
                        token: tokens[if *i >= 1 { *i - 1 } else { *i }].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i, until == Self::PARSING_FOR_STATEMENT))),
                    }));
                }
                else {
                    self.error(line!(), "Expected identifier before double colon", "Expected identifier before double colon: `IDENT::IDENT`", &token.location);
                }
                last_was_ident = false;
            } else if token.token_type == TokenType::Dot {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push(Box::new(ASTNode {
                        token: tokens[if *i >= 1 { *i - 1 } else { *i }].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i, until == Self::PARSING_FOR_STATEMENT))),
                    }));
                }
                else {
                    self.error(line!(), "Expected identifier before dot", "Expected a correct expression before scoping: `expression.member`", &token.location);
                }
                last_was_ident = false;
            } else if token.token_type == TokenType::LParen {
                last_was_unary_operator = false;
                if last_was_ident {
                    last_was_ident = false;
                    // part of function call
                    expr_stack.push(Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i, until == Self::PARSING_FOR_STATEMENT))),
                    }));
                }
                else {
                    let (is_tuple, tuple) = self.check_and_return_tuple(tokens, i);
                    if is_tuple { // is a tuple
                        if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                            // scope traversal
                            let tuple_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: tuple,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };
                            Self::inc(i);

                            let scope = self.scope_call_with_scope(tokens, i, tuple_scope, Some(ScopeType::Dot), until == Self::PARSING_FOR_STATEMENT);

                            expr_stack.push(Box::new(ASTNode {
                                token: tokens[*i - 1].clone(),
                                node: Box::new(NodeType::ScopedExpression(scope)),
                            }));
                        }
                        else {
                            expr_stack.push(tuple);
                        }

                        last_was_ident = false;
                    }
                    else { // not a tuple
                        // if not then it is apart of expression 
                        Self::inc(i);

                        // this parses weird
                        // function( (a + b).c )   parses as:   function( (a + b) ).c
                        // this is because it parses until `)` and then removes 1 from the `i`.
                        // so `i` would be at `b`. So when it checks for `i + 1` it would be at `)` and not `.`
                        // to fix this, we need to instead use an `until` parsing, use a enum where it would parse for function or for expression

                        let node = self.get_expression(tokens, i, Self::PARSING_FOR_INSIDE_PARENTHESIS);

                        if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::Dot) {
                            // scope traversal
                            Self::inc(i);
                            let parenthesis_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: node,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };

                            let scope = self.scope_call_with_scope(tokens, i, parenthesis_scope, Some(ScopeType::Dot), until == Self::PARSING_FOR_STATEMENT);

                            expr_stack.push(Box::new(ASTNode {
                                token: tokens[*i - 1].clone(),
                                node: Box::new(NodeType::ScopedExpression(scope)),
                            }));
                        }
                        else {
                            expr_stack.push(node);
                        }
                    }
                }
            } else if token.token_type == TokenType::RParen {
                break;
            } else if token.token_type == TokenType::LBracket {
                if last_was_ident {
                    // indexing
                    let temp_scope = ScopedIdentifier {
                        scope: vec![Identifier {
                            expression: expr_stack.pop().unwrap_or(
                                Box::new(ASTNode {
                                    token: tokens[*i - 1].clone(),
                                    node: Box::new(NodeType::Identifier(tokens[*i - 1].clone())),
                                })),
                            scope_type: None,
                            type_parameters: None
                        }]
                    };
                    let indexing_expresion = self.get_indexer_expression(tokens, Some(temp_scope), i, until == Self::PARSING_FOR_STATEMENT);
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(indexing_expresion)),
                    }));
                }
                else {
                    // array expression
                    let array_expression = self.get_array_expression(tokens, i);
                    inc_i = false;
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(array_expression)),
                    }));
                }
            } else if token.token_type == TokenType::RBracket {
                //self.error(line!(), "Missing delimeter", "Expected opening bracket `[`", &tokens[*i].location);
                break;
            } else if token.token_type == TokenType::LBrace {
                if last_was_ident {
                    let expr_token = tokens[*i].clone();
                    let obj_instantiation = self.get_object_instantiation(tokens, None, i, None);

                    expr_stack.push(Box::new(ASTNode {
                        token: expr_token,
                        node: Box::new(NodeType::ScopedExpression(obj_instantiation))
                    }));
                }
                else {
                    // code block
                    let code_block = self.get_code_block(tokens, i, false);
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::CodeBlock(code_block)),
                    }));
                    if tokens.len() >= *i && self.__curent_parsing_line + 1 < self.lines.len() {
                        if self.lines[self.__curent_parsing_line + 1].get(0).map_or(false, |t| t.token_type.is_operator() || matches!(t.token_type, TokenType::Dot | TokenType::LBrace | TokenType::LBracket | TokenType::LParen | TokenType::RParen | TokenType::RBracket | TokenType::RBrace)) {
                            self.__curent_parsing_line += 1;
                            *i = 0;
                            *tokens = self.lines[self.__curent_parsing_line].clone();
                            inc_i = false;
                        }
                    }
                }
            } else if token.token_type == TokenType::RBrace {
                //self.error(line!(), "Missing delimeter", "Expected opening brace `{`", &tokens[*i].location);
                break;
            } else if token.token_type == TokenType::QuestionMark {
                // ternary operator a ? b : c

                let left_operand = expr_stack.pop().unwrap_or_else(|| {
                    self.error(line!(), "Ternary operator has no left operand", "Ternary operator must have a left operand: `a ? b : c`", &token.location);
                    return Box::new(ASTNode::err());
                });
                
                let ternary = self.get_ternary(tokens, i, left_operand.clone(), until == Self::PARSING_FOR_STATEMENT);

                if *i > 0 && tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::RParen || t.token_type == TokenType::RBracket || t.token_type == TokenType::RBrace) {
                    Self::dec(i);
                }
                Self::dec(i);

                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(ternary),
                }));
            } else if token.token_type == TokenType::DoubleArrow {
                let lambda = self.get_lambda(tokens, i, &mut expr_stack, &mut inc_i);

                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::LambdaExpression(lambda)),
                }));
            } else if token.token_type == TokenType::Underscore {
                expr_stack.push(Box::new(ASTNode {
                    node: Box::new(NodeType::Discard(token.clone())),
                    token
                }));
            } else if token.token_type == TokenType::Break {
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::Break(token.clone()))
                }))
            } else if token.token_type == TokenType::Continue {
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::Continue(token.clone()))
                }))
            } else if token.token_type == TokenType::Return {
                Self::inc(i);
                let return_node = self.get_expression(tokens, i, Self::NORMAL_PARSING);
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::ReturnExpression(return_node))
                }));
            } else if token.token_type == TokenType::If {
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::If(self.parse_conditional_statement(tokens, i))),
                }))
            } else if token.token_type == TokenType::While {
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::While(self.parse_conditional_statement(tokens, i))),
                }))
            } else if token.token_type == TokenType::For {
                expr_stack.push(Box::new(self.parse_for(tokens)));
            } else if token.token_type == TokenType::Match {
                expr_stack.push(Box::new(self.parse_match(tokens, i)));
            } else if token.token_type == TokenType::Else {
                self.error(line!(), "Else statement is by itself", "Else statement must be after if statement: `if EXPR {} else {}`", &tokens[0].location);
                return Box::new(ASTNode::err());
            } else if token.token_type == TokenType::Shabang {
                if *i != 0 {
                    self.error(line!(), "Unexpected Shabang token", "Shabang must be at the start of a line: `#! ...`", &token.location);
                    return Box::new(ASTNode::err());
                }
                Self::inc(i);
                if tokens.len() == 1 {
                    self.error(line!(), "Unexpected Shabang token", "Shabang doesn't have any content: `#! ...`", &token.location);
                    return Box::new(ASTNode::err());
                }
                match tokens[1].value.to_lowercase().as_str() {
                    "crumb" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Crumb))
                        });
                    }
                    "deprecated" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Depricated))
                        });
                    }
                    "else" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Else)),
                        });
                    }
                    "endif" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::EndIf)),
                        });
                    }
                    _ => {
                        if tokens.len() == 2 {
                            return Box::new(ASTNode {
                                token: token.clone(),
                                node: Box::new(NodeType::Shabang(ShabangType::Other(tokens.iter().skip(1).map(|t| t.clone()).collect()))),
                            });
                        } 
                    }
                }
                
                match tokens[1].value.to_lowercase().as_str() {
                    "allow" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Allow(tokens[2].clone()))),
                        });
                    }
                    "warn" | "warning" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Warning(tokens[2].clone()))),
                        });
                    }
                    "err" | "error" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Err(tokens[2].clone()))),
                        });
                    }
                    "ifndef" | "ifndefine" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::IfNotDefined(tokens[2].clone()))),
                        });
                    }
                    "ifdef" | "ifdefine" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::IfDefined(tokens[2].clone()))),
                        });
                    }
                    "once" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Once(tokens[2].clone()))),
                        });
                    }
                    "define" | "def" if tokens.len() == 3 => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::DefineFile(tokens[2].clone()))),
                        });
                    }
                    "define" | "def" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Define(self.get_expression(tokens, i, Self::PARSING_FOR_SHABANG), self.get_expression(tokens, i, Self::NORMAL_PARSING)))),
                        });
                    }
                    "if" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::If(self.get_expression(tokens, i, Self::NORMAL_PARSING)))),
                        });
                    }
                    "ifn" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::IfNot(self.get_expression(tokens, i, Self::NORMAL_PARSING)))),
                        });
                    }
                    "elif" | "elseif" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::ElseIf(self.get_expression(tokens, i, Self::NORMAL_PARSING)))),
                        });
                    }
                    "undef" => {
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Undef(tokens[2].clone()))),
                        });
                    }
                    _ => { 
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shabang(ShabangType::Other(tokens.iter().skip(1).map(|t| t.clone()).collect()))),
                        });
                    }
                }
            } else if token.token_type == TokenType::Comma {
                //self.error(line!(), "Unexpected token in expression", "Didn't expect this token in expression. Maybe you meant to use a comma in a tuple `(val1, val2)`, if that's so, put inside parentheises", &token.location);
                //return Box::new(ASTNode::err());
                break;
            } 
            else {
                self.error(line!(), "Unexpected token in expression", "Didn't expect this token in expression", &token.location);
                return Box::new(ASTNode::err());
            }

            if inc_i && *i != Self::SET_I_TO_ZERO {
                Self::inc(i);
            } else if *i == Self::SET_I_TO_ZERO {
                *i = 0;
            }
        }

        let expression_node = self.expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack);

        return expression_node.unwrap_or(Box::new(ASTNode::err()));
    }

    fn expression_stacks_to_ast_node(&mut self, op_stack: &mut Vec<Box<Token>>, expr_stack: &mut Vec<Box<ASTNode>>) -> Option<Box<ASTNode>> {
        while let Some(operator) = op_stack.pop() {
            if operator.token_type == TokenType::LParen && expr_stack.len() < 2 {
                break;
            }

            let right = expr_stack.pop();
            let left = expr_stack.pop();

            let node = Box::new(ASTNode {
                token: operator.clone(),
                node: Box::new(NodeType::Operator(Expression {
                    left: left.unwrap_or_else(|| {
                        self.error(line!(), "Couldn't parse expression", "Left hand side of expression is empty", &operator.location);
                        Box::new(ASTNode::err())
                    }),
                    right: right.unwrap_or_else(|| {
                        self.error(line!(), "Couldn't parse expression", "Right hand side of expression is empty", &operator.location);
                        Box::new(ASTNode::err())
                    }),
                    operator: operator.clone(),
                })),
            });

            expr_stack.push(node);
        }

        if expr_stack.len() != 1 && expr_stack.len() > 0 {
            self.error(line!(), "Couldn't parse expression", "The expression does not have a root. This usually occurs with the typo: `a b`", &expr_stack.last().unwrap().token.location);
        }

        return expr_stack.pop()
    }

    fn get_lambda(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, expr_stack: &mut Vec<Box<ASTNode>>, inc_i: &mut bool) -> LambdaExpression {
        let token = tokens[*i].clone();

        if expr_stack.is_empty() {
            self.error(line!(), "Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
        }
        let parameters_node = expr_stack.pop().unwrap_or_else(|| {
            self.error(line!(), "Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
            return Box::new(ASTNode::err());
        });

        let parameters = NodeParameters {
            parameters: {
                if let NodeType::TupleExpression(ref value) = parameters_node.node.as_ref() {
                    value.parameters.clone()
                } else {
                    vec![parameters_node]
                }
            }
        };

        *inc_i = false;
        // check if body is code block or expression: `x => x + 1` or `x => { x + 1 }`
        let body = if tokens.get(*i + 1).is_some().then(|| tokens[*i + 1].token_type == TokenType::LBrace).unwrap_or(false) {
            // code block
            Self::inc(i);
            let block = self.get_code_block(tokens, i, true);
            
            if *i + 1 >= tokens.len() && self.__curent_parsing_line + 1 < self.lines.len() {
                self.__curent_parsing_line += 1;
                *i = 0;
                *tokens = self.lines[self.__curent_parsing_line].clone();
            }
            
            block
        } else {
            // expression
            Self::inc(i);
            let expression = self.get_expression(tokens, i, Self::NORMAL_PARSING);

            BodyRegion {
                body: vec![Box::new(ASTNode {
                    token,
                    node: Box::new(NodeType::ReturnExpression(expression)),
                })]
            }
        };

        LambdaExpression {
            parameters,
            body,
        }
    }

    fn get_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, until: u8) -> Vec<Box<ASTNode>> {
        // this allows this function to be reused for both functions and arrays
        let until_token = match until {
            Self::PARSING_FOR_FUNCTION => TokenType::RParen,
            Self::PARSING_FOR_ARRAY => TokenType::RBracket,
            Self::PARSING_FOR_STATEMENT => TokenType::LBrace,
            Self::PARSING_FOR_OBJECT_INSTANTIATION | Self::PARSING_FOR_CODE_BLOCK => TokenType::RBrace,
            _  => { 
                self.error(line!(), "INTERNAL ERROR", "Internal parsing error, caused when invalid argument was passed while trying to get node parameters in `Parser::get_node_parameters`. Expected a valid `until` argument. Try using `PARSING_FOR_FUNCTION`, `PARSING_FOR_ARRAY`, `PARSING_FOR_STATEMENT`, `PARSING_FOR_CODE_BLOCK`, or `PARSING_FOR_OBJECT_INSTANTIATION`", &tokens[*i].location);
                return vec![];
            }
        };

        if tokens.get(*i).is_some_and(|t| t.token_type == until_token) {
            // empty parameters
            Self::inc(i);
            return vec![];
        }

        let mut parameters = vec![];

        if until != Self::PARSING_FOR_OBJECT_INSTANTIATION && until != Self::PARSING_FOR_CODE_BLOCK && until != Self::PARSING_FOR_STATEMENT {
            Self::inc(i);
        }
        
        parameters.push(self.get_expression(tokens, i, until));

        if until != Self::PARSING_FOR_STATEMENT && tokens.get(*i).map_or(false, |x| x.token_type != TokenType::Comma) && (*i != 0 || (tokens.len() == 2 && tokens[1].token_type == until_token)) {
            Self::inc(i);
        }
        if *i == Parser::SET_I_TO_ZERO {
            *i = 0;
        }

        let mut done = false;
        while self.__curent_parsing_line < self.lines.len() {
            while *i < tokens.len() {
                //debug!(tokens[*i]);
                if tokens[*i].token_type == until_token {
                    // ends
                    Self::inc(i);
                    done = true;
                    break;
                }
                else if tokens[*i].token_type == TokenType::Comma {
                    // add to stack
                    Self::inc(i);
                    let expression = self.get_expression(tokens, i, until);
                    parameters.push(expression);
                }
                else if until == Self::PARSING_FOR_CODE_BLOCK && tokens[*i].token_type != until_token {
                    let expression = self.get_expression(tokens, i, until);
                    parameters.push(expression);

                    if tokens.get(*i + 1).map_or(false, |t| t.token_type == TokenType::RBrace) {
                        Self::inc(i);
                        done = true;
                        break;
                    }
                    
                    if *i != Parser::SET_I_TO_ZERO {
                        Self::inc(i);
                    } 
                }
                else {
                    // error
                    //self.error(line!(), "Error getting parameters for function", format!("Expected `,` or `)` but got `{}` while parsing function", tokens[*i].value).as_str(), &tokens[*i].location);
                    //debug!("Error getting parameters for function: Expected `,` or matching closing delimeter", tokens[*i]);
                    done = true;
                    break;
                }
                
                if *i == Parser::SET_I_TO_ZERO {
                    *i = 0;
                } else if until != Self::PARSING_FOR_STATEMENT && *i != 0 {
                    Self::inc(i);
                }
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
            let mut zero = 0;
            return_tokens.push(self.get_type_idententifier(&mut tokens, &mut zero, false));
        }

        return_tokens
    }

    fn get_type_idententifier(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, from_as_is_operators: bool) -> Box<ASTNode> {
        if let Some(first_token) = tokens.clone().get(*i) {
            let mut copy_tokens = tokens.clone();
            if from_as_is_operators { // if this is from an `as` or `is` operator, we need to find the type but not include any `{`, `}`, `)`, or `]`. For example: `if a is b {}`, we need to stop parsing the type at `{` because that starts the code block
                let tokens_skipped_i: Vec<_> = tokens.iter().skip(*i).collect(); // we don't want to stop parsing before the `i`, otherwise `} else if a is b {}` would stop parsing at the first `}` 
                copy_tokens = tokens[..tokens_skipped_i.iter().enumerate()
                    .position(|(index, t)| t.token_type == TokenType::LBrace 
                        || t.token_type == TokenType::RBrace 
                        || t.token_type == TokenType::RParen
                        // check if token is `]`, and if the token before it wasn't `[`. This is so this will get caught: `[a is b]` and this won't: `a is b[]`
                        || (t.token_type == TokenType::RBracket && index > 0 && !tokens_skipped_i.get(index - 1).map_or(false, |y| y.token_type == TokenType::LBracket))
                        )
                    .map(|pos| pos + *i) // adjust the positions to undo the `skip(*i)`
                    .unwrap_or(tokens.len()) // if none were found, than include the entire vector of tokens to parse
                ].to_vec();
            }
            if matches!(first_token.token_type, TokenType::Identifier | TokenType::Ampersand) {               
                let mut is_ptr_or_ref = vec![];
                while copy_tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Ampersand) {
                    is_ptr_or_ref.push(TypeMemoryModifier::Ref);
                    *i += 1;
                }

                if copy_tokens.iter().skip(*i).len() == 1 {
                    if copy_tokens.len() != tokens.len() {
                        Self::inc(i);
                    }
                    let token = copy_tokens.get(*i);
                    if token.is_none() {
                        self.error(line!(), "Expected type", "Parser error, expected type", &first_token.location);
                    }
                    let token = token.unwrap();
                    return Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(ScopedType {
                            scope: vec![TypeIdentifier {
                                name: token.clone(),
                                type_parameters: None,
                                scope_type: None,
                            }],
                            is_array: false,
                            is_ptr_or_ref,
                        }))
                    });
                }
                if copy_tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::DoubleColon || t.token_type == TokenType::Dot || t.token_type == TokenType::LessThan || t.token_type == TokenType::LBracket || t.token_type == TokenType::Star || t.token_type == TokenType::Ampersand) {
                    let scope_and_types = self.get_scoped_typed(&mut copy_tokens, i, is_ptr_or_ref);
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(scope_and_types)),
                    });
                }
                else if copy_tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::Comma) {
                    self.error(line!(), "Incorrect type", "Multiple variables in one declaration is not supported, try instead using a tuple by putting parenthesis around types", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                    return Box::new(ASTNode::err());
                } else {
                    let err_token = &tokens.get(*i + 1).map_or(tokens[0].clone(), |t| t.clone());
                    self.error(line!(), "Incorrect type", format!("Expected a correct type or tuple, did not expect `{}`", err_token.value).as_str(), &err_token.location);
                    return Box::new(ASTNode::err());
                }
            }
            else if first_token.token_type == TokenType::LParen {
                let tuple = self.get_tuple_node_parameters(&mut copy_tokens, i);
                return Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::TupleDeclaration(NodeParameters {
                        parameters: tuple
                    }))
                });
            }
            else if first_token.token_type == TokenType::LBrace {
                self.error(line!(), "Incorrect type", "Incorrect type declaration `type: name`, objects `{}` are not supported as a type", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                return Box::new(ASTNode::err());
            }
            else if first_token.token_type == TokenType::LBracket {
                self.error(line!(), "Incorrect type", "Incorrect type declaration. If you were trying to create an array, do: `type[]`", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
                return Box::new(ASTNode::err());
            }
            else {
                self.error(line!(), "Variable declaration has incorrect type", format!("Expected tuple or type, found: `{}`", first_token.value).as_str(), &first_token.location);
                return Box::new(ASTNode::err());
            }
        }
        else if tokens.iter().skip(*i).len() > 0 {
            self.error(line!(), "Variable declaration has no type", "Expected a type before the `:`, no type was provided", &tokens.get(*i + 1).map_or(tokens[0].location.clone(), |t| t.location.clone()));
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
                self.error(line!(), "Error getting node parameters", "Expected to start with `(`", &token.location);
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
            Self::inc(i);

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
                    self.error(line!(), "Error getting node parameters", "Could not find next line", &token.location);
                    break;
                }
            }
        }
        all_tokens
    }

    fn get_scoped_typed(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, mut is_ptr_or_ref: Vec<TypeMemoryModifier>) -> ScopedType {
        let scope = self.get_type_scope(tokens, i);
        let is_array = self.next_is_array_for_type(tokens, i);

        while *i < tokens.len() {
            if tokens[*i].token_type == TokenType::Star {
                is_ptr_or_ref.push(TypeMemoryModifier::Ptr);
            } else {
                break;
            }
            Self::inc(i);
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
                self.error(line!(), "Error getting type parameters", "Expected to start with `<`", &token.location);
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
            Self::inc(i);
            if angle_bracket_level == 0 && parenthesis_level == 0 && brace_level == 0 && bracket_level == 0 {
                break;
            }
        }

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_type_idententifier(&mut tokens, &mut 0, false));
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
                    Self::inc(i);
                    if last_punc.is_some() || first_token {
                        self.error(line!(), "Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return (scope, true);
                    }
                    *last_punc = Some(ScopeType::DoubleColon);
                    keep_last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    Self::inc(i);
                    if last_punc.is_some() || first_token {
                        self.error(line!(), "Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return (scope, true);
                    }
                    *last_punc = Some(ScopeType::Dot);
                    keep_last_punc = Some(ScopeType::Dot);
                }
                TokenType::Identifier => {
                    last_identifier_index = *i;
                    Self::inc(i);
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
                        self.error(line!(), "Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
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
            self.error(line!(), "Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
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
                    Self::inc(i);
                    if last_punc.is_some() {
                        self.error(line!(), "Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                    first_token = false;
                }
                TokenType::Dot => {
                    Self::inc(i);
                    if last_punc.is_some() {
                        self.error(line!(), "Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::Dot);
                    first_token = false;
                }
                TokenType::Identifier => {
                    let mut maybe_type_parameters = NodeParameters { parameters: vec![] };
                    if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::LessThan) {
                        Self::inc(i);

                        let mut insides = vec![];
                        let mut level = 0;
                        
                        while *i < tokens.len() {
                            insides.push(tokens[*i].clone());
                            if tokens[*i].token_type == TokenType::LessThan {
                                level += 1;
                            } else if tokens[*i].token_type == TokenType::GreaterThan {
                                level -= 1;
                                if level == 0 {
                                    break;
                                }
                            }
                            Self::inc(i);
                        }

                        let mut j = 0;
                        maybe_type_parameters = self.get_type_parameters(&mut insides, &mut j);
                    } 
                    Self::inc(i);

                    if last_punc.is_none() && !first_token {
                        self.error(line!(), "Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
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
            self.error(line!(), "Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
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
                self.error(line!(), "Error setting type parameters", "Expected type to be array, but only `[` was given with no closing `]`. Expected: `type[]`", &tokens[*i].location);
                return false;
            }
        }
        return false;
    }

    fn get_anonymous_type_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> AnonymousTypeParameters {
        Self::inc(i); // skip the `<`
        
        let mut parameters = vec![];
        let mut last_was_comma = true;
        while *i < tokens.len() && tokens[*i].token_type != TokenType::GreaterThan {
            let token = tokens[*i].clone();
            match token.token_type {
                TokenType::Comma => {
                    if last_was_comma {
                        self.error(line!(), "Error getting type parameters", "Consecutive commas found. Expected: `<type, type, type>`", &token.location);
                        return AnonymousTypeParameters { parameters };
                    }
                    last_was_comma = true;
                }
                TokenType::Identifier => {
                    if !last_was_comma {
                        self.error(line!(), "Error getting type parameters", "Expected comma between type parameters. Expected: `<type, type, type>`", &token.location);
                        return AnonymousTypeParameters { parameters };
                    }
                    last_was_comma = false;

                    if tokens[*i + 1].token_type == TokenType::Is {
                        Self::inc(i);
                        Self::inc(i);
                        let constraints = self.get_expression(tokens, i, Self::PARSING_FOR_TYPE_CONSTRAINT);
                        parameters.push(AnonymousType {
                            name: token.clone(),
                            constraints: Some(constraints)
                        });
                    } else {
                        parameters.push(AnonymousType {
                            name: token.clone(),
                            constraints: None
                        });
                    }
                }
                _ => {
                    self.error(line!(), "Error getting type parameters", "Expected identifier after comma. Expected: `<type, type, type>`", &token.location);
                    return AnonymousTypeParameters { parameters };
                }
            }
            Self::inc(i);
        }

        Self::inc(i); // skip the `>`
        AnonymousTypeParameters { parameters }
    }

    fn get_defined_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<DefinedNodeParameter> {
        let error_token = tokens[*i].clone();
        Self::inc(i); // skip the `(`
        
        let mut parameters = vec![];
        
        while *i < tokens.len() && tokens[*i].token_type != TokenType::RParen {
            // first get the type
            // go through tokens until `:`
            let mut type_tokens = vec![];
            while *i < tokens.len() && tokens[*i].token_type != TokenType::Colon {
                type_tokens.push(tokens[*i].clone());
                Self::inc(i);
            }
            let ty = self.get_type_idententifier(&mut type_tokens, &mut 0, false);

            // next is the name
            Self::inc(i);
            let name = tokens[*i].clone();
            Self::inc(i);

            // check if next token is `=`, if so skip it and get the value
            let default_value;
            if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Assign) {
                Self::inc(i);
                // next is the value
                default_value = Some(self.get_expression(tokens, i, Self::PARSING_FOR_FUNCTION));
                Self::inc(i);
            } else {
                default_value = None;
            }

            parameters.push(DefinedNodeParameter { name, ty, default_value });

            // check if next token is `,` or `)` 
            if let Some(nt) = tokens.get(*i) {
                match nt.token_type {
                    TokenType::Comma => {
                        Self::inc(i);
                        continue;
                    }
                    TokenType::RParen => {
                        Self::inc(i); 
                        break;
                    }
                    _ => {
                        self.error(line!(), "Error parsing defined node parameters", "Expected next token to be `,` or `)`", &nt.location);
                        break;
                    }
                }
            } else {
                self.error(line!(), "Error parsing defined node parameters", "Expect next token to be `,` or `)`", if *i - 1 < tokens.len() { tokens.get(*i - 1).map_or(&error_token.location, |t| &t.location) } else { &error_token.location});
                break;
            }
        } 

        parameters
    }

    #[allow(dead_code)]
    fn print_node(node: &ASTNode) {
        println!("{}", Self::node_expr_to_string(node, 0));
    }

    fn node_expr_to_string(node: &ASTNode, mut tab_level: usize) -> String {
        match *node.node {
            NodeType::VariableDeclaration(ref value) => {
                let var_type = Self::node_expr_to_string(&value.var_type, tab_level);
                let var_name = value.var_name.value.clone();
                let var_value = Self::node_expr_to_string(&value.var_value, tab_level);
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
                format!("{}{}: {} = {}", access_modifiers, var_type, var_name, var_value)
            }
            NodeType::TypeIdentifier(ref value) => {
                let array = value.is_array.then(|| "[]").unwrap_or("");
                let mut pointer = "".to_string();
                let mut reference = "".to_string();
                for modifier in value.is_ptr_or_ref.iter() {
                    match modifier {
                        TypeMemoryModifier::Ptr => {
                            pointer += "*";
                        },
                        TypeMemoryModifier::Ref => {
                            reference += "&";
                        },
                    };
                }
                let mut scope = "".to_string();
                for ident in value.scope.iter() {
                    let scope_type = ident.scope_type.clone().is_some().then(|| ident.scope_type.clone().unwrap().to_string()).unwrap_or("".to_string());
                    let identifier = ident.name.value.clone();
                    if let Some(type_parameters) = &ident.type_parameters {
                        let mut parameters = "".to_string();
                        for (index, param) in type_parameters.parameters.iter().enumerate() {
                            parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), type_parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                        }
                        scope += format!("{}{}<{}>", scope_type, identifier, parameters).as_str();
                    }
                    else {
                        scope += format!("{}{}", scope_type, identifier).as_str();
                    }
                }
                format!("{}{}{}{}", reference, scope, array, pointer)
            }
            NodeType::TupleDeclaration(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("({})", tuple)
            }
            NodeType::Operator(ref value) => {
                let left = Self::node_expr_to_string(&value.left, tab_level);
                let right = Self::node_expr_to_string(&value.right, tab_level);

                format!("({} {} {})", left, value.operator.value, right)
            }
            NodeType::UnaryOperator(ref value) => {
                let operand = Self::node_expr_to_string(&value.operand, tab_level);

                format!("({}{})", value.operator.value, operand)
            }
            NodeType::FunctionCall(ref value) => {
                let mut parameters = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut type_parameters = "".to_string();
                if value.type_parameters.is_some() {
                    type_parameters += "<";
                    for (index, param) in value.type_parameters.clone().unwrap().iter().enumerate() {
                        type_parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.type_parameters.clone().unwrap().get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
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
                            type_parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), ident.type_parameters.clone().unwrap().parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                        }
                        type_parameters += ">";
                    }
                    scope += format!("{}{}{}", scope_type, Self::node_expr_to_string(ident.expression.as_ref(), tab_level), type_parameters).as_str();
                }
                scope
            }
            NodeType::TupleExpression(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
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
                let condition = Self::node_expr_to_string(&value.condition, tab_level);
                let then = Self::node_expr_to_string(&value.then, tab_level);
                let else_then = Self::node_expr_to_string(&value.else_then, tab_level);

                format!("({} ? {} : {})", condition, then, else_then)
            }
            NodeType::ArrayExpression(ref value) => {
                let mut array = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    array += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("[{}]", array)
            }
            NodeType::Indexer(ref value) => {
                let object = Self::node_expr_to_string(&value.object, tab_level);
                let mut index = "".to_string();
                for (i, param) in value.index.iter().enumerate() {
                    index += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.index.get(i + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("{}[{}]", object, index)
            }
            NodeType::ObjectInstantiation(ref value) => {
                let mut object = "".to_string();
                let object_type = Self::node_expr_to_string(&value.object_type, tab_level);
                for (index, param) in value.properties.iter().enumerate() {
                    object += format!("{} = {}{}", param.name.value, Self::node_expr_to_string(&param.value, tab_level).as_str(), value.properties.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
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
                tab_level += 1;
                for param in value.body.iter() {
                    body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                }
                tab_level -= 1;
                format!("{{\n{}{}}}", body, "    ".repeat(tab_level))
            }
            NodeType::ReturnExpression(ref value) => {
                format!("return {}", Self::node_expr_to_string(&value, tab_level))
            }
            NodeType::LambdaExpression(ref value) => {
                let mut parameters = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut body = "".to_string();
                for param in value.body.body.iter() {
                    body += format!("{}; ", Self::node_expr_to_string(&param, tab_level).as_str()).as_str();
                }
                format!("({}) => {{ {}}}", parameters, body)
            }
            NodeType::Assignment(ref value) => {
                format!("{} = {}", Self::node_expr_to_string(&value.left, tab_level), Self::node_expr_to_string(&value.right, tab_level))
            }
            NodeType::If(ref value) | NodeType::While(ref value) => {
                let condition = Self::node_expr_to_string(&value.condition, tab_level);
                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));
                
                let mut else_if_string = "".to_string();
                if let Some(else_if) = value.else_if_regions.clone() {
                    for region in else_if.iter() {
                        let condition = Self::node_expr_to_string(&region.condition, tab_level);
                        let mut body = "".to_string();
                        tab_level += 1;
                        for param in region.body.body.iter() {
                            body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                        }
                        tab_level -= 1;
                        body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));
                        let regional_if_name = region.is_while.then(|| "while").unwrap_or("if");
                        else_if_string += format!("\n{}else {} {} {}", "    ".repeat(tab_level), regional_if_name, condition, body).as_str();
                    }
                }

                let mut else_string = "".to_string();
                if let Some(else_region) = value.else_region.clone() {
                    let mut body = "".to_string();
                    tab_level += 1;
                    for param in else_region.body.iter() {
                        body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                    }
                    tab_level -= 1;
                    body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));
                    else_string += format!("\n{}else {}", "    ".repeat(tab_level), body).as_str();
                }
                let if_name = value.is_while.then(|| "while").unwrap_or("if");

                format!("{} {} {} {}{}", if_name, condition, body, else_if_string, else_string)
            }
            NodeType::ForEach(ref value) => {
                let index = value.index_segment.clone().map_or("".to_string(), |t| t.value.clone());
                let val = Self::node_expr_to_string(&value.iter_value, tab_level);
                let range = Self::node_expr_to_string(&value.iter_range, tab_level);

                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));

                format!("for {}{} in {} {}", value.index_segment.clone().map_or("".to_string(), |_| index + ", "), val, range, body)
            }
            NodeType::For(ref value) => {
                let index = value.index_segment.clone().map_or("".to_string(), |t| t.value.clone());
                let set = Self::node_expr_to_string(&value.set_segment, tab_level);
                let cond = Self::node_expr_to_string(&value.condition_segment, tab_level);
                let inc = Self::node_expr_to_string(&value.increment_segment, tab_level);

                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));

                format!("for {}{}, {}, {} {}", value.index_segment.clone().map_or("".to_string(), |_| index + ", "), set, cond, inc, body)
            }
            NodeType::Match(ref value) => {
                let expression = Self::node_expr_to_string(&value.match_value, tab_level);
                tab_level += 1;
                let mut cases = "".to_string();

                for case in value.match_cases.clone() {
                    let mut c = "\n".to_string();

                    c += "    ".repeat(tab_level).as_str();
                    c += Self::node_expr_to_string(&case.pattern, tab_level).as_str();
                    c += " => ";
                    c += Self::node_expr_to_string(&case.body, tab_level).as_str();

                    cases = format!("{}{}", cases, c);
                }

                tab_level -= 1;
                format!("match {} {{{}\n{}}}", expression, cases, "    ".repeat(tab_level))
            }
            NodeType::Discard(_) => {
                "_".to_string()
            }
            NodeType::Use(ref value) => {
                format!("use \"{}\"", value.value)
            }
            NodeType::Shabang(ref value) => {
                match value {
                    ShabangType::Allow(t) => format!("#! allow {}", t.value),
                    ShabangType::Warning(t) => format!("#! warning {}", t.value),
                    ShabangType::Err(t) => format!("#! err {}", t.value),
                    ShabangType::Depricated => "#! depricated".to_string(),
                    ShabangType::Crumb => "#! crumb".to_string(),
                    ShabangType::If(node) => format!("#! if {}", Self::node_expr_to_string(node, tab_level)),
                    ShabangType::IfNot(node) => format!("#! ifn {}", Self::node_expr_to_string(node, tab_level)),
                    ShabangType::ElseIf(node) => format!("#! elif {}", Self::node_expr_to_string(node, tab_level)),
                    ShabangType::Else => "#! else".to_string(),
                    ShabangType::EndIf => "#! endif".to_string(),
                    ShabangType::IfNotDefined(token) => format!("#! ifndef {}", token.value),
                    ShabangType::IfDefined(token) => format!("#! ifdef {}", token.value),
                    ShabangType::Define(node, value) => format!("#! def {} -> {}", Self::node_expr_to_string(node, tab_level), Self::node_expr_to_string(value, tab_level)),
                    ShabangType::Undef(token) => format!("#! undef {}", token.value),
                    ShabangType::Once(token) => format!("#! once {}", token.value),
                    ShabangType::DefineFile(token) => format!("#! define file {}", token.value),
                    ShabangType::Other(tokens) => format!("#! {}", tokens.iter().map(|t| t.value.clone()).collect::<Vec<String>>().join(" ")),
                }
            }
            NodeType::FunctionDeclaration(ref value) => {
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
                let ty = Self::node_expr_to_string(&value.return_type, tab_level);
                let name = value.name.value.clone();
                let mut generics = "".to_string();
                if value.type_parameters.is_some() {
                    generics += "<";

                    for anonymous in value.type_parameters.clone().unwrap().parameters.iter() {
                        generics += &anonymous.name.value;
                        if anonymous.constraints.is_some() {
                            generics += " is ";
                            generics += &Self::node_expr_to_string(&anonymous.constraints.clone().unwrap(), tab_level);
                        }
                        
                        if anonymous != value.type_parameters.clone().unwrap().parameters.last().unwrap() {
                            generics += ", ";
                        }
                    }

                    generics += ">";
                } 
                
                let mut parameters = "(".to_string();
                for param in value.parameters.iter() {
                    parameters += &Self::node_expr_to_string(&param.ty, tab_level);
                    parameters += ": ";
                    parameters += &param.name.value;

                    if param.default_value.is_some() {
                        parameters += " = ";
                        parameters += &Self::node_expr_to_string(&param.default_value.clone().unwrap(), tab_level);
                    }

                    if param != value.parameters.last().unwrap() {
                        parameters += ", ";
                    }
                }
                parameters += ")";

                let mut body = "".to_string();
                if value.body.is_some() {
                    tab_level += 1;
                    for param in value.body.clone().unwrap().body.iter() {
                        body += format!("{}{}{}", "    ".repeat(tab_level), Self::node_expr_to_string(&param, tab_level).as_str(), ";\n").as_str();
                    }
                    tab_level -= 1;
                    body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level))
                } 
                format!("{}{}: {}{}{} {}", access_modifiers, ty, name, generics, parameters, body)
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
            //.filter(|x| !x.is_empty()) // commented out because it breaks object instantiation
            .collect()
    }
}