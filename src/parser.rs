// eventually need to remove [] and instead use get
// remove all unwrapping and replace with error handling
// go over and make sure no unneciary cloning is happening

use std::vec;

use crate::lexer::{Token, TokenType, Location};
use crate::ast::*;
use crate::error_handling::ErrorHandling;

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
    __curent_line: usize,
}

impl Parser {
    pub fn new(file: Option<String>, full_code: String, tokens: Vec<Token>) -> Parser {
        Parser { 
            output: ErrorHandling::new(file, full_code), 
            ast: Vec::new(), 
            lines: Self::split_tokens_into_lines(&tokens),
            __curent_line: 0,
        }
    }

    pub fn generate_ast(&mut self) {        
        self.ast = self.generate_from_tokens();

        // for testing
        for node in self.ast.iter() {
            Self::print_node(node);
        }
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
    
    fn generate_from_tokens(&mut self) -> Vec<ASTNode> {
        let mut ast: Vec<ASTNode> = Vec::new();
        
        while self.__curent_line < self.lines.len() {
            let mut tokens = self.lines[self.__curent_line].clone();
            if let Ok(node) = self.get_ast_node(&mut tokens) {
                ast.push(node);
            }
            self.__curent_line += 1;
        }
        
        ast
    }
    
    fn get_ast_node(&mut self, tokens: &mut Vec<Box<Token>>) -> Result<ASTNode, ()> {
        if tokens.len() == 0 {
            return Err(());
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
                let var_type: Box<ASTNode> = self.get_type_idententifier(&mut type_tokens);

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
            _ => {
                return Ok(*self.get_entire_expression(tokens));
            }
        }
    }

    fn function_call(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize) -> ScopedIdentifier {
        let mut scope = scope;
        *i -= 1;

        let mut node_parameters: Vec<Box<ASTNode>> = vec![];
        let mut type_parameters: Vec<Box<ASTNode>> = vec![];
        let name = tokens[*i].clone();

        if let Some(next_token) = tokens.clone().get(*i + 1) {
            if next_token.token_type == TokenType::LessThan {
                *i += 1;
                type_parameters = self.get_type_parameters(tokens, i).parameters;
            }

            if tokens.get(*i + 1).is_some() && tokens[*i + 1].token_type == TokenType::LParen {
                *i += 1;
                node_parameters = self.get_expression_node_parameters(tokens, i);
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
            name: name.clone(),
            expression: Some(Box::new(ASTNode {
                token: name.clone(),
                node: Box::new(NodeType::FunctionCall(function_call.clone())),
            })),
            scope_type: None,
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
            
            let name = tokens[*i].clone();
            let chained_expression = self.get_expression(tokens, i);

            scope.scope.push(Identifier {
                name,
                expression: Some(chained_expression.clone()),
                scope_type: Some(ScopeType::Dot),
            });

            *i -= 1;
            scope
        }
        else {
            *i -= 1;
            scope
        }
    }

    fn scope_call(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedIdentifier {
        *i -= 1;

        let scope = self.get_ident_scope(tokens, i);

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            self.function_call(tokens, Some(scope), i)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBracket) { 
            self.get_array_expression(tokens, Some(scope), Some(tokens[*i - 1].clone()), i)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            self.get_object_instantiation(tokens, Some(scope), i)
        }
        else {
            *i -= 1;
            scope
        }
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
    
        let mut ternary_state = TernaryState::Condition;
        let mut ternary_tokens = TernaryType {
            condition: vec![],
            then: vec![],
            else_then: vec![],
        };
    
        let mut current_tokens: Vec<Box<Token>> = vec![];
        let mut left_parenthesis_started = false;
        let mut parenthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;

        /*
            Another problem, when it gets to the `?` and there is a parenthesis before it,
            even if the entire ternary operation is wrapped in parenthesis, it has no way of knowing that the end parenthesis
            is at the end of the expression. I need to allow both of these to be valid: `(a ? b : c) ? d : e` and `(a ? b : c)`.
            It won't be able to tell if the first `?` is apart of nested expression, or if it is wraps the entire ternary operation.
            To solve this, we will need to check if it's wrapped when it hits the first `(` by looping and checking the nested level.
            The variable `parenthesis_wrapped` will be set it the there is no matching `(` in the condition. When it gets to the else section, 
            and `parenthesis_wrapped`, if the nesting level goes below 0, then we will not throw an error because it's the matching end `)`.
        */

        let mut parenthesis_wrapped = 0;
    
        while *i < tokens.len() {
            match tokens[*i].token_type {
                TokenType::LBrace => {
                    brace_level += 1;
                    current_tokens.push(tokens[*i].clone());
                }
                TokenType::RBrace => {
                    brace_level -= 1;
                    current_tokens.push(tokens[*i].clone());
                }
                TokenType::LBracket => {
                    bracket_level += 1;
                    current_tokens.push(tokens[*i].clone());
                }
                TokenType::RBracket => {
                    bracket_level -= 1;
                    current_tokens.push(tokens[*i].clone());
                }
                TokenType::QuestionMark => {
                    if parenthesis_level > 0 || brace_level > 0 || bracket_level > 0 {
                        current_tokens.push(tokens[*i].clone());
                    } else {
                        if ternary_state != TernaryState::Condition {
                            self.error("Unexpected `?`", "Unexpected `?` found in ternary expression: `a ? b : c`", &tokens[*i].location);
                            return NodeType::None;
                        }
                        left_parenthesis_started = false;
                        ternary_tokens.condition = std::mem::take(&mut current_tokens);
                        ternary_state = TernaryState::Then;
                    }
                }
                TokenType::Colon => {
                    if parenthesis_level > 0 || brace_level > 0 || bracket_level > 0 {
                        current_tokens.push(tokens[*i].clone());
                    } else if ternary_state == TernaryState::Then {
                        ternary_tokens.then = std::mem::take(&mut current_tokens);
                        ternary_state = TernaryState::Else;
                        left_parenthesis_started = false;
                    } else {
                        self.error("Unexpected `:`", "Unexpected `:` found in ternary expression: `a ? b : c`", &tokens[*i].location);
                        return NodeType::None;
                    }
                }
                TokenType::LParen => {
                    left_parenthesis_started = true;
                    parenthesis_level += 1;
                    current_tokens.push(tokens[*i].clone());
                    if ternary_state == TernaryState::Condition {
                        // we will loop through tokens until we find the matching end parenthesis or a non nested `?` 
                        // if we find a non nested `?`, then we know the entire ternary operation was wrapped in parenthesis
                        // if not, then we know the parenthesis is part of the condition
                        let mut j = i.clone() + 1;
                        let mut tracking_parenthesis_level = 1;
                        let mut tracking_brace_level = 0;
                        let mut tracking_bracket_level = 0;
                        while j < tokens.len() {
                            match tokens[j].token_type {
                                TokenType::LBrace => tracking_brace_level += 1,
                                TokenType::RBrace => tracking_brace_level -= 1,
                                TokenType::LBracket => tracking_bracket_level += 1,
                                TokenType::RBracket => tracking_bracket_level -= 1,
                                TokenType::LParen => tracking_parenthesis_level += 1,
                                TokenType::RParen => {
                                    tracking_parenthesis_level -= 1;
                                    if tracking_parenthesis_level == 0 && tracking_brace_level == 0 && tracking_bracket_level == 0 {
                                        // has found matching end parenthesis
                                        break;
                                    }
                                }
                                TokenType::QuestionMark => {
                                    if tracking_parenthesis_level == 1 {
                                        // found non nested `?`, so the entire ternary operation was wrapped in parenthesis
                                        parenthesis_level -= 1;
                                        parenthesis_wrapped += 1;
                                        current_tokens.pop();
                                        break;
                                    }
                                }
                                _ => {}
                            }
                            j += 1;
                        }

                    }
                }
                TokenType::RParen => {
                    parenthesis_level -= 1;
                    if parenthesis_level < 0 {
                        if parenthesis_wrapped != 0 {
                            parenthesis_wrapped -= 1;
                            parenthesis_level += 1;
                        }
                        else {
                            self.error("Unmatched `)`", "Found unmatched `)` in ternary expression: `a ? b : c`", &tokens[*i].location);
                            return NodeType::None;
                        }
                    }
                    else {
                        current_tokens.push(tokens[*i].clone());
                    }
                }
                _ => {
                    current_tokens.push(tokens[*i].clone());
                }
            }
    
            if ternary_state == TernaryState::Else && parenthesis_level == 0 && left_parenthesis_started {
                break;
            }
    
            *i += 1;
        }
    
        if parenthesis_level != 0 {
            self.error("Unmatched parentheses", "Mismatched parentheses in ternary expression.", &tokens[i.saturating_sub(1)].location);
            return NodeType::None;
        }
    
        match ternary_state {
            TernaryState::Else => ternary_tokens.else_then = current_tokens,
            _ => {
                self.error("Incomplete ternary expression", "Expected `:` to complete the ternary expression: `a ? b : c`", &tokens[i.saturating_sub(1)].location);
                return NodeType::None;
            }
        }

        *i -= 1;
    
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

    fn get_array_expression(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, original_token: Option<Box<Token>>, i: &mut usize) -> ScopedIdentifier {
        let mut scope = scope;
        let first_token = tokens[*i].clone();
        // I need to loop through until I find the matching end bracket. I need to keep track of parenthesis and braces.
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
                    if bracket_level == 0 && brace_level == 0 && parenthesis_level == 0 && angle_bracket_level_count == 0 {
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
                    if bracket_level == 1 && parenthesis_level == 0 && brace_level == 0 && angle_bracket_level_count == 0 {
                        last_was_comma = 2;
                        all_tokens.push(vec![]);
                        comma_vec_index += 1;
                    }
                    else {
                        all_tokens[comma_vec_index].push(tokens[*i].clone());
                    }
                }
                TokenType::LessThan => {
                    if angle_bracket_level_count == 0 {
                        let mut j = *i;
                        let mut temp_angle_bracket_level = 0;
                        let mut highest = 0;
                        let mut is_angle_bracket = true; // if is operator: `X < Y` or `Type<T>`
                        while j < tokens.len() {
                            match tokens[j].token_type {
                                TokenType::GreaterThan => {
                                    temp_angle_bracket_level -= 1;
                                    if temp_angle_bracket_level == 0 {
                                        if tokens.get(j + 1).is_some().then(|| tokens[j + 1].token_type != TokenType::LParen).unwrap_or(false) {
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

        let mut array_nodes: Vec<Box<ASTNode>> = vec![];
        for token_vec in all_tokens {
            let mut token_vec= token_vec.clone();
            let node = self.get_entire_expression(&mut token_vec);
            array_nodes.push(node);
        }
    
        // Update the last identifier's expression if possible
        if let Some(s) = scope.as_mut() {
            if let Some(last) = s.clone().scope.last_mut() {
                let last_token = original_token.unwrap_or(last.name.clone());    
                last.expression = Some(Box::new(ASTNode {
                    token: last_token.clone(),
                    node: Box::new(NodeType::Indexer(IndexingExpression {
                        object: last.expression.clone().unwrap_or(Box::new(ASTNode {
                            token: last.name.clone(),
                            node: Box::new(NodeType::Identifier(last_token.clone())),
                        })),
                        index: array_nodes.clone(),
                    })),
                }));
            }
        }

        let mut scope = scope.take().unwrap_or(ScopedIdentifier {
            scope: vec![Identifier {
                name: first_token.clone(),
                expression: Some(Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::ArrayExpression(NodeParameters { parameters: array_nodes.clone() })),
                })),
                scope_type: None,
            }],
        });
    
        // Handle chaining if the next token is a dot
        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
            *i += 1;
            let chained_expression = self.get_expression(tokens, i);
            scope.scope.push(Identifier {
                name: tokens[*i].clone(),
                expression: Some(chained_expression),
                scope_type: Some(ScopeType::Dot),
            });
            *i -= 1; 
        } else {
            *i -= 1;
        }
    
        scope        
    }

    fn get_object_instantiation(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize) -> ScopedIdentifier {
        if scope.is_none() {
            self.error("No scope for object instantiation", "Expected a type for object instantiation: `type { properties... }`", &tokens[*i].location);
        }
        let mut scope = scope.unwrap();
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        let mut comma_vec_index = 0;
        let mut parenthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;

        let mut last_was_comma = 0;
        let mut angle_bracket_level_count = 0; 

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
                    if bracket_level == 0 && brace_level == 0 && parenthesis_level == 0 && angle_bracket_level_count == 0 {
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
                    if angle_bracket_level_count == 0 {
                        let mut j = *i;
                        let mut temp_angle_bracket_level = 0;
                        let mut highest = 0;
                        let mut is_angle_bracket = true; 
                        while j < tokens.len() {
                            match tokens[j].token_type {
                                TokenType::GreaterThan => {
                                    temp_angle_bracket_level -= 1;
                                    if temp_angle_bracket_level == 0 {
                                        if tokens.get(j + 1).is_some().then(|| tokens[j + 1].token_type != TokenType::LParen).unwrap_or(false) {
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
            object_type: scope.scope.last().unwrap().expression.clone().unwrap_or(Box::new(ASTNode {
                token: scope.scope.last().unwrap().name.clone(),
                node: Box::new(NodeType::Identifier(scope.scope.last().unwrap().name.clone())),
            })),
        };

        scope.scope.last_mut().unwrap().expression = Some(Box::new(ASTNode {
            token: tokens[*i].clone(),
            node: Box::new(NodeType::ObjectInstantiation(object_instantiation)),
        }));

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Dot) {
            *i += 1;
            let chained_expression = self.get_expression(tokens, i);
            scope.scope.push(Identifier {
                name: tokens[*i].clone(),
                expression: Some(chained_expression),
                scope_type: Some(ScopeType::Dot),
            });

            *i -= 1; 
            scope
        } else {
            *i -= 1;
            scope
        }
    }

    fn get_code_block(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, last_is_return: bool) -> BodyRegion {
        let mut brace_level = 0;
        let mut body = vec![];
        let mut semicolon_count = 0;
        let mut current_tokens = vec![vec![]];
        let mut last_was_identifer = false;

        while self.__curent_line < self.lines.len() {
            while *i < tokens.len() {
                match tokens[*i].token_type {
                    TokenType::LBrace => {
                        brace_level += 1;
                        if brace_level != 1 {
                            if last_was_identifer {
                                current_tokens[semicolon_count].push(tokens[*i].clone());
                            }
                            else {
                                let nested_region = self.get_code_block(tokens, i, last_is_return);
                                body.push(Box::new(ASTNode{
                                    token: tokens[*i].clone(),
                                    node: Box::new(NodeType::CodeBlock(nested_region)),
                                }));
                                brace_level -= 1;
                            }
                        }
                        last_was_identifer = false;
                    }
                    TokenType::RBrace => {
                        brace_level -= 1;
                        if brace_level == 0 {
                            if let Ok(node) = self.get_ast_node(&mut current_tokens[semicolon_count]) {
                                body.push(Box::new(node));
                            }
                            if last_is_return {
                                if let Some(last) = body.last_mut() {
                                    if let NodeType::ReturnExpression(_) = last.node.as_ref() {
                                        // nothing
                                    } else {
                                        last.node = Box::new(NodeType::ReturnExpression(Box::new(ASTNode {
                                            token: tokens[*i].clone(),
                                            node: last.as_ref().node.clone(),
                                        })));
                                    }
                                }
                            }                            
                            return BodyRegion { body };
                        }
                        else {
                            current_tokens[semicolon_count].push(tokens[*i].clone());
                        }
                        last_was_identifer = false;
                    }
                    TokenType::Identifier => {
                        current_tokens[semicolon_count].push(tokens[*i].clone());
                        last_was_identifer = true;
                    }
                    _ => {
                        current_tokens[semicolon_count].push(tokens[*i].clone());
                        last_was_identifer = false;
                    }
                }
                *i += 1;
            }

            last_was_identifer = false;
            current_tokens.push(vec![]);
            if let Ok(node) = self.get_ast_node(&mut current_tokens[semicolon_count]) {
                body.push(Box::new(node));
            }
            semicolon_count += 1;

            self.__curent_line += 1;
            *i = 0;
            if self.__curent_line >= self.lines.len() {
                break;
            }
            *tokens = self.lines[self.__curent_line].clone();
        }
        
        self.error("No end of Body Region", "Code block does not have ending brace `{ ... }` ", &tokens[*i].location);
        return BodyRegion { body };
    }
    
    fn get_entire_expression(&mut self, tokens: &mut Vec<Box<Token>>) -> Box<ASTNode> {
        let mut __ = usize::MAX;
        self.get_expression(tokens, &mut __)
    }

    fn get_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Box<ASTNode> {
        let mut expr_stack: Vec<(Box<ASTNode>, usize)> = Vec::new();
        let mut op_stack: Vec<(Box<Token>, usize)> = Vec::new();
        let mut tuple_vec: Vec<Box<ASTNode>> = Vec::new();
        let mut last_was_ident = false;
        let mut is_inside_parenthesis = false;
        let mut last_was_unary_operator = false;
        let mut paran_index = 0;
        let mut is_1_expression = true;
        
        if *i == usize::MAX {
            is_1_expression = false;
            *i = 0;
        }
        let mut starting_index_for_ternary = *i;
    
        while *i < tokens.len() {
            let token = tokens[*i].clone();
    
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
                expr_stack.push((Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(ternary),
                }), *i));
            }
            else if token.token_type.is_operator() {
                // To check for unary operators, the operator must be the first token in the expression, or the previous token must be an operator: `-0` or `0+-1`
                let mut handle_as_operator = true;
                if last_was_ident && token.token_type == TokenType::LessThan {
                    // part of function call: function<TYPE>()
                    expr_stack.push((Box::new(ASTNode { 
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(self.function_call(tokens, None, i))),
                    }), *i));
                    handle_as_operator = false;
                    last_was_ident = false;
                }
                else if last_was_unary_operator {
                    handle_as_operator = false;
                }
                else if token.token_type.is_unary_operator() && tokens.get(*i + 1).is_some().then(|| !tokens[*i + 1].token_type.is_operator()).unwrap_or(false) {
                    handle_as_operator = false;
                    if *i == 0 || is_1_expression {
                        last_was_unary_operator = false;
                        
                        *i += 1;
                        let next_expression = self.get_expression(tokens, i);
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
                    else if tokens.get(*i - 1).is_some().then(|| tokens[*i - 1].token_type.is_operator() || tokens[*i - 1].token_type == TokenType::LParen || tokens[*i - 1].token_type == TokenType::LBracket || tokens[*i - 1].token_type == TokenType::LBrace).unwrap_or(false) {
                        last_was_unary_operator = false;

                        *i += 1;
                        let next_expression = self.get_expression(tokens, i);
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
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            
                            let node = Box::new(ASTNode {
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
                    self.error("Expected identifier before dot", "Expected identifier before dot: `IDENT.IDENT`", &token.location);
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::LParen {
                starting_index_for_ternary = *i;
                last_was_unary_operator = false;
                if last_was_ident {
                    // part of function call
                    expr_stack.push((Box::new(ASTNode { 
                        token: tokens[*i - 1].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.function_call(tokens, None, i))),
                    }), *i));
                }
                else {
                    // part of expression
                    is_inside_parenthesis = true;
                    paran_index += 1;
                    op_stack.push((token, *i));
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::RParen {
                last_was_unary_operator = false;
                is_inside_parenthesis = false;
                paran_index -= 1;
                while let Some(top_op) = op_stack.pop() {
                    if top_op.0.token_type == TokenType::LParen {
                        break;
                    } else {
                        let right = expr_stack.pop().unwrap_or_else(|| {
                            self.error("Couldn't parse expression", "Right hand side of expression is empty", &token.location);
                            return (Box::new(ASTNode::none()), 0);
                        }).0;
                        let left = expr_stack.pop().unwrap_or_else(|| {
                            self.error("Couldn't parse expression", "Left hand side of expression is empty", &token.location);
                            return (Box::new(ASTNode::none()), 0);
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
                }
            }
            else if token.token_type == TokenType::LBracket {
                if last_was_ident {
                    // indexing
                    let temp_scope = ScopedIdentifier {
                        scope: vec![Identifier {
                            name: tokens[*i - 1].clone(),
                            expression: expr_stack.pop().map(|x| x.0),
                            scope_type: None,
                        }]
                    };
                    let indexing_expresion = self.get_array_expression(tokens, Some(temp_scope), Some(tokens[*i - 1].clone()), i);
                    expr_stack.push((Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedExpression(indexing_expresion)),
                    }), *i));
                }
                else {
                    // array expression
                    let array_expression = self.get_array_expression(tokens, None, None, i);
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
                    let temp_scope = ScopedIdentifier {
                        scope: vec![Identifier {
                            name: tokens[*i - 1].clone(),
                            expression: expr_stack.pop().map(|x| x.0),
                            scope_type: None,
                        }]
                    };
                    let obj_instantiation = self.get_object_instantiation(tokens, Some(temp_scope), i);

                    expr_stack.push((Box::new(ASTNode {
                        token: tokens[*i - 1].clone(),
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
                    return Box::new(ASTNode::none());
                }

                expr_stack.push((node, *i));
            }
            else if token.token_type == TokenType::Identifier {
                last_was_unary_operator = false;
                if tokens.get(*i + 1).is_some() && (tokens[*i + 1].token_type == TokenType::DoubleColon || tokens[*i + 1].token_type == TokenType::Dot || tokens[*i + 1].token_type == TokenType::LParen || tokens[*i + 1].token_type == TokenType::LessThan) {
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
                                    last_was_ident = tokens.get(j + 1).is_some().then(|| tokens[j + 1].token_type == TokenType::LParen).unwrap_or(false);
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
                        if check_token.token_type == TokenType::LBracket || check_token.token_type == TokenType::LBrace {
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
            else if token.token_type == TokenType::Comma {
                last_was_unary_operator = false;
                if is_inside_parenthesis {
                    
                    // handle right parenthesis
                    while let Some(top_op) = op_stack.pop() {
                        if top_op.0.token_type == TokenType::LParen {
                            break;
                        } else {
                            let right = expr_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "Right hand side of expression is empty", &token.location);
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                self.error("Couldn't parse expression", "Left hand side of expression is empty", &token.location);
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            
                            let node = Box::new(ASTNode {
                                token: top_op.0.clone(),
                                node: Box::new(NodeType::Operator(Expression{
                                    left,
                                    right,
                                    operator: top_op.0
                                })),
                            });
                            
                            expr_stack.push((node, *i));
                        }
                    }
                    
                    // handle tuple
                    let tuple_node = self.expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack);
                    op_stack.clear();
                    expr_stack.clear();

                    if let Some(final_node) = tuple_node {
                        tuple_vec.push(final_node);
                    }
                    else {
                        self.error("Failed to parse tuple", "Couldn't parse tuple", &token.location);
                        return Box::new(ASTNode::none());
                    }
                }
                else {
                    self.error("Unexpected token in expression", "Didn't expect this token in expression. Maybe you meant to use a comma in a tuple, If that's so, put inside parentheises", &token.location);
                    return Box::new(ASTNode::none());
                }
            }
            else if token.token_type == TokenType::DoubleArrow {
                if expr_stack.is_empty() {
                    self.error("Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
                    return Box::new(ASTNode::none());
                }
                let parameters_node = expr_stack.pop().unwrap_or_else(|| {
                    self.error("Couldn't parse lamda", "No parameters for lambda were found: `x => x + 1`", &token.location);
                    return (Box::new(ASTNode::none()), 0);
                }).0;

                let parameters = NodeParameters { 
                    parameters: {
                        let mut tuple_vector_clone = tuple_vec.clone();
                        tuple_vec.clear();
                        tuple_vector_clone.append(&mut vec![parameters_node]);
                        tuple_vector_clone
                    }
                };

                // check if body is code block or expression: `x => x + 1` or `x => { x + 1 }`
                let body = if tokens.get(*i + 1).is_some().then(|| tokens[*i + 1].token_type == TokenType::LBrace).unwrap_or(false) {
                    // code block
                    *i += 1;
                    self.get_code_block(tokens, i, true)
                } else {
                    // expression
                    let mut lambda_tokens = vec![];
                    *i += 1;
                    let mut parenthesis_level = 0;
                    let mut brace_level = 0;
                    let mut bracket_level = 0;

                    while let Some(token) = tokens.get(*i) {
                        match token.token_type {
                            TokenType::LParen => {
                                parenthesis_level += 1;
                                lambda_tokens.push(token.clone());
                            }
                            TokenType::RParen => {
                                parenthesis_level -= 1;
                                lambda_tokens.push(token.clone());
                                if parenthesis_level <= 0 {
                                    break;
                                }
                            }
                            TokenType::LBrace => {
                                brace_level += 1;
                                lambda_tokens.push(token.clone());
                            }
                            TokenType::RBrace => {
                                brace_level -= 1;
                                lambda_tokens.push(token.clone());
                                if brace_level <= 0 {
                                    break;
                                }
                            }
                            TokenType::LBracket => {
                                bracket_level += 1;
                                lambda_tokens.push(token.clone());
                            }
                            TokenType::RBracket => {
                                bracket_level -= 1;
                                lambda_tokens.push(token.clone());
                                if bracket_level <= 0 {
                                    break;
                                }
                            }
                            _ => {
                                lambda_tokens.push(token.clone());
                            }
                        }
                        *i += 1;
                    }

                    let expression = self.get_entire_expression(&mut lambda_tokens);
                    BodyRegion {
                        body: vec![Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::ReturnExpression(expression)),
                        })]
                    }
                };

                let lambda = LambdaExpression {
                    parameters,
                    body,
                };

                expr_stack.push((Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::LambdaExpression(lambda)),
                }), *i));
            }
            else {
                self.error("Unexpected token in expression", "Didn't expect this token in expression", &token.location);
                return Box::new(ASTNode::none());
            }
    
            *i += 1;
            if is_1_expression && paran_index == 0 && !last_was_ident && !(tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::QuestionMark) {
                break;
            }
        }
    
        let expression_node = self.expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack);
    
        if let Some(final_node) = expression_node {
            tuple_vec.push(final_node);
        } else {
            return Box::new(ASTNode::none());
        }
        
        if tuple_vec.len() == 1 {
            return tuple_vec[0].clone();
        }

        let node = Box::new(NodeType::TupleExpression({
            NodeParameters { parameters: tuple_vec }
        }));

        return Box::new(ASTNode {
            token: Box::new(Token::new_empty()),
            node,
        })
    }

    fn expression_stacks_to_ast_node(&mut self, op_stack: &mut Vec<(Box<Token>, usize)>, expr_stack: &mut Vec<(Box<ASTNode>, usize)>) -> Option<Box<ASTNode>> {
        while let Some(operator) = op_stack.pop() {
            let right = expr_stack.pop();
            let left = expr_stack.pop();
    
            let node = Box::new(ASTNode {
                token: operator.0.clone(),
                node: Box::new(NodeType::Operator(Expression {
                    left: left.unwrap_or_else(|| { 
                        self.error("Couldn't parse expression", "Left hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::none()), 0)
                    }).0,
                    right: right.unwrap_or_else(|| { 
                        self.error("Couldn't parse expression", "Right hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::none()), 0)
                    }).0,
                    operator: operator.0.clone(),
                })),
            });
    
            expr_stack.push((node, 0));
        }

        return expr_stack.pop().map(|x| x.0);
    }

    fn get_expression_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Box<ASTNode>> {
        let all_tokens = self.get_node_parameters(tokens, i);
        if all_tokens.len() == 1 && all_tokens[0].len() == 0 {
            return vec![];
        }

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_entire_expression(&mut tokens));
        }

        return_tokens
    }
    
    fn get_type_idententifier(&mut self, tokens: &mut Vec<Box<Token>>) -> Box<ASTNode> {
        if let Some(first_token) = tokens.clone().first() {
            let mut uneeded_index = 0_usize;
            if first_token.token_type == TokenType::Identifier || first_token.token_type == TokenType::Auto {
                if tokens.len() == 1 {
                    // TYPE
                    // parser.message("TYPE", "TYPE", &first_token.location);
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(ScopedType {
                            scope: vec![TypeIdentifier {
                                name: first_token.clone(),
                                type_parameters: None,
                                scope_type: None,
                            }],
                            is_array: false
                        }))
                    });
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::LBracket) {
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(ScopedType {
                            scope: vec![TypeIdentifier {
                                name: first_token.clone(),
                                type_parameters: None,
                                scope_type: None,
                            }],
                            is_array: true
                        }))
                    });
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::LessThan) {
                    // Type<TYPE, TYPE>
                    // parser.message("Type<TYPE, TYPE>", "Type<TYPE, TYPE>", &first_token.location);
                    let type_with_parameters = self.get_scoped_typed(tokens, &mut uneeded_index);
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(type_with_parameters)),
                    });
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::DoubleColon || t.token_type == TokenType::Dot) {
                    // Scope::Type
                    // parser.message("Scope::Type", "Scope::Type", &first_token.location);
                    let scope_and_types = self.get_scoped_typed(tokens, &mut uneeded_index);
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(scope_and_types)),
                    });
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::Comma) {
                    self.error("Incorrect type", "Multiple variables in one declaration is not supported, try instead using a tuple by putting parenthesis around types", &tokens.get(1).unwrap().location);
                    return Box::new(ASTNode::none());
                }
                else {
                    self.error("Incorrect type", "Expected a tuple or type before the `::` or  `.`", &tokens.get(1).unwrap().location);
                    return Box::new(ASTNode::none());
                }
            }
            else if first_token.token_type == TokenType::LParen {
                // Tuple
                // parser.message("Tuple", "Tuple", &first_token.location);
                let tuple = self.get_tuple_node_parameters(tokens, &mut uneeded_index);
                return Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::TupleDeclaration(NodeParameters {
                        parameters: tuple
                    }))
                });
            }
            else if first_token.token_type == TokenType::LBrace {
                self.error("Incorrect type", "Incorrect type declaration `type: name`, objects `{}` are not supported as a type", &tokens.get(1).unwrap().location);
                return Box::new(ASTNode::none());
            }
            else if first_token.token_type == TokenType::LBracket {
                self.error("Incorrect type", "Incorrect type declaration. If you were trying to create an array, do: `type[]`", &tokens.get(1).unwrap().location);
                return Box::new(ASTNode::none());
            }
            else {
                self.error("Variable declaration has incorrect type", format!("Expected tuple or type, found: `{}`", first_token.value).as_str(), &first_token.location);
                return Box::new(ASTNode::none());
            }
        }
        else {
            self.error("Variable declaration has no type", "Expected a type before the `:`, no type was provided", &tokens.get(1).unwrap().location);
            return Box::new(ASTNode::none());
        }
    }

    fn get_tuple_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Box<ASTNode>> {
        // (Tuple, Type) 
        let all_tokens = self.get_node_parameters(tokens, i);

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_type_idententifier(&mut tokens));
        }

        return_tokens
    }

    fn get_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<Vec<Box<Token>>> {
        let mut comma_count = 0;
        let mut angle_bracket_level = 0;
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        let mut paranthesis_level = 0;
        let mut brace_level = 0;
        let mut bracket_level = 0;

        if let Some(token) = tokens.get(*i) {
            if token.token_type != TokenType::LParen {
                self.error("Error getting node parameters", "Expected to start with `(`", &token.location);
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
            if paranthesis_level == 0 && angle_bracket_level == 0 && brace_level == 0 && bracket_level == 0 {
                break;
            }
        }
        all_tokens
    }

    fn get_scoped_typed(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedType {
        // Type<TYPE, TYPE>
        let name = tokens[*i].clone();
        *i += 1;
        let first_parameters = self.get_type_parameters(tokens, i);
        let mut scope = vec![TypeIdentifier {
            name,
            scope_type: None,
            type_parameters: if first_parameters.parameters.len() > 0 {
                Some(first_parameters)
            } else {
                None
            }
        }];
        if let Some(token) = tokens.get(*i) {
            if token.token_type == TokenType::Dot || token.token_type == TokenType::DoubleColon {
                scope = self.get_type_scope(tokens, i);
            }
        }

        let is_array = self.next_is_array_for_type(tokens, i);

        ScopedType {
            is_array,
            scope
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
            return_tokens.push(self.get_type_idententifier(&mut tokens));
        }

        NodeParameters {
            parameters: return_tokens
        }
    }

    fn get_ident_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ScopedIdentifier { 
        let mut iterate = tokens.iter().skip(*i).peekable();
        let mut scope = ScopedIdentifier { 
            scope: vec![Identifier { 
                name: tokens[*i].clone(),
                expression: None,
                scope_type: None
            }
        ]};
        let mut last_punc: Option<ScopeType> = None;
        let mut first_token = true;

        while let Some(token) = iterate.next() {
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        self.error("Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        self.error("Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::Dot);
                }
                TokenType::Identifier => {
                    *i += 1;
                    if first_token {
                        first_token = false;
                        continue;
                    }
                    if last_punc.is_none() {
                        self.error("Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
                        return scope;
                    }

                    scope.scope.push(Identifier {
                        name: token.clone(),
                        expression: None,
                        scope_type: last_punc.clone(),
                    });
                    last_punc = None;
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

    fn get_type_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> Vec<TypeIdentifier> { 
        let mut iterate = tokens.iter().skip(*i).peekable();
        let mut scope = vec![TypeIdentifier { 
            name: tokens[*i].clone(),
            type_parameters: None,
            scope_type: None
        }];
        let mut last_punc: Option<ScopeType> = None;
        let mut angle_bracket_level = 0;
        let mut angle_bracket_insides = vec![];

        while let Some(token) = iterate.next() {
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() {
                        self.error("Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() {
                        self.error("Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                        return scope;
                    }
                    last_punc = Some(ScopeType::Dot);
                }
                TokenType::Identifier => {
                    if angle_bracket_level != 0 {
                        angle_bracket_insides.push(token.clone());
                        continue;
                    }

                    *i += 1;
                    if last_punc.is_none() {
                        self.error("Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
                        return scope;
                    }

                    scope.push(TypeIdentifier {
                        name: token.clone(),
                        scope_type: last_punc.clone(),
                        type_parameters: if angle_bracket_insides.len() != 0 {
                            Some(self.get_type_parameters(&mut angle_bracket_insides, i))
                        } else {
                            None
                        }
                    });
                    last_punc = None;
                }
                TokenType::LessThan => {
                    angle_bracket_level += 1;
                    angle_bracket_insides.push(token.clone());
                }
                TokenType::GreaterThan => {
                    angle_bracket_level -= 1;
                    angle_bracket_insides.push(token.clone());
                }
                _ => {
                    if angle_bracket_level != 0 {
                        angle_bracket_insides.push(token.clone());
                    }
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
                format!("{}{}", scope, array)
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
                    let identifier = ident.name.value.clone();
                    if let Some(expression) = &ident.expression {
                        scope += format!("{}{}", scope_type, Self::node_expr_to_string(expression)).as_str();
                    }
                    else {
                        scope += format!("{}{}", scope_type, identifier).as_str();
                    }
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