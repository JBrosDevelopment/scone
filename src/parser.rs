use crate::ast::*;
use crate::lexer::{Token, TokenType};

pub fn generate_ast(lexer_tokens: Vec<Token>) -> Result<Vec<Box<dyn ASTNode>>, String> {
    let mut ast: Vec<Box<dyn ASTNode>> = Vec::new();

    // Split tokens into lines
    let lines: Vec<Vec<Token>> = lexer_tokens.split(|t| t.token_type == TokenType::EndOfLine).map(|l| l.to_vec()).filter(|x| !x.is_empty()).collect();

    let mut line_index = 0; 
    while line_index < lines.len() {        
        let line = lines[line_index].clone();
        let mut i = 0;
        while i < line.len() {

            match line[i].clone().token_type {
                TokenType::Identifier => {
                    // type: name = value
                    // type<anonymous, args>: name = valued
                    // type: name()
                    // type<anonymous, args>: name()
                    let type_identifier = (Some(AnonymousTypeParameter { type_simple: line[i].clone(), type_complex: None}), None); ///////////////////////
                    ast.push(declare_variable_or_function(line.clone(), i, (None, None), type_identifier)?);
                    line_index += 1;
                    break;
                }
                _ => {

                }
            }
            i += 1;
        } 
    }

    Ok(ast)
}

pub fn declare_variable_or_function(line: Vec<Token>, i: usize, options: (Option<AccessModifier>, Option<VariableModifier>), type_identifier: Type) -> Result<Box<dyn ASTNode>, String> {
    let colon_token = line[i + 1].clone(); 
    if colon_token.token_type == TokenType::Colon {
        // type: name = value
        // type: name()
        
        let type_token = line[i].clone();
        let name_token = line[i + 2].clone();
        if line[i + 3].token_type == TokenType::Assign { 
            // type: name = value
            println!("{}: {} = value", type_token.value, name_token.value);

            let return_expression = parse_value_expression(line.clone(), i + 4)?;
            let value = return_expression.0;
            let access_modifier = options.0.unwrap_or(AccessModifier::None);
            let description = return_expression.1;
            let variable_modifier = options.1.unwrap_or(VariableModifier::None);

            let var_declaration = ASTNodeVariableDeclaration { 
                token: type_token.clone(), 
                name: name_token.value, 
                type_identifier,
                value,
                access_modifier,
                description,
                variable_modifier
            };
            
            //println!("{:?}", var_declaration);
            return Ok(Box::new(var_declaration));
        }
        else if line[i + 3].token_type == TokenType::LParen || line[i + 3].token_type == TokenType::LessThan {
            // type: name()
            // type<anonymous, args>: name()
            println!("{}: {}()", type_token.value, name_token.value);

            let access_modifier = options.0.unwrap_or(AccessModifier::None);

            let function_declaration = ASTNodeFunctionDeclaration { 
                token: type_token.clone(), 
                name: name_token.value, 
                type_identifier,
                access_modifier,

                description: None,
                parameters: vec![],
                type_parameters: None,
                body: None,
            };
            
            //println!("{:?}", function_declaration);
            return Ok(Box::new(function_declaration));
        }
        else {
            return Err(format!("unexpected token while parsing declaration: {:?}", name_token.value));
        }
    }
    else {
        return Err("could not parse declaration".to_string());
    }
}
pub fn precedence(op: String) -> i32 {
    match op.as_str() {
        "=" => 1,
        "->" | "<-" | "=>" => 1,
        ":" => 3,
        ">" | "<" | ">=" | "<=" => 4,
        "+" | "-" => 5,
        "*" | "/" => 6,
        "&&" | "||" => 7,
        "^" | "|" | "&" => 8,
        "%" => 9,
        _ => 0,
    }
}
fn get_parameter_args() {

}
fn parse_value_expression(line: Vec<Token>, mut i: usize) -> Result<(Option<Box<dyn ASTNode>>, Option<String>), String> {
    let mut operator_stack: Vec<Token> = Vec::new();
    let mut expr_stack: Vec<Box<dyn ASTNode>> = Vec::new();
    let mut tokens = line.into_iter().skip(i).peekable();
    let mut last_was_value = false;
    let mut return_description = None;

    while let Some(token) = tokens.peek() {
        match token.token_type.clone() {
            TokenType::LParen => {
                operator_stack.push((*token).clone());
                tokens.next();
                i += 1;
                last_was_value = false;
            }
            TokenType::RParen => {
                while operator_stack.last().map_or(false, |op| op.token_type != TokenType::LParen) && expr_stack.len() > 1 {
                    let operator = operator_stack.pop().unwrap();
                    let e2 = expr_stack.pop();
                    let e1 = expr_stack.pop();
                    expr_stack.push(Box::new(ASTNodeExpression {token: operator, lhs: e1, rhs: e2}));
                }
                operator_stack.pop(); // Pop the '('
                tokens.next();
                i += 1;
                last_was_value = false;
            }
            TokenType::NumberConstant => {
                expr_stack.push(Box::new(ASTNodeConstant { token: (*token).clone() }));
                tokens.next();
                i += 1;
                last_was_value = true;
            }
            TokenType::Identifier => {
                i += 1;
                if let Some(this_token) = tokens.next() {
                    let tt = this_token.clone();
                    if let Some(next_token) = tokens.peek() {
                        if next_token.token_type == TokenType::LParen || next_token.token_type == TokenType::LessThan {
                            if let Ok(return_function) = parse_parameter_and_type_arguments(tokens.clone().collect(), i.clone() as i32) {

                                let path = ObjectPath { name: tt.value.clone(), child: None }; //////////////////////////////////////////////////////////////

                                let function = ASTNodeFunctionCall {
                                    token: tt.clone(),
                                    type_parameters: return_function.0,
                                    argumments: return_function.1.unwrap(),
                                    path
                                };

                                expr_stack.push(Box::new(function));

                                for _ in 0..return_function.2 {
                                    tokens.next();
                                    i += 1;
                                }
                            }
                            else {
                                return Err("Could not parse function call".to_string());
                            }
                        }
                        else {
                            expr_stack.push(Box::new(ASTNodeExpression { token: tt, lhs: None, rhs: None }));
                        }
                    }
                }
                last_was_value = true;
            }
            TokenType::Not => {
                let operator = Token::new(TokenType::Not,"!".to_string(), String::new(), token.location.clone());
                let e1 = expr_stack.pop();
                let e2 = None;
                expr_stack.push(Box::new(ASTNodeExpression {token: operator, lhs: e1, rhs: e2}));
                
                tokens.next();
                i += 1;
                last_was_value = false;
            }
            TokenType::Minus => {
                if last_was_value {
                    while operator_stack.last().map_or(false, |top| precedence(top.value.clone()) >= precedence(token.value.clone())) && expr_stack.len() > 1 {
                        let operator = operator_stack.pop().unwrap();
                        let e2 = expr_stack.pop();
                        let e1 = expr_stack.pop();
                        expr_stack.push(Box::new(ASTNodeExpression {
                            token: operator.clone(),
                            lhs: e1,
                            rhs: e2,
                        }));
                    }
                    operator_stack.push(token.clone());
                } else {
                    // We can advance tokens directly here with tokens.next()
                    let operator = Token::new(TokenType::Minus, "-".to_string(), String::new(), token.location.clone());
                    let e1 = ASTNodeConstant {
                        token: Token::new(TokenType::NumberConstant, "0".to_string(), String::new(), token.location.clone()),
                    };
                    i += 1;
                    if let Some(_) = tokens.next() {
                        i += 1;
                        if let Some(next_token) = tokens.next() {   
                            let e2 = ASTNodeConstant { token: next_token.clone() };
                            expr_stack.push(Box::new(ASTNodeExpression {
                                token: operator.clone(),
                                lhs: Some(Box::new(e1)),
                                rhs: Some(Box::new(e2)),
                            }));
                        }
                    }
                }
                last_was_value = false;
            }
            TokenType::RightArrow => {
                if return_description.is_none() {
                    i += 1;
                    if let Some(description) = tokens.next() {
                        return_description = Some(description.value);
                        if description.token_type != TokenType::StringConstant {
                            return Err("Could not parse expression description. Expected description to be a string".to_string())
                        }
                        i += 1;
                        if let Some(_) = tokens.next() {
                            return Err("Could not parse expression description. Unexpected trailing token".to_string());
                        }
                    }
                    else {
                        return Err("Could not parse expression description. No string description found".to_string())
                    }
                }
                else {
                    return Err("Could not parse expression description".to_string())
                }
            }
            _ => {
                if TokenType::is_operator(&token.token_type) {
                    while operator_stack.last().map_or(false, |top| precedence(top.value.clone()) >= precedence(token.value.clone())) && expr_stack.len() > 1 {
                        let operator = operator_stack.pop().unwrap();
                        let e2 = expr_stack.pop();
                        let e1 = expr_stack.pop();
                        expr_stack.push(Box::new(ASTNodeExpression {token: operator.clone(), lhs: e1, rhs: e2}));
                    }
                    operator_stack.push((*token).clone());
                    tokens.next();
                    i += 1;
                    last_was_value = false;
                }
                else {
                    return Err(format!("Could not parse expression. Unexpected token: {}", token.value));
                }
            }
        }
    }
    
    let mut i = 0;
    while let Some(operator) = operator_stack.pop() {
        i += 1;
        if i < 99 {
            let mut e1 = None;
            let mut e2 = None;
            if !expr_stack.is_empty() {
                e2 = expr_stack.pop();
            }
            if !expr_stack.is_empty() {
                e1 = expr_stack.pop();
            }
            expr_stack.push(Box::new(ASTNodeExpression {token: operator, lhs: e1, rhs: e2}));
        }
    }

    let returns = expr_stack.pop().unwrap();

    Ok((Some(returns), return_description))
}

fn parse_parameter_and_type_arguments(tokens_vec: Vec<Token>, mut i: i32) -> Result<(Option<Vec<Type>>, Option<Vec<Box<dyn ASTNode>>>, i32), String> {
    let mut skipped = 0;
    let mut types: Vec<AnonymousTypeParameter> = Vec::new();
    let mut args: Vec<Box<dyn ASTNode>> = Vec::new();
    let mut type_tokens: Vec<Vec<Token>> = vec![Vec::new()];
    let mut arg_tokens: Vec<Vec<Token>> = vec![Vec::new()];
    let mut types_has_started = false;
    let mut args_has_started = false;
    let mut has_started = false;
    let mut parenthesis_count = 0;
    let mut bracket_count = 0;
    let mut brace_count = 0;
    let mut chevron_count = 0;
    let mut comma_count = 0;
    let mut tokens = tokens_vec.iter().peekable();
    while let Some(token) = tokens.next() {
        i += 1;
        skipped += 1;
        match token.token_type {
            TokenType::LessThan => {
                chevron_count += 1;
                if !args_has_started {
                    if !has_started {
                        has_started = true;
                    }
                    else {
                        type_tokens[comma_count].push(token.clone());
                    }
                    types_has_started = true;
                }
                else {
                    arg_tokens[comma_count].push(token.clone());
                }
            }
            TokenType::GreaterThan => {
                chevron_count -= 1;
                if !args_has_started {
                    if chevron_count == 0 {
                        types_has_started = false;
                        has_started = false;
                        comma_count = 0;
                        parenthesis_count = 0;
                        bracket_count = 0;
                        brace_count = 0;
                    }
                    else {
                        type_tokens[comma_count].push(token.clone());
                    }
                }
                else {
                    arg_tokens[comma_count].push(token.clone());
                }
            }
            TokenType::Comma => {
                if types_has_started {
                    if chevron_count == 1 && parenthesis_count == 0 && brace_count == 0 && bracket_count == 0 {
                        type_tokens.push(vec![]);
                        comma_count += 1;
                    }
                    else {
                        type_tokens[comma_count].push(token.clone());
                    }
                }
                else if args_has_started { 
                    if chevron_count == 0 && parenthesis_count == 1 && brace_count == 0 && bracket_count == 0 {
                        arg_tokens.push(vec![]);
                        comma_count += 1;
                    }
                    else {
                        arg_tokens[comma_count].push(token.clone());
                    }
                }
                else {
                    return Err("Could not parse function call. Invalid comma".to_string());
                }
            }
            TokenType::LBrace | TokenType::LBracket | TokenType::RBrace | TokenType::RBracket => {
                match token.token_type {
                    TokenType::LBrace => brace_count += 1,
                    TokenType::LBracket => bracket_count += 1,
                    TokenType::RBrace => brace_count -= 1,
                    TokenType::RBracket => bracket_count -= 1,
                    _ => {}
                }
                if types_has_started {
                    type_tokens[comma_count].push(token.clone());
                }
                else if args_has_started { 
                    arg_tokens[comma_count].push(token.clone());
                }
                else {
                    return Err("Could not parse function call. Invalid brace or bracket".to_string());
                }
            }
            TokenType::LParen => {
                parenthesis_count += 1;
                if !types_has_started {
                    if !args_has_started {
                        args_has_started = true;
                    }
                    else {
                        arg_tokens[comma_count].push(token.clone());
                    }
                    args_has_started = true;
                }
                else {
                    if types_has_started {
                        type_tokens[comma_count].push(token.clone());
                    }
                    else if args_has_started { 
                        arg_tokens[comma_count].push(token.clone());
                    }
                    else {
                        return Err("Could not parse function call. Invalid parenthesis".to_string());
                    }
                }
            }
            TokenType::RParen => {
                parenthesis_count -= 1;
                if args_has_started {
                    if parenthesis_count == 0 {
                        break;
                    }
                    else {
                        arg_tokens[comma_count].push(token.clone());
                    }
                }
                else {
                    if types_has_started {
                        type_tokens[comma_count].push(token.clone());
                    }
                    else if args_has_started { 
                        arg_tokens[comma_count].push(token.clone());
                    }
                    else {
                        return Err("Could not parse function call. Invalid parenthesis".to_string());
                    }
                }
            }
            _ => {
                if args_has_started {
                    arg_tokens[comma_count].push(token.clone());
                }
                else if types_has_started {
                    type_tokens[comma_count].push(token.clone());
                }
                else {
                    return Err("Could not parse function call. Unexpected token".to_string());
                }
            }
        }
    }
    let mut type_parameters: Vec<Type> = Vec::new();
    let mut arg_parameters: Vec<Box<dyn ASTNode>> = Vec::new();

    for (index, type_tokens) in type_tokens.iter().enumerate() {
        let result = parse_parameter_and_type_arguments(type_tokens.clone(), index as i32)?;
        let value = result.0.unwrap()[0].clone();
        type_parameters.push(value);
    }

    for (index, arg_tokens) in arg_tokens.iter().enumerate() {
        let result = parse_parameter_and_type_arguments(arg_tokens.clone(), index as i32)?;
        let value: Box<dyn ASTNode> = result.1.unwrap()[0].clone();
        arg_parameters.push(value);
    }

    let wrapped_types = if type_parameters.len() > 0 { Some(type_parameters) } else { None };
    let wrapped_args = if arg_parameters.len() > 0 { Some(arg_parameters) } else { None };

    return Ok((wrapped_types, wrapped_args, skipped));    
}
