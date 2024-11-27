use crate::lexer::{Token, TokenType, Location};
use crate::ast::*;
use crate::error_handling::ErrorHandling;

pub fn parse(tokens: Vec<Token>, file: &String, code: &String) -> Result<Vec<ASTNode>, String> {
    let mut parser = Parser::new(Some(file.clone()), code.clone());
    parser.generate_ast(tokens);
    parser.output.print_messages();
    Ok(parser.ast)
}

#[derive(Debug, Clone)]
pub struct Parser {
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
            let mut assign_index = -1;
            for (index, token) in tokens.iter().enumerate() {
                match token.token_type {
                    TokenType::Assign => { assign_index = index as i32; break; },
                    TokenType::Colon => is_func = 1,
                    TokenType::Identifier => if is_func == 1 { is_func = 2 } else { is_func = 0 },
                    TokenType::LParen => if is_func == 2 { is_func = 3 } else { is_func = 0 },
                    TokenType::LessThan => if is_func == 2 { is_func = 3 } else { is_func = 0 },
                    _ => is_func = 0
                }
            }

            if assign_index != -1 {
                let lhs = tokens[0..assign_index as usize].to_vec();
                let rhs = tokens[assign_index as usize + 1..].to_vec();
                
                if lhs.len() >= 2 && lhs[lhs.len() - 2].token_type == TokenType::Colon {
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
                            _ => break
                        }
                        accessing_end_index += 1;
                    }

                    let description: Option<Box<Token>> = rhs.iter().position(|x| x.token_type == TokenType::RightArrow).and_then(|arrow_index| rhs.get(arrow_index + 1).cloned());
                    if let Some(des) = description.clone() {
                        if des.token_type != TokenType::StringConstant {
                            parser.error("variable description must be a string", "Description must be a string. Try placing quotes arounf it", &des.location);
                        }
                    }

                    let var_value: Box<ASTNode> = Self::set_node_with_children(parser, &rhs).unwrap_or_else(|_| Box::new(ASTNode::none()));

                    // println!("{}", Self::print_node_expression(&var_value));

                    let type_tokens = lhs[accessing_end_index..lhs.len() - 2].to_vec();
                    let var_type: Box<ASTNode> = Self::get_type_from_tokens(&type_tokens, &lhs[lhs.len() - 2].location, parser);

                    let var_decl = VariableDeclaration {
                        access_modifier,
                        var_name: var_name.clone(),
                        description,
                        var_value,
                        var_type,
                    };
                    ast.push(ASTNode {
                        token: var_name.clone(),
                        node: Box::new(NodeType::VariableDeclaration(var_decl)),
                    });
                }
                else {
                    // assigning variable

                    let left = Self::set_node_with_children(parser, &lhs).unwrap_or_else(|_| Box::new(ASTNode::none()));
                    let right = Self::set_node_with_children(parser, &rhs).unwrap_or_else(|_| Box::new(ASTNode::none()));

                    let assign = Assignment { left, right };
                    ast.push(ASTNode{
                        token: lhs[0].clone(),
                        node: Box::new(NodeType::Assignment(assign))
                    });
                }

                line_index += 1;
                continue;
            }

            if is_func == 3 {

                line_index += 1;
                continue;
            }

            let mut token_index = 0;
            match tokens[0].token_type {
                TokenType::Identifier => {
                    match tokens[1].token_type {
                        TokenType::Colon | TokenType::Assign => { // Previously handled
                            parser.error("Previously handled token", "Expected declaration to be found with patterns: '=', ': IDENT <'', or ': IDENT ('", &tokens[0].location);
                        }
                        TokenType::LParen => { // Function call
                            ast.push(ASTNode {
                                token: tokens[0].clone(),
                                node: Self::function_from_tokens(&tokens, parser, None, &mut token_index),
                            });
                        }
                        TokenType::DoubleColon => { // Traverse Scope
                            ast.push(ASTNode {
                                token: tokens[0].clone(),
                                node: Self::traverse_scope_from_tokens(&tokens, parser, &mut token_index),
                            });
                        }
                        _ => {
                            parser.error("Unexpected token", "Unexpected token after identifier", &tokens[0].location);
                        }
                    }
                }
                TokenType::Break => {
                    ast.push(ASTNode {
                        token: tokens[0].clone(),
                        node: Box::new(NodeType::Break(tokens[0].clone())),
                    });
                }
                TokenType::Continue => {
                    ast.push(ASTNode {
                        token: tokens[0].clone(),
                        node: Box::new(NodeType::Continue(tokens[0].clone())),
                    });
                }
                _ => {
                    parser.error("Unexpected token", "Did not expect token to begin line", &tokens[0].location);
                }
            }

            line_index += 1;
        }

        ast
    }

    pub fn function_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, scope: Option<ScopeToIdentifier>, i: &mut usize) -> Box<NodeType> {
        *i -= 1;

        let mut node_parameters: Vec<Box<ASTNode>> = vec![];
        let mut type_parameters: Vec<Box<ASTNode>> = vec![];
        let mut name = tokens[*i].clone();

        if tokens[*i + 1].token_type == TokenType::LessThan {
            let type_parameters_from_tokens = Self::get_type_parameters_from_tokens(&tokens, parser, i);
            type_parameters = type_parameters_from_tokens.1;
            name = type_parameters_from_tokens.0;
        }
        if tokens[*i + 1].token_type == TokenType::LParen {
            *i += 1;
            node_parameters = Self::get_expression_node_parameters(&tokens, parser, i);
        }
        else {
            parser.error("Invalid token for funciton call", format!("Expected either '<' or '(' for function call, got: {}", tokens[*i + 1].value).as_str(), &tokens[*i].location);
        }

        *i -= 1;

        Box::new(NodeType::FunctionCall(FunctionCall {
            parameters: NodeParameters { parameters: node_parameters },
            scope: scope.unwrap_or_else(|| ScopeToIdentifier { identifier: name, child: None, scope_type: None }),
            type_parameters: type_parameters.is_empty().then(|| None).unwrap_or(Some(type_parameters)),
        }))
    }

    pub fn traverse_scope_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> Box<NodeType> {
        *i -= 1;

        let (scope, type_parameters) = Self::get_scope_from_tokens(&tokens, parser, i);
        if type_parameters.is_some() {
            return Self::function_from_tokens(&tokens, parser, Some(scope), i);
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            return Self::function_from_tokens(&tokens, parser, Some(scope), i);
        }
        else {
            *i -= 1;
            return Box::new(NodeType::Identifier(scope));
        }
    }

    pub fn set_node_with_children(parser: &mut Parser, tokens: &Vec<Box<Token>>) -> Result<Box<ASTNode>, &'static str> {
        let mut expr_stack: Vec<Box<ASTNode>> = Vec::new(); 
        let mut op_stack: Vec<Box<Token>> = Vec::new();
        let mut last_was_ident = false;
        let mut tuple_vec: Vec<Box<ASTNode>> = Vec::new();
        let mut is_inside_parenthesis = false;
        
        let mut i = 0;
        while i < tokens.len() {
            let token = tokens[i].clone();
    
            if token.token_type == TokenType::RightArrow {
                break;
            }
            else if token.token_type.is_operator() {
                if last_was_ident && token.token_type == TokenType::LessThan {
                    // part of function call: function<TYPE>()
                    expr_stack.push(Box::new(ASTNode { 
                        token: token.clone(),
                        node: Self::function_from_tokens(&tokens, parser, None, &mut i),
                    }));
                }
                else {
                    while let Some(top_op) = op_stack.last() {
                        // Check if the top operator has higher precedence, and if so, pop it to expr_stack
                        if token.token_type.precedence() <= top_op.token_type.precedence() {
                            let operator = op_stack.pop().unwrap();
                            let right = expr_stack.pop().unwrap();
                            let left = expr_stack.pop().unwrap();
                            
                            let node = Box::new(ASTNode {
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
                    op_stack.push(token); 
                }
            }
            else if token.token_type == TokenType::DoubleColon {
                if last_was_ident {
                    // scope traversal
                    expr_stack.push(Box::new(ASTNode { 
                        token: token.clone(),
                        node: Self::traverse_scope_from_tokens(&tokens, parser, &mut i),
                    }));
                }
                else {
                    parser.error("Expected identifier before double colon", "Expected identifier before double colon: IDENT::IDENT", &token.location);
                }
            }
            else if token.token_type == TokenType::Dot {
                if last_was_ident {
                    // scope traversal
                    expr_stack.push(Box::new(ASTNode { 
                        token: token.clone(),
                        node: Self::traverse_scope_from_tokens(&tokens, parser, &mut i),
                    }));
                }
                else {
                    parser.error("Expected identifier before dot", "Expected identifier before dot: IDENT.IDENT", &token.location);
                }
            }
            else if token.token_type == TokenType::LParen {
                if last_was_ident {
                    // part of function call
                    expr_stack.push(Box::new(ASTNode { 
                        token: token.clone(),
                        node: Self::function_from_tokens(&tokens, parser, None, &mut i),
                    }));
                }
                else {
                    // part of expression
                    is_inside_parenthesis = true;
                    op_stack.push(token);
                }
            }
            else if token.token_type == TokenType::RParen {
                is_inside_parenthesis = false;
                while let Some(top_op) = op_stack.pop() {
                    if top_op.token_type == TokenType::LParen {
                        break; 
                    } else {
                        let right = expr_stack.pop().unwrap();
                        let left = expr_stack.pop().unwrap();
                        
                        let node = Box::new(ASTNode {
                            token: top_op.clone(),
                            node: Box::new(NodeType::Operator(Expression{
                                left,
                                right,
                                operator: top_op.clone(),
                            })),
                        });
                        
                        expr_stack.push(node);
                    }
                }
            }
            else if token.token_type.is_constant() {
                let mut node = Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::None),
                });

                if let Ok(constant_type) = Self::get_constant_type(&token) {
                    node.node = Box::new(NodeType::Constant(ConstantNode {
                        value: token.clone(),
                        constant_type,
                    }));
                } else {
                    parser.error("Could not parse constant", "Couldn't decide type from constant", &token.location);
                    return Err("could not get constant type");
                }

                expr_stack.push(node);
            }
            else if token.token_type == TokenType::Identifier {
                if tokens.get(i + 1).is_some() && (tokens[i + 1].token_type == TokenType::DoubleColon || tokens[i + 1].token_type == TokenType::Dot || tokens[i + 1].token_type == TokenType::LParen || tokens[i + 1].token_type == TokenType::LessThan) {
                    last_was_ident = true;
                }
                else {
                    let node = Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::Identifier(ScopeToIdentifier {
                            identifier: token.clone(),
                            child: None,
                            scope_type: None
                        })),
                    });
                    expr_stack.push(node);
                }
            }
            else if token.token_type == TokenType::Comma {
                if is_inside_parenthesis {
                    
                    // handle right parenthesis
                    while let Some(top_op) = op_stack.pop() {
                        if top_op.token_type == TokenType::LParen {
                            break; 
                        } else {
                            let right = expr_stack.pop().unwrap();
                            let left = expr_stack.pop().unwrap();
                            
                            let node = Box::new(ASTNode {
                                token: top_op.clone(),
                                node: Box::new(NodeType::Operator(Expression{
                                    left,
                                    right,
                                    operator: top_op.clone(),
                                })),
                            });
                            
                            expr_stack.push(node);
                        }
                    }
                    
                    // handle tuple
                    let tuple_node = Self::expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack, parser);
                    op_stack.clear();
                    expr_stack.clear();

                    if let Some(final_node) = tuple_node {
                        tuple_vec.push(final_node);
                    }
                    else {
                        parser.error("Failed to parse tuple", "Couldn't parse tuple", &token.location);
                        return Err("Failed to parse tuple");
                    }
                }
                else {
                    parser.error("Unexpected token in expression", "Didn't expect this token in expression. Maybe you meant to use a comma in a tuple, If that's so, put inside parentheises", &token.location);
                    return Err("Unexpected token in expression");
                }
            }
            else {
                parser.error("Unexpected token in expression", "Didn't expect this token in expression", &token.location);
                return Err("Unexpected token in expression");
            }
    
            i += 1;
        }
    
        let expression_node = Self::expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack, parser);
    
        if let Some(final_node) = expression_node {
            tuple_vec.push(final_node);
        } else {
            return Err("Failed to parse expression")
        }

        if tuple_vec.len() == 1 {
            return Ok(tuple_vec[0].clone());
        }

        let node = Box::new(NodeType::TupleExpression({
            NodeParameters { parameters: tuple_vec }
        }));

        return Ok(Box::new(ASTNode {
            token: Box::new(Token::new_empty()),
            node,
        }))
    }

    pub fn expression_stacks_to_ast_node(op_stack: &mut Vec<Box<Token>>, expr_stack: &mut Vec<Box<ASTNode>>, parser: &mut Parser) -> Option<Box<ASTNode>> {
        while let Some(operator) = op_stack.pop() {
            let right = expr_stack.pop();
            let left = expr_stack.pop();
    
            let node = Box::new(ASTNode {
                token: operator.clone(),
                node: Box::new(NodeType::Operator(Expression {
                    left: left.unwrap_or_else(|| { 
                        parser.error("Error parsing expression", "Left hand side of expression is empty", &operator.location); 
                        Box::new(ASTNode::none())
                    }),
                    right: right.unwrap_or_else(|| { 
                        parser.error("Error parsing expression", "Left hand side of expression is empty", &operator.location); 
                        Box::new(ASTNode::none())
                    }),
                    operator: operator.clone(),
                })),
            });
    
            expr_stack.push(node);
        }

        return expr_stack.pop()
    }

    #[allow(dead_code)]
    fn print_node_expression(node: &Box<ASTNode>) -> String {
        if let NodeType::Operator(ref value) = *node.node {
            let left = Self::print_node_expression(&value.left);
            let right = Self::print_node_expression(&value.right);

            format!("({} {} {})", left, value.operator.value, right)
        }
        else if let NodeType::Identifier(ref value) = *node.node {
            value.identifier.value.clone()
        }
        else if let NodeType::Constant(ref value) = *node.node {
            value.value.value.clone()
        }
        else {
            node.token.value.clone()
        }
    }

    pub fn get_constant_type(token: &Token) -> Result<ConstantType, &str> {
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

    pub fn get_expression_node_parameters(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> Vec<Box<ASTNode>> {
        let all_tokens = Self::get_node_parameters_from_tokens(tokens, parser, i);

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            if let Ok(ast_node) = Self::set_node_with_children(parser, &tokens) {
                return_tokens.push(ast_node);
            }
        }

        return_tokens
        
    }

    pub fn get_type_from_tokens(tokens: &Vec<Box<Token>>, location: &Location, parser: &mut Parser) -> Box<ASTNode> {
        if let Ok(ast_node) = Self::ast_node_from_tokens_var_decl(tokens, parser) {
            return ast_node;
        }

        parser.error("Variable declaration has no type", "Expected a type before the ':', no type was provided", &location);
        Box::new(ASTNode::none()) 
    }
    
    pub fn ast_node_from_tokens_var_decl(tokens: &Vec<Box<Token>>, parser: &mut Parser) -> Result<Box<ASTNode>, &'static str> {
        if let Some(first_token) = tokens.first() {
            let mut uneeded_index = 0_usize;
            if first_token.token_type == TokenType::Identifier {
                if tokens.len() == 1 {
                    // TYPE
                    // parser.message("TYPE", "TYPE", &first_token.location);
                    return Ok(Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(TypeIdentifier {
                            scope: ScopeToIdentifier { identifier: first_token.clone(), child: None, scope_type: None },
                            types: None
                        }))
                    }));
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::LessThan) {
                    // Type<TYPE, TYPE>
                    // parser.message("Type<TYPE, TYPE>", "Type<TYPE, TYPE>", &first_token.location);
                    let type_parameters = Self::get_type_parameters_from_tokens(tokens, parser, &mut uneeded_index);
                    return Ok(Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(TypeIdentifier {
                            scope: ScopeToIdentifier { identifier: type_parameters.0, child: None, scope_type: None },
                            types: Some(NodeParameters { parameters: type_parameters.1 })
                        }))
                    }));
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::DoubleColon || t.token_type == TokenType::Dot) {
                    // Scope::Type
                    // parser.message("Scope::Type", "Scope::Type", &first_token.location);
                    let scope_and_types = Self::get_scope_from_tokens(tokens, parser, &mut uneeded_index);
                    return Ok(Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::TypeIdentifier(TypeIdentifier {
                            scope: scope_and_types.0,
                            types: scope_and_types.1.is_some().then(|| NodeParameters { parameters: scope_and_types.1.unwrap() }) 
                        }))
                    }));
                }
                else if tokens.get(1).is_some_and(|t| t.token_type == TokenType::Comma) {
                    parser.error("Incorrect type", "Multiple variables in one declaration is not supported, try instead using a tuple by putting parenthesis around types", &tokens.get(1).unwrap().location);
                    return Err("Incorrect type");
                }
                else {
                    parser.error("Incorrect type", "Expected a tuple or type before the '::' or  '.'", &tokens.get(1).unwrap().location);
                    return Err("Incorrect type");
                }
            }
            else if first_token.token_type == TokenType::LParen {
                // Tuple
                // parser.message("Tuple", "Tuple", &first_token.location);
                let tuple = Self::get_tuple_node_parameters(tokens, parser, &mut uneeded_index);
                return Ok(Box::new(ASTNode {
                    token: first_token.clone(),
                    node: Box::new(NodeType::TupleDeclaration(NodeParameters {
                        parameters: tuple
                    }))
                }));
            }
            else {
                parser.error("Variable declaration has incorrect type", format!("Expected tuple or type, found: '{}'", first_token.value).as_str(), &first_token.location);
                return Err("Variable declaration has incorrect type")
            }
        }
        Err("Variable declaration has incorrect type")
    }

    pub fn get_tuple_node_parameters(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> Vec<Box<ASTNode>> {
        // (Tuple, Type) 
        let all_tokens = Self::get_node_parameters_from_tokens(tokens, parser, i);

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            if let Ok(ast_node) = Self::ast_node_from_tokens_var_decl(&tokens, parser) {
                return_tokens.push(ast_node);
            }
        }

        return_tokens
        
    }

    pub fn get_node_parameters_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> Vec<Vec<Box<Token>>> {
        let mut paran_count = 0;
        let mut comma_count = 0;
        let mut chevron_count = 0;
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];

        if let Some(token) = tokens.get(*i) {
            if token.token_type != TokenType::LParen {
                parser.error("Error getting node parameters", "Expected to start with '('", &token.location);
            }
        }

        for token in tokens.iter().skip(*i) {
            match token.token_type {
                TokenType::LParen => {
                    paran_count += 1;
                    if paran_count != 1 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::RParen => {
                    paran_count -= 1;
                    if paran_count != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::Comma => {
                    if chevron_count == 0 && paran_count == 1 {
                        comma_count += 1;
                        all_tokens.push(Vec::new());
                    }
                    else if paran_count != 1 || chevron_count != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::LessThan => {
                    chevron_count += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::GreaterThan => {
                    chevron_count -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                _ => {
                    all_tokens[comma_count].push(token.clone());
                }
            }
            *i += 1;
            if paran_count == 0 {
                break;
            }
        }
        all_tokens
    }

    pub fn get_type_parameters_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> (Box<Token>, Vec<Box<ASTNode>>) { // returns (name, parameters)
        // Type<With, Parameters>
        let mut chevron_count = 0;
        let mut paren_count = 0;
        let mut comma_count = 0;
        let mut all_tokens: Vec<Vec<Box<Token>>> = vec![vec![]];
        
        let mut name = tokens[0].clone();
        if let Some(token) = tokens.get(*i) {
            name = token.clone();
        }

        if let Some(token) = tokens.get(*i + 1) {
            if token.token_type != TokenType::LessThan {
                parser.error("Error getting type parameters", "Expected to start with '<'", &token.location);
            }
        }

        for token in tokens.iter().skip(*i + 1) {
            match token.token_type {
                TokenType::LessThan => {
                    chevron_count += 1;
                    if chevron_count != 1 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::GreaterThan => {
                    chevron_count -= 1;
                    if chevron_count != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::Comma => {
                    if paren_count == 0 && chevron_count == 1 {
                        comma_count += 1;
                        all_tokens.push(Vec::new());
                    }
                    else if chevron_count != 1 || paren_count != 0 {
                        all_tokens[comma_count].push(token.clone());
                    }
                }
                TokenType::LParen => {
                    paren_count += 1;
                    all_tokens[comma_count].push(token.clone());
                }
                TokenType::RParen => {
                    paren_count -= 1;
                    all_tokens[comma_count].push(token.clone());
                }
                _ => {
                    all_tokens[comma_count].push(token.clone());
                }
            }
            *i += 1;
            if chevron_count == 0 {
                break;
            }
        }

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            if let Ok(ast_node) = Self::ast_node_from_tokens_var_decl(&tokens, parser) {
                return_tokens.push(ast_node);
            }
        }

        (name, return_tokens)
    }

    pub fn get_scope_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> (ScopeToIdentifier, Option<Vec<Box<ASTNode>>>) { // returns (scope, type parameters) 
        let mut iterate = tokens.iter().skip(*i).peekable();
        let mut root = ScopeToIdentifier { 
            identifier: tokens[0].clone(), 
            child: None,
            scope_type: None
        };
        let mut current_scope = &mut root;
        let mut last_punc: Option<ScopeType> = None; 
        let mut first_token = true;
        let mut last_index = 0;
    
        while let Some(token) = iterate.next() {
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        parser.error("Scope has incorrect type", "Consecutive '::' found. Use '::' only between valid names, for example 'A::B::C'", &token.location);
                        return (root, None);
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        parser.error("Scope has incorrect type", "Consecutive '.' found. Use '.' only between valid names, for example 'A.B.C'", &token.location);
                        return (root, None);
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
                        parser.error("Scope has incorrect type", "Expected '::' or '.' before identifier. Use '::' and '.' to separate scope levels.", &token.location);
                        return (root, None);
                    }
    
                    current_scope.child = Some(Box::new(ScopeToIdentifier {
                        identifier: token.clone(),
                        child: None,
                        scope_type: last_punc
                    }));
                    current_scope = current_scope.child.as_mut().unwrap();
                    last_punc = None; 
                }
                _ => {
                    break;
                }
            }
            last_index += 1;
        }
    
        if last_punc.is_some() {
            parser.error("Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow '::' or '.'", &tokens.last().unwrap().location);
            return (root, None);
        }

        if tokens.get(last_index + 1).is_some_and(|t| t.token_type == TokenType::LessThan) {
            let next_tokens: Vec<Box<Token>> = tokens.iter().skip(last_index).cloned().collect();
            if let Ok(node) = Self::ast_node_from_tokens_var_decl(&next_tokens, parser) {
                if let NodeType::TypeIdentifier(ref value) = *node.node {
                    if let Some(types) = &value.types {
                        return (root, Some(types.parameters.clone()));
                    }
                }
            }

            parser.error("Error setting scope type parameters", "Type parameter for scoped type is giving errors", &tokens[last_index].location);
            return (root, None);
        }
        else {
            return (root, None);
        }
    }

    pub fn split_tokens_into_lines(tokens: &Vec<Token>) -> Vec<Vec<Box<Token>>> {
        tokens.split(|t| t.token_type == TokenType::EndOfLine)
            .map(|l| l.iter().map(|t| Box::new(t.clone())).collect::<Vec<Box<Token>>>())
            .filter(|x| !x.is_empty())
            .collect()
    }
}