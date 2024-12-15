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
                            TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                            TokenType::Abstract => access_modifier.push(AccessModifier::Abstract),
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

                    let mut __ = usize::MAX;
                    let var_value: Box<ASTNode> = Self::set_node_with_children(parser, &rhs, &mut __);

                    println!("{}", Self::print_node_expression(&var_value));

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
                    let tokens_after_return = lhs[1..].to_vec();

                    if lhs.get(0).map_or(false, |t| t.token_type == TokenType::Return) {
                        let mut __ = usize::MAX;
                        let left = Self::set_node_with_children(parser, &tokens_after_return, &mut __);
                        let mut __ = usize::MAX;
                        let right = Self::set_node_with_children(parser, &rhs, &mut __);
                        
                        let assign = ASTNode {
                            token: lhs[0].clone(), 
                            node: Box::new(NodeType::Assignment(Assignment { left, right })) 
                        };
                        ast.push(ASTNode{
                            token: lhs[0].clone(),
                            node: Box::new(NodeType::ReturnExpression(Box::new(assign)))
                        });
                    }
                    else {
                        let mut __ = usize::MAX;
                        let left = Self::set_node_with_children(parser, &lhs, &mut __);
                        let mut __ = usize::MAX;
                        let right = Self::set_node_with_children(parser, &rhs, &mut __);
                        
                        let assign = Assignment { left, right };
                        ast.push(ASTNode{
                            token: lhs[0].clone(),
                            node: Box::new(NodeType::Assignment(assign))
                        });
                    }
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
                            parser.error("Previously handled token", "Expected declaration to be found with patterns: `=`, `: IDENT <``, or `: IDENT (`", &tokens[0].location);
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
                TokenType::Return => {
                    let tokens_after_return = tokens[1..].to_vec();
                    let mut __ = usize::MAX;
                    let return_node = Self::set_node_with_children(parser, &tokens_after_return, &mut __);
                    ast.push(ASTNode {
                        token: tokens[0].clone(),
                        node: Box::new(NodeType::ReturnExpression(return_node))
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
            parser.error("Invalid token for funciton call", format!("Expected either `<` or `(` for function call, got: {}", tokens[*i + 1].value).as_str(), &tokens[*i].location);
        }
        
        let mut function_call = FunctionCall {
            parameters: NodeParameters { parameters: node_parameters },
            scope: scope.unwrap_or_else(|| ScopeToIdentifier { identifier: name, child: None, scope_type: None, as_expression: None }),
            type_parameters: type_parameters.is_empty().then(|| None).unwrap_or(Some(type_parameters)),
        };

        if tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::Dot { // is chaining
            /* This part is slightly complext and I need to write it out so I know what I'm doing

                The `function_call_node` variable has a scope that does not have a `child`, or an `as_expression` value
                when we chain, we need to update the scope of the `function_call_node` to have a `child` value to the next token after the dot
                we also need to set the `as_expression` value of the `child` to be set by `set_node_with_children`
                In the end, it should generate this structure for this example code: `a().b()`
                ```
                ASTNode {
                    token: a string
                    node: FunctionCall {
                        parameters: []
                        type_parameters: []
                        scope: ScopeToIdentifier {
                            identifier: a token
                            as_expression: None ASTNode // `as_expression` is none because `a` is the function call defined in root 
                            child: ScopeToIdentifier {
                                identifier: b token
                                as_expression: { ... } // defines the function call of `b()` using the `set_node_with_children` function. It's scope would be relatice to `a()`
                                child: None
                                scope_type: None
                            }
                        }
                    }
                }
                ```
                What about a member chain instead of a function chain, like so: `a().b`
                It would be the same, but b's `as_expression` would be an identifier instead
                The identifier's scope would have to be relarive to the chained function so there would be no infinite loop
                If there is `a().b.c`, then `a` would be the function call and then it would scope to `c` releative to `a` as `b.c`
            */
            *i += 1;
            let chained_token = tokens[*i].clone();
            let chained_expression = Self::set_node_with_children(parser, tokens, i);
            
            match *chained_expression.node.clone() {
                NodeType::FunctionCall(func) => {
                    let mut current = &mut function_call.scope;
                    while current.child.is_some() {
                        current = current.child.as_mut().unwrap();
                    }
                    current.child = Some(Box::new(func.scope));
                    current.child.as_mut().unwrap().as_expression = Some(chained_expression);
                }
                NodeType::Identifier(ident) => {
                    let mut current = &mut function_call.scope;
                    while current.child.is_some() {
                        current = current.child.as_mut().unwrap();
                    }
                    current.child = Some(Box::new(ident));
                    current.child.as_mut().unwrap().as_expression = Some(chained_expression);
                }
                _ => {
                    parser.error("Error with chained member", "Expects a member or function after preceding funciton for a valid chain: `a().b` or `a().b()`", &chained_token.location);
                    return Box::new(NodeType::None)
                }
            }

            *i -= 1;
            Box::new(NodeType::FunctionCall(function_call))
        }
        else {

            *i -= 1;
            Box::new(NodeType::FunctionCall(function_call))
        }
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
    pub fn get_ternary_from_tokens(tokens: &Vec<Box<Token>>, parser: &mut Parser, i: &mut usize) -> NodeType {
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
        let mut nesting_level = 0;
        let mut left_parenthesis_started = false;

        /*
            But then there is another problem, when it gets to the `?` and there is a parenthesis before it,
            even if the entire ternary operation is wrapped in parenthesis, it has no way of knowing that the end parenthesis
            is at the end of the expression. I need to allow both of these to be valid: `(a ? b : c ) ? d : e` and `(a ? b : c)`.
            It won't be able to tell if the first `?` is apart of nested expression, or if it is wraps the entire ternary operation.
            To solve this, we will need to check if it's wrapped when it hits the first `(` by looping and checking the nested level.
            The variable `parenthesis_wrapped` will be set it the there is no matching `(` in the condition. When it gets to the else section, 
            and `parenthesis_wrapped`, if the nesting level goes below 0, then we will not throw an error because it's the matching end `)`.
        */

        let mut parenthesis_wrapped = 0;
    
        while *i < tokens.len() {
            match tokens[*i].token_type {
                TokenType::QuestionMark => {
                    if nesting_level > 0 {
                        current_tokens.push(tokens[*i].clone());
                    } else {
                        if ternary_state != TernaryState::Condition {
                            parser.error("Unexpected `?`", "Unexpected `?` found in ternary expression: `a ? b : c`", &tokens[*i].location);
                            return NodeType::None;
                        }
                        left_parenthesis_started = false;
                        ternary_tokens.condition = std::mem::take(&mut current_tokens);
                        ternary_state = TernaryState::Then;
                    }
                }
                TokenType::Colon => {
                    if nesting_level > 0 {
                        current_tokens.push(tokens[*i].clone());
                    } else if ternary_state == TernaryState::Then {
                        ternary_tokens.then = std::mem::take(&mut current_tokens);
                        ternary_state = TernaryState::Else;
                        left_parenthesis_started = false;
                    } else {
                        parser.error("Unexpected `:`", "Unexpected `:` found in ternary expression: `a ? b : c`", &tokens[*i].location);
                        return NodeType::None;
                    }
                }
                TokenType::LParen => {
                    left_parenthesis_started = true;
                    nesting_level += 1;
                    current_tokens.push(tokens[*i].clone());
                    if ternary_state == TernaryState::Condition {
                        // we will loop through tokens until we find the matching end parenthesis or a non nested `?` 
                        // if we find a non nested `?`, then we know the entire ternary operation was wrapped in parenthesis
                        // if not, then we know the parenthesis is part of the condition
                        let mut j = i.clone() + 1;
                        let mut tracking_nesting_level = 1;
                        while j < tokens.len() {
                            match tokens[j].token_type {
                                TokenType::LParen => tracking_nesting_level += 1,
                                TokenType::RParen => {
                                    tracking_nesting_level -= 1;
                                    if tracking_nesting_level == 0 {
                                        // has found matching end parenthesis
                                        break;
                                    }
                                }
                                TokenType::QuestionMark => {
                                    if tracking_nesting_level == 1 {
                                        // found non nested `?`, so the entire ternary operation was wrapped in parenthesis
                                        nesting_level -= 1;
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
                    nesting_level -= 1;
                    if nesting_level < 0 {
                        if parenthesis_wrapped != 0 {
                            parenthesis_wrapped -= 1;
                            nesting_level += 1;
                        }
                        else {
                            parser.error("Unmatched `)`", "Found unmatched `)` in ternary expression: `a ? b : c`", &tokens[*i].location);
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
    
            if ternary_state == TernaryState::Else && nesting_level == 0 && left_parenthesis_started {
                break;
            }
    
            *i += 1;
        }
    
        if nesting_level != 0 {
            parser.error("Unmatched parentheses", "Mismatched parentheses in ternary expression.", &tokens[i.saturating_sub(1)].location);
            return NodeType::None;
        }
    
        match ternary_state {
            TernaryState::Else => ternary_tokens.else_then = current_tokens,
            _ => {
                parser.error("Incomplete ternary expression", "Expected `:` to complete the ternary expression: `a ? b : c`", &tokens[i.saturating_sub(1)].location);
                return NodeType::None;
            }
        }

        *i -= 1;

        println!("Condition: {:#?}", ternary_tokens.condition);
        println!("Then: {:#?}", ternary_tokens.then);
        println!("Else: {:#?}", ternary_tokens.else_then);
    
        let mut __ = usize::MAX;
        let condition = Self::set_node_with_children(parser, &ternary_tokens.condition, &mut __);
        let mut __ = usize::MAX;
        let then = Self::set_node_with_children(parser, &ternary_tokens.then, &mut __);
        let mut __ = usize::MAX;
        let else_then = Self::set_node_with_children(parser, &ternary_tokens.else_then, &mut __);
    
        let ternary = TernaryConditional {
            condition,
            then,
            else_then,
        };
        NodeType::TernaryOperator(ternary)
    }
    
    pub fn set_node_with_children(parser: &mut Parser, tokens: &Vec<Box<Token>>, i: &mut usize) -> Box<ASTNode> {
        let mut expr_stack: Vec<(Box<ASTNode>, usize)> = Vec::new();
        let mut op_stack: Vec<(Box<Token>, usize)> = Vec::new();
        let mut tuple_vec: Vec<Box<ASTNode>> = Vec::new();
        let mut last_was_ident = false;
        let mut is_inside_parenthesis = false;
        let mut last_was_unary_operator = false;
        let mut param_index = 0;
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
                            parser.error("Error parsing expression", "Operator stack is empty in ternary expression", &token.location);
                            return 0;
                        }));
                    }
                }
                for expr in expr_stack.clone() {
                    if expr.1 >= *i {
                        expr_stack.remove(expr_stack.iter().position(|x| x.1 == expr.1).unwrap_or_else(|| {
                            parser.error("Error parsing expression", "Expression stack is empty in ternary expression", &token.location);
                            return 0;
                        }));
                    }
                }
                let ternary = Self::get_ternary_from_tokens(&tokens, parser, i);
                expr_stack.push((Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(ternary),
                }), *i));
            }
            else if token.token_type.is_operator() {
                let mut handle_as_operator = true;

                // To check for unary operators, the operator must be the first token in the expression, or the previous token must be an operator: `-0` or `0+-1`

                if last_was_ident && token.token_type == TokenType::LessThan {
                    // part of function call: function<TYPE>()
                    expr_stack.push((Box::new(ASTNode { 
                        token: token.clone(),
                        node: Self::function_from_tokens(&tokens, parser, None, i),
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
                        let next_expression = Self::set_node_with_children(parser, &tokens.to_vec(), i);
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
                    else if tokens.get(*i - 1).is_some().then(|| tokens[*i - 1].token_type.is_operator() || tokens[*i - 1].token_type == TokenType::LParen).unwrap_or(false) {
                        last_was_unary_operator = false;

                        *i += 1;
                        let next_expression = Self::set_node_with_children(parser, &tokens.to_vec(), i);
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
                                parser.error("Error parsing expression", "Operator stack is empty", &token.location);
                                return (Box::new(Token::new_empty()), 0);
                            }).0;
                            let right = expr_stack.pop().unwrap_or_else(|| {
                                parser.error("Error parsing expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                parser.error("Error parsing expression", "This error usually occurs when 2 operators are next to eachother, `val + / val`", &token.location);
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
                        node: Self::traverse_scope_from_tokens(&tokens, parser, i),
                    }), *i));
                }
                else {
                    parser.error("Expected identifier before double colon", "Expected identifier before double colon: `IDENT::IDENT`", &token.location);
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::Dot {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push((Box::new(ASTNode { 
                        token: tokens[*i - 1].clone(),
                        node: Self::traverse_scope_from_tokens(&tokens, parser, i),
                    }), *i));
                }
                else {
                    parser.error("Expected identifier before dot", "Expected identifier before dot: `IDENT.IDENT`", &token.location);
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
                        node: Self::function_from_tokens(&tokens, parser, None, i),
                    }), *i));
                }
                else {
                    // part of expression
                    is_inside_parenthesis = true;
                    param_index += 1;
                    op_stack.push((token, *i));
                }
                last_was_ident = false;
            }
            else if token.token_type == TokenType::RParen {
                last_was_unary_operator = false;
                is_inside_parenthesis = false;
                param_index -= 1;
                while let Some(top_op) = op_stack.pop() {
                    if top_op.0.token_type == TokenType::LParen {
                        break;
                    } else {
                        let right = expr_stack.pop().unwrap_or_else(|| {
                            parser.error("Error parsing expression", "Right hand side of expression is empty", &token.location);
                            return (Box::new(ASTNode::none()), 0);
                        }).0;
                        let left = expr_stack.pop().unwrap_or_else(|| {
                            parser.error("Error parsing expression", "Left hand side of expression is empty", &token.location);
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
            else if token.token_type.is_constant() {
                last_was_unary_operator = false;
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
                    return Box::new(ASTNode::none());
                }

                expr_stack.push((node, *i));
            }
            else if token.token_type == TokenType::Identifier {
                last_was_unary_operator = false;
                if tokens.get(*i + 1).is_some() && (tokens[*i + 1].token_type == TokenType::DoubleColon || tokens[*i + 1].token_type == TokenType::Dot || tokens[*i + 1].token_type == TokenType::LParen || tokens[*i + 1].token_type == TokenType::LessThan) {
                    last_was_ident = true;
                }
                else {
                    let node = Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::Identifier(ScopeToIdentifier {
                            identifier: token.clone(),
                            child: None,
                            as_expression: None,
                            scope_type: None
                        })),
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
                                parser.error("Error parsing expression", "Right hand side of expression is empty", &token.location);
                                return (Box::new(ASTNode::none()), 0);
                            }).0;
                            let left = expr_stack.pop().unwrap_or_else(|| {
                                parser.error("Error parsing expression", "Left hand side of expression is empty", &token.location);
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
                    let tuple_node = Self::expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack, parser);
                    op_stack.clear();
                    expr_stack.clear();

                    if let Some(final_node) = tuple_node {
                        tuple_vec.push(final_node);
                    }
                    else {
                        parser.error("Failed to parse tuple", "Couldn't parse tuple", &token.location);
                        return Box::new(ASTNode::none());
                    }
                }
                else {
                    parser.error("Unexpected token in expression", "Didn't expect this token in expression. Maybe you meant to use a comma in a tuple, If that's so, put inside parentheises", &token.location);
                    return Box::new(ASTNode::none());
                }
            }
            else {
                parser.error("Unexpected token in expression", "Didn't expect this token in expression", &token.location);
                return Box::new(ASTNode::none());
            }
    
            *i += 1;
            if is_1_expression && param_index == 0 && !last_was_ident && !(tokens.get(*i).is_some() && tokens[*i].token_type == TokenType::QuestionMark) {
                break;
            }
        }
    
        let expression_node = Self::expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack, parser);
    
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

    pub fn expression_stacks_to_ast_node(op_stack: &mut Vec<(Box<Token>, usize)>, expr_stack: &mut Vec<(Box<ASTNode>, usize)>, parser: &mut Parser) -> Option<Box<ASTNode>> {
        while let Some(operator) = op_stack.pop() {
            let right = expr_stack.pop();
            let left = expr_stack.pop();
    
            let node = Box::new(ASTNode {
                token: operator.0.clone(),
                node: Box::new(NodeType::Operator(Expression {
                    left: left.unwrap_or_else(|| { 
                        parser.error("Error parsing expression", "Left hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::none()), 0)
                    }).0,
                    right: right.unwrap_or_else(|| { 
                        parser.error("Error parsing expression", "Right hand side of expression is empty", &operator.0.location);
                        (Box::new(ASTNode::none()), 0)
                    }).0,
                    operator: operator.0.clone(),
                })),
            });
    
            expr_stack.push((node, 0));
        }

        return expr_stack.pop().map(|x| x.0);
    }

    #[allow(dead_code)]
    fn print_node_expression(node: &Box<ASTNode>) -> String {
        match *node.node {
            NodeType::Operator(ref value) => {
                let left = Self::print_node_expression(&value.left);
                let right = Self::print_node_expression(&value.right);
    
                format!("({} {} {})", left, value.operator.value, right)
            }
            NodeType::UnaryOperator(ref value) => {
                let operand = Self::print_node_expression(&value.operand);
    
                format!("({}{})", value.operator.value, operand)
            }
            NodeType::FunctionCall(ref value) => {
                let mut scope = "".to_string();
                let mut s_child = Some(Box::new(value.scope.clone()));
                while let Some(child) = s_child {
                    scope += format!("{}{}", child.scope_type.clone().is_some().then(|| child.scope_type.clone().unwrap().to_string()).unwrap_or(child.is_chained().then(|| "/*CHAINED*/.".to_string()).unwrap_or("".to_string()) ), child.identifier.value.clone()).as_str();
                    s_child = child.child.clone();
                }
                let mut parameters = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    parameters += format!("{}{}", Self::print_node_expression(param).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut type_parameters = "".to_string();
                if value.type_parameters.is_some() {
                    type_parameters += "<";
                    for (index, param) in value.type_parameters.clone().unwrap().iter().enumerate() {
                        type_parameters += format!("{}{}", Self::print_node_expression(param).as_str(), value.type_parameters.clone().unwrap().get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                    }
                    type_parameters += ">";
                }
                format!("{}{}({})", scope, type_parameters, parameters)
            }
            NodeType::Identifier(ref value) => {
                let mut scope = "".to_string();
                let mut s_child = Some(Box::new(value.clone()));
                while let Some(child) = s_child {
                    scope += format!("{}{}", child.scope_type.clone().is_some().then(|| child.scope_type.clone().unwrap().to_string()).unwrap_or(child.is_chained().then(|| "/*CHAINED*/.".to_string()).unwrap_or("".to_string())), child.identifier.value.clone()).as_str();
                    s_child = child.child.clone();
                }
                scope
            }
            NodeType::TupleExpression(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::print_node_expression(param).as_str(), value.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
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
                let condition = Self::print_node_expression(&value.condition);
                let then = Self::print_node_expression(&value.then);
                let else_then = Self::print_node_expression(&value.else_then);
    
                format!("({} ? {} : {})", condition, then, else_then)
            }
            _ => {
                node.token.value.clone()
            }
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
        if all_tokens.len() == 1 && all_tokens[0].len() == 0 {
            return vec![];
        }

        let mut return_tokens: Vec<Box<ASTNode>> = Vec::new();
        for tokens in all_tokens {
            let mut __ = usize::MAX;
            return_tokens.push(Self::set_node_with_children(parser, &tokens, &mut __));
        }

        return_tokens
    }

    pub fn get_type_from_tokens(tokens: &Vec<Box<Token>>, location: &Location, parser: &mut Parser) -> Box<ASTNode> {
        if let Ok(ast_node) = Self::ast_node_from_tokens_var_decl(tokens, parser) {
            return ast_node;
        }

        parser.error("Variable declaration has no type", "Expected a type before the `:`, no type was provided", &location);
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
                            scope: ScopeToIdentifier { identifier: first_token.clone(), child: None, scope_type: None, as_expression: None },
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
                            scope: ScopeToIdentifier { identifier: type_parameters.0, child: None, scope_type: None, as_expression: None },
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
                    parser.error("Incorrect type", "Expected a tuple or type before the `::` or  `.`", &tokens.get(1).unwrap().location);
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
                parser.error("Variable declaration has incorrect type", format!("Expected tuple or type, found: `{}`", first_token.value).as_str(), &first_token.location);
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
                parser.error("Error getting node parameters", "Expected to start with `(`", &token.location);
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
        
        let mut name = tokens[*i].clone();
        if let Some(token) = tokens.get(*i) {
            name = token.clone();
        }

        if let Some(token) = tokens.get(*i + 1) {
            if token.token_type != TokenType::LessThan {
                parser.error("Error getting type parameters", "Expected to start with `<`", &token.location);
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
            identifier: tokens[*i].clone(), 
            child: None,
            scope_type: None,
            as_expression: None
        };
        let mut current_scope = &mut root;
        let mut last_punc: Option<ScopeType> = None;
        let mut first_token = true;
        let mut last_index = *i;

        while let Some(token) = iterate.next() {
            match token.token_type {
                TokenType::DoubleColon => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        parser.error("Scope has incorrect type", "Consecutive `::` found. Use `::` only between valid names, for example `A::B::C`", &token.location);
                        return (root, None);
                    }
                    last_punc = Some(ScopeType::DoubleColon);
                }
                TokenType::Dot => {
                    *i += 1;
                    if last_punc.is_some() || first_token {
                        parser.error("Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
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
                        parser.error("Scope has incorrect type", "Expected `::` or `.` before identifier. Use `::` and `.` to separate scope levels.", &token.location);
                        return (root, None);
                    }
    
                    current_scope.child = Some(Box::new(ScopeToIdentifier {
                        identifier: token.clone(),
                        child: None,
                        scope_type: last_punc,
                        as_expression: None
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
            parser.error("Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
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