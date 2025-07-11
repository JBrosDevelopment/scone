// eventually need to remove [] and instead use get
// remove all unwrapping and replace with error handling
// go over and make sure no unneciary cloning is happening

use std::vec;

use crate::lexer::{Token, TokenType, Location};
use crate::operator_tokens;

#[allow(unused_imports)]
use crate::{ast::*, error_handling::{ErrorHandling, DEBUGGING}};
#[allow(unused_imports)]
use crate::debug;

pub fn parse(tokens: Vec<Token>, code: &String, path: Option<String>) -> (Vec<ASTNode>, ErrorHandling) {
    let mut parser = Parser::new(tokens, code.clone(), path.clone());
    parser.generate_ast();
    
    parser.output.print_messages();
    (parser.ast, parser.output)
}

#[derive(Debug, Clone)]
pub struct Parser {
    output: ErrorHandling,
    ast: Vec<ASTNode>,
    lines: Vec<Vec<Box<Token>>>,
    __curent_parsing_line: usize,
    __current_tags: Vec<Tag>
}

impl Parser {
    pub fn new(tokens: Vec<Token>, full_code: String, path: Option<String>) -> Parser {
        Parser {
            output: ErrorHandling::new(path, full_code),
            ast: vec![],
            lines: Self::split_tokens_into_lines(&tokens),
            __curent_parsing_line: 0,
            __current_tags: vec![],
        }
    }

    pub fn generate_ast(&mut self) {
        self.ast = self.generate_from_tokens();

        // for testing
        for _node in self.ast.iter() {
            //Self::print_node(_node);
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
        let mut ast: Vec<ASTNode> = vec![];

        while self.__curent_parsing_line < self.lines.len() {
            let mut tokens = self.lines[self.__curent_parsing_line].clone();
            let node = self.get_ast_node(&mut tokens);
            
            //println!("{}", Self::node_expr_to_string(&node, 0));

            if node != ASTNode::err() {
                ast.push(node);
            }
            
            self.__curent_parsing_line += 1;
        }

        ast
    }

    fn get_ast_node(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        if tokens.len() == 0 {
            return ASTNode::err();
        }
        
        let mut set_i_to_zero = Parser::SET_I_TO_ZERO;
        match tokens[0].token_type {
            TokenType::For => { // this is so that `for i = 0, i < 10, i += 1` doesn't parse as a variable assignment with the `=`
                return *self.get_expression(tokens, &mut set_i_to_zero, Self::NORMAL_PARSING);
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
            TokenType::TypeDef => {
                return self.get_typedef(tokens);
            }
            _ => {}
        }

        if tokens.iter().any(|t| t.token_type == TokenType::Class || t.token_type == TokenType::Struct) {
            return self.get_class_or_struct(tokens);
        }

        if tokens.iter().any(|t| t.token_type == TokenType::Trait) {
            return self.get_trait(tokens);
        }

        // anything else
        return *self.get_expression(tokens, &mut set_i_to_zero, Self::NORMAL_PARSING);
    }

    fn get_trait(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        let tags = self.__current_tags.clone();
        self.__current_tags.clear();

        let mut access_modifier: Vec<AccessModifier> = vec![];
        let mut start_of_name = 0;
        for token in tokens.iter() {
            match token.token_type {
                TokenType::Pub => access_modifier.push(AccessModifier::Public),
                TokenType::Priv => access_modifier.push(AccessModifier::Private),
                TokenType::Override => access_modifier.push(AccessModifier::Override),
                TokenType::Virtual => access_modifier.push(AccessModifier::Virtual),
                TokenType::Static => access_modifier.push(AccessModifier::Static),
                TokenType::Const => access_modifier.push(AccessModifier::Const),
                TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                TokenType::Safe => access_modifier.push(AccessModifier::Safe),
                TokenType::Unsafe => access_modifier.push(AccessModifier::Unsafe),
                _ => break
            }
            start_of_name += 1;
        }

        let mut i = start_of_name;
        if tokens.get(i).is_some_and(|t| t.token_type != TokenType::Trait) {
            self.error(line!(), "Error parsing trait", "Expected `trait` keyword: `trait NAME {{ ... }}", &tokens[start_of_name].location);
            return ASTNode::err();
        }
        let trait_keyword = tokens[i].clone();
        
        Self::inc(&mut i);
        let name = tokens.get(i).cloned().unwrap_or_else(|| {
            self.error(line!(), "Error parsing trait", "Expected a name for the trait: `trait NAME {{ ... }}`", &tokens[start_of_name].location);
            Box::new(Token::new_empty())
        });

        Self::inc(&mut i);
        let extends = if tokens.get(i).is_some_and(|t| t.token_type == TokenType::RightArrow) {
            Self::inc(&mut i);

            let mut extends = vec![];
            let mut last_was_comma = true;
            for token in tokens.iter().skip(i) {
                if token.token_type == TokenType::LBrace {
                    break;
                }
                if !matches!(token.token_type, TokenType::Comma | TokenType::Identifier) {
                    self.error(line!(), "Error parsing traits extensions for {}", "Unexpected token while parsing traits for trait. Expects `{{`, `,`, or `IDENTIFIER`, but got `{B}`: `trait NAME -> EXTENDS, TRAITS {{ ... }}`", &tokens[start_of_name].location);
                } 
                if last_was_comma && token.token_type == TokenType::Comma {
                    self.error(line!(), "Error parsing traits extensions for {}", "Two commas in a row while parsing traits for trait: `trait NAME -> EXTENDS, TRAITS {{ ... }}`", &tokens[start_of_name].location);
                }
                if !last_was_comma && token.token_type != TokenType::Comma {
                    self.error(line!(), "Error parsing traits extensions for {}", "Missing a comma while parsing traits for trait: `trait NAME -> EXTENDS, TRAITS {{ ... }}`", &tokens[start_of_name].location);
                }

                if last_was_comma && token.token_type == TokenType::Identifier {
                    extends.push(token.clone());
                    last_was_comma = false;
                }
                if !last_was_comma && token.token_type == TokenType::Comma {
                    last_was_comma = true;
                } 
            }
            extends
        } else if tokens.get(i).is_some_and(|t| t.token_type == TokenType::LBrace) {
            vec![]
        } else {
            self.error(line!(), "Error parsing traits extensions", "Expected either `->` or `{` after trait name: `trait NAME -> EXTENDS {...}` or `trait NAME {...}`", tokens.get(i).map_or(&tokens[start_of_name].location, |t| &t.location));
            vec![]
        };

        Self::inc(&mut i);
        let body = self.get_code_block(tokens, &mut i, false);
        
        for thing in body.body.clone() {
            if let NodeType::FunctionDeclaration(_) = thing.node.as_ref() {
            }
            else if let NodeType::VariableDeclaration(ref v) = thing.node.as_ref() {
                if v.var_value.is_some() {
                    self.error(line!(), "Error while parsing {} body", "Expected only a function or variable declaration for trait: `trait NAME {{ TYPE: NAME = VALUEl; TYPE: NAME() {{ ... }} }}`", &thing.token.location);
                    return ASTNode::err();
                }
            } 
            else {
                self.error(line!(), "Error while parsing {} body", "Expected only a function or variable declaration for trait: `trait NAME {{ TYPE: NAME = VALUEl; TYPE: NAME() {{ ... }} }}`", &thing.token.location);
                return ASTNode::err();
            }
        }

        return ASTNode {
            node: Box::new(NodeType::TraitDeclaration(TraitDeclaration {
                access_modifier,
                name,
                extends,
                body,
                tags
            })),
            token: trait_keyword.clone(),
        };
    }

    fn get_class_or_struct(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        let tags = self.__current_tags.clone();
        self.__current_tags.clear();

        let mut access_modifier: Vec<AccessModifier> = vec![];
        let mut start_of_name = 0;
        for token in tokens.iter() {
            match token.token_type {
                TokenType::Pub => access_modifier.push(AccessModifier::Public),
                TokenType::Priv => access_modifier.push(AccessModifier::Private),
                TokenType::Override => access_modifier.push(AccessModifier::Override),
                TokenType::Virtual => access_modifier.push(AccessModifier::Virtual),
                TokenType::Static => access_modifier.push(AccessModifier::Static),
                TokenType::Const => access_modifier.push(AccessModifier::Const),
                TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                TokenType::Safe => access_modifier.push(AccessModifier::Safe),
                TokenType::Unsafe => access_modifier.push(AccessModifier::Unsafe),
                _ => break
            }
            start_of_name += 1;
        }

        
        let mut i = start_of_name;
        let class_or_struct_token = tokens.get(i).cloned().unwrap_or_else(|| {
            self.error(line!(), "Error parsing class or struct", "Expected either `class` or `struct`", &tokens[start_of_name].location);
            Box::new(Token::new_empty())
        });
        let class_or_struct = if class_or_struct_token.token_type == TokenType::Class { "class" } else { "struct" };
        
        Self::inc(&mut i);
        let name = tokens.get(i).cloned().unwrap_or_else(|| {
            self.error(line!(), format!("Error parsing {}", class_or_struct).as_str(), format!("Expected a name for the {A}: `{A} NAME {{ ... }}`", A = class_or_struct).as_str(), &tokens[start_of_name].location);
            Box::new(Token::new_empty())
        });
        
        Self::inc(&mut i);
        let type_parameters: Option<AnonymousTypeParameters> = if tokens.get(i).is_some_and(|t| t.token_type == TokenType::LessThan) { // has type parameters
            Some(self.get_anonymous_type_parameters(tokens, &mut i))
        } else {
            None
        };

        let extends = if tokens.get(i).is_some_and(|t| t.token_type == TokenType::RightArrow) {
            Self::inc(&mut i);

            let mut extends = vec![];
            let mut last_was_comma = true;
            for token in tokens.iter().skip(i) {
                if token.token_type == TokenType::LBrace {
                    break;
                }
                if !matches!(token.token_type, TokenType::Comma | TokenType::Identifier) {
                    self.error(line!(), format!("Error parsing traits extensions for {}", class_or_struct).as_str(), format!("Unexpected token while parsing traits for {A}. Expects `{{`, `,`, or `IDENTIFIER`, but got `{B}`: `{A} NAME -> EXTENDS, TRAITS {{ ... }}`", A = class_or_struct, B = token.value).as_str(), &tokens[start_of_name].location);
                } 
                if last_was_comma && token.token_type == TokenType::Comma {
                    self.error(line!(), format!("Error parsing traits extensions for {}", class_or_struct).as_str(), format!("Two commas in a row while parsing traits for {A}: `{A} NAME -> EXTENDS, TRAITS {{ ... }}`", A = class_or_struct).as_str(), &tokens[start_of_name].location);
                }
                if !last_was_comma && token.token_type != TokenType::Comma {
                    self.error(line!(), format!("Error parsing traits extensions for {}", class_or_struct).as_str(), format!("Missing a comma while parsing traits for {A}: `{A} NAME -> EXTENDS, TRAITS {{ ... }}`", A = class_or_struct).as_str(), &tokens[start_of_name].location);
                }

                if last_was_comma && token.token_type == TokenType::Identifier {
                    extends.push(token.clone());
                    last_was_comma = false;
                }
                if !last_was_comma && token.token_type == TokenType::Comma {
                    last_was_comma = true;
                } 
            }
            extends
        } else {
            vec![]
        };

        Self::inc(&mut i);
        let body = self.get_code_block(tokens, &mut i, false);
        
        for thing in body.body.clone() {
            if let NodeType::FunctionDeclaration(_) = thing.node.as_ref() {
            }
            else if let NodeType::VariableDeclaration(_) = thing.node.as_ref() {
            } 
            else {
                self.error(line!(), format!("Error while parsing {} body", class_or_struct).as_str(), format!("Expected only a function or variable declaration for {A}, but got a `{B}`: `{A} NAME {{ TYPE: NAME = VALUEl; TYPE: NAME() {{ ... }} }}`", A = class_or_struct, B = thing.node.to_string()).as_str(), &thing.token.location);
            }
        }

        if class_or_struct_token.token_type == TokenType::Class {
            return ASTNode {
                node: Box::new(NodeType::ClassDeclaration(ClassDeclaration {
                    access_modifier,
                    name,
                    type_parameters,
                    extends,
                    body,
                    tags
                })),
                token: class_or_struct_token.clone(),
            };
        } else {
            return ASTNode {
                node: Box::new(NodeType::StructDeclaration(StructDeclaration {
                    access_modifier,
                    name,
                    type_parameters,
                    extends,
                    body,
                    tags
                })),
                token: class_or_struct_token.clone(),
            };
        }
    }

    fn get_typedef(&mut self, tokens: &mut Vec<Box<Token>>) -> ASTNode {
        if tokens.len() < 4 {
            self.error(line!(), "Error parsing `typedef` statement", "`typedef` statement must have two arguments: `typedef TYPE: NAME`", &tokens[0].location);
            return ASTNode::err();
        }

        let mut i = 1;
        let mut tokens_until_colon = tokens.iter().take_while(|t| t.token_type != TokenType::Colon).cloned().collect();
        let type_definition  = self.get_type_idententifier(&mut tokens_until_colon, &mut i, false);

        i = tokens.iter().position(|t| t.token_type == TokenType::Colon).unwrap_or_else(|| {
            self.error(line!(), "Error parsing `typedef` statement", "Expected `typedef` statement to have a colon: `typedef TYPE: NAME`", &tokens[0].location);
            return tokens.len() - 2;
        }) + 1;
        let name = tokens[i].clone();

        if tokens.len() != i + 1 {
            self.error(line!(), "Error parsing `typedef` statement", "Incorrect syntax for `typedef` statement: `typedef TYPE: NAME`", &tokens[0].location);
            return ASTNode::err();
        }

        ASTNode {
            token: tokens[0].clone(),
            node: Box::new(NodeType::TypeDef(TypeDefDeclaration { 
                name,
                type_definition 
            })),
        }
    }

    fn declaring_variable(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, is_in_for: bool, assigning: bool) -> ASTNode { // `tokens` fpr tokens to parse, `i` for current token index only used if `is_in_for` is true, `is_in_for` whether or not we are in a for loop
        let tags = self.__current_tags.clone();
        self.__current_tags.clear();

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

        let assign_index = tokens.iter().position(|t| t.token_type == TokenType::Assign).map_or(if assigning { -1 } else { tokens.len() as i32 }, |index| index as i32);

        if assign_index != -1 {
            let lhs = tokens[starting_index..assign_index as usize].to_vec();
            
            if !is_in_for {
                *i = assign_index as usize + 1; // skip `=`
            } 

            if lhs.len() == 1 && lhs[0].token_type == TokenType::Underscore {
                // is an underscore assignment,
                return *self.get_expression(tokens, i, Self::NORMAL_PARSING);
            } else if (lhs.len() >= 2 && lhs[lhs.len() - 2].token_type == TokenType::Colon && assigning) || !assigning {
                // declaring variable
                let var_name: Box<Token> = lhs[lhs.len() - 1].clone();

                let mut accessing_end_index = 0;
                let mut access_modifier: Vec<AccessModifier> = vec![];
                for token in lhs.iter() {
                    match token.token_type {
                        TokenType::Pub => access_modifier.push(AccessModifier::Public),
                        TokenType::Priv => access_modifier.push(AccessModifier::Private),
                        TokenType::Override => access_modifier.push(AccessModifier::Override),
                        TokenType::Virtual => access_modifier.push(AccessModifier::Virtual),
                        TokenType::Static => access_modifier.push(AccessModifier::Static),
                        TokenType::Const => access_modifier.push(AccessModifier::Const),
                        TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                        TokenType::Safe => access_modifier.push(AccessModifier::Safe),
                        TokenType::Unsafe => access_modifier.push(AccessModifier::Unsafe),
                        _ => break
                    }
                    accessing_end_index += 1;
                }

                let mut type_tokens = lhs[accessing_end_index..lhs.len() - 2].to_vec();
                let var_type: Box<ASTNode> = self.get_type_idententifier(&mut type_tokens, &mut 0, false);

                let var_value;
                if assigning {
                    let var = if is_in_for {
                        self.get_expression(tokens, i, Self::PARSING_FOR_FOR_LOOP_STATEMENT)
                    } else {
                        self.get_expression(tokens, i, Self::NORMAL_PARSING)
                        // let mut j = 0;
                        // let expression = self.get_expression(&mut rhs, &mut j, Self::NORMAL_PARSING);
                        // *i += j; 
                        // expression
                    };
                    var_value = Some(var);
                } else {
                    var_value = None;
                }

                let var_decl = VariableDeclaration {
                    access_modifier,
                    var_name: var_name.clone(),
                    var_value,
                    var_type,
                    tags,
                };
                self.__current_tags.clear(); 

                return ASTNode {
                    token: var_name.clone(),
                    node: Box::new(NodeType::VariableDeclaration(var_decl)),
                };
            }
        }
        return ASTNode::err();
    }

    fn declaring_function(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode {
        let tags = self.__current_tags.clone();
        self.__current_tags.clear();

        Self::dec(i); // move `i` to `:`
        let before_colon: Vec<Box<Token>> = tokens.iter().take(*i).map(|t| t.clone()).collect(); // get tokens before `:`
        
        Self::inc(i);
        let name: Box<Token> = tokens[*i].clone();

        let mut access_modifier: Vec<AccessModifier> = vec![];
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
                TokenType::Safe => access_modifier.push(AccessModifier::Safe),
                TokenType::Unsafe => access_modifier.push(AccessModifier::Unsafe),
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
        if parameters.len() == 0 {
            Self::inc(i);
        }
        
        let body = if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LBrace) {
            let last_is_return = return_type.token.value != "void";
            Some(self.get_code_block(tokens, i, last_is_return))
        } else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::DoubleArrow) {
            Self::inc(i);
            let mut expression = self.get_expression(tokens, i, Self::NORMAL_PARSING);

            if let NodeType::ReturnExpression(_) = expression.node.as_ref() {
            } else {
                let old = expression.clone();
                if *old == ASTNode::err() {
                    expression = Box::new(ASTNode {
                        token: old.token.clone(),
                        node: Box::new(NodeType::ReturnExpression(None))
                    });
                } else {
                    expression = Box::new(ASTNode {
                        token: old.token.clone(),
                        node: Box::new(NodeType::ReturnExpression(Some(old)))
                    });
                }
            }
            Some(BodyRegion {
                body: vec![expression],
            })
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
            tags,
        };
        self.__current_tags.clear(); 

        return ASTNode {
            token: name.clone(),
            node: Box::new(NodeType::FunctionDeclaration(function)),
        };
    }

    fn get_enum(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode { 
        let node_token = tokens[*i].clone();

        let mut access_modifier: Vec<AccessModifier> = vec![];
        for token in tokens.iter() {
            match token.token_type {
                TokenType::Pub => access_modifier.push(AccessModifier::Public),
                TokenType::Priv => access_modifier.push(AccessModifier::Private),
                TokenType::Override => access_modifier.push(AccessModifier::Override),
                TokenType::Virtual => access_modifier.push(AccessModifier::Virtual),
                TokenType::Static => access_modifier.push(AccessModifier::Static),
                TokenType::Const => access_modifier.push(AccessModifier::Const),
                TokenType::Extern => access_modifier.push(AccessModifier::Extern),
                TokenType::Safe => access_modifier.push(AccessModifier::Safe),
                TokenType::Unsafe => access_modifier.push(AccessModifier::Unsafe),
                _ => break
            }
        }

        let name = tokens.get(*i + 1).cloned().unwrap_or_else(|| {
            self.error(line!(), "Expected enum name", "Parser error, expected enum name: `enum NAME { ... }`", &tokens[*i].location);
            Box::new(Token::new_empty())
        }).clone();

        Self::inc(i);
        Self::inc(i);
        if !tokens.get(*i).is_some_and(|t| t.token_type == TokenType::LBrace) {
            self.error(line!(), "Error parsing enum, Expected `{`", "Parser error, expected `{` in enum declaration: `enum NAME { ... }`", &node_token.location);
            return ASTNode::err();
        }
        
        self.__curent_parsing_line += 1;
        if self.__curent_parsing_line >= self.lines.len() {
            self.error(line!(), "Error parsing enum, Expected closing delimeter", "Parser error, expected closing `}` in enum declaration but found end of file: `enum NAME { ... }`", &node_token.location);
            return ASTNode::err();
        }
        *tokens = self.lines[self.__curent_parsing_line].clone(); 
        *i = 0;

        let mut body: Vec<(Box<Token>, Option<Box<ASTNode>>)> = vec![];
        
        let mut expessions = self.get_node_parameters(tokens, i, Self::PARSING_FOR_ENUM);
        if expessions.parameters.last().is_some_and(|t| **t == ASTNode::err()) {
            expessions.parameters.pop();
        }

        for expession in expessions.parameters {
            if let NodeType::Identifier(ref name) = *expession.node {
                body.push((name.clone(), None));
            } else if let NodeType::Operator(ref value) = *expession.node {
                if value.operator.token_type == TokenType::Assign {
                    if let NodeType::Identifier(ref name) = *value.left.node {
                        body.push((name.clone(), Some(value.right.clone())));
                    } else {
                        self.error(line!(), "Error parsing enum, Expected identifier", format!("Expected identifier in enum declaration but found `{}`: `enum NAME {{ ... }}`", value.left.token.value).as_str(), &value.left.token.location);
                    }
                } else {
                    self.error(line!(), "Error parsing enum, Expected `=`", format!("Expected `=` operator in enum declaration but found `{}`: `enum NAME {{ THING = 0, ... }}`", value.operator.value).as_str(), &value.operator.location);
                }
            } else {
                self.error(line!(), "Error parsing enum, invalid syntax", "Expected identifier with or without assigned valie in enum declaration: `enum NAME { THING1, THING2 = 1, ... }", &node_token.location);
            }
        }

        let tags = self.__current_tags.clone();
        self.__current_tags.clear();

        ASTNode { 
            token: node_token, 
            node:  Box::new(NodeType::EnumDeclaration(EnumDeclaration {
                access_modifier,
                name,
                body,
                tags
            }))
        }
    }

    fn parse_match_case(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> MatchCase {
        MatchCase { 
            pattern: self.get_expression(tokens, i, Self::PARSING_FOR_MATCH_CASE), 
            body: self.get_expression(tokens, i, Self::PARSING_FOR_MATCH_STATEMENT)
        }
    }

    fn parse_match(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> ASTNode {
        if tokens[*i].token_type != TokenType::Match {
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

                    if tokens.get(*i).is_some_and(|t| t.token_type == TokenType::RBrace) {
                        // ends
                        done = true;
                        break;
                    }

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

        if segments.parameters.len() == 0 {
            self.error(line!(), "Could not parse for statement", "No parameters were found for the for statement: `for X in Y { ... }` or `for VAR, CONDITION, INC { ... }`", &for_token.location);
            return ASTNode::err();
        } else if segments.parameters.len() == 1 || segments.parameters.len() == 2 {
            // for X in Y
            let mut index_segment = None;
            if segments.parameters.len() == 2 {
                if let NodeType::Identifier(ref v) = segments.parameters[0].node.as_ref() {
                    index_segment = Some(v.clone());
                } else {
                    self.error(line!(), "Unable to parse `for` statement, incorrect indexer expression", "Expected a single identifier token as the indexer for the `for` statement: `for I, X in Y {}`", &for_token.location);
                }
            }

            let iter_value;
            let iter_range;
            if let NodeType::Operator(ref in_expression) = segments.parameters[if segments.parameters.len() == 1 {0} else {1}].node.as_ref() {
                iter_value = in_expression.left.clone();
                iter_range = in_expression.right.clone();
                if let NodeType::Identifier(_) = in_expression.left.node.as_ref() { 
                } else if let NodeType::Discard(_) = in_expression.left.node.as_ref() { 
                } else if let NodeType::TupleExpression(_) = in_expression.left.node.as_ref() {
                } else {
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
        } else if segments.parameters.len() == 3 || segments.parameters.len() == 4 {
            let mut index_segment = None;
            if segments.parameters.len() == 4 {
                if let NodeType::Identifier(ref v) = segments.parameters[0].node.as_ref() {
                    index_segment = Some(v.clone());
                } else {
                    self.error(line!(), "Unable to parse `for` statement, incorrect indexer expression", "Expected a single identifier token as the indexer for the `for` statement: `for INDEX, SET, CONDITION, INCREMENT {}`", &for_token.location);
                }
            }
            let start = if segments.parameters.len() == 3 {0} else {1};

            let set_segment;
            let condition_segment;
            let increment_segment;
            if let NodeType::VariableDeclaration(_) = segments.parameters[start].node.as_ref() {
                set_segment = segments.parameters[start].clone();
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect set expression", "Expected a variable declaration for the set segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                return ASTNode::err();
            }
            if let NodeType::Operator(ref v) = segments.parameters[start + 1].node.as_ref() {
                if !v.operator.token_type.operator_boolean() {
                    self.error(line!(), "Unable to parse `for` statement, incorrect condition expression", "Expected a valid expression for the condition segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                    return ASTNode::err();
                }
                condition_segment = segments.parameters[start + 1].clone();
            } else {
                self.error(line!(), "Unable to parse `for` statement, incorrect condition expression", "Expected a valid expression for the condition segment for the `for` statement: `for i32: i = 0, i < 10, i += 1 {}`", &for_token.location);
                return ASTNode::err();
            }
            if let NodeType::Operator(ref v) = segments.parameters[start + 2].node.as_ref() {
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

    fn get_condition_and_body_for_if(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> (Box<ASTNode>, BodyRegion) {
        if tokens[*i].token_type != TokenType::If && tokens[*i].token_type != TokenType::While { 
            self.error(line!(), "Expected `if` or `while`", "Parser error, expected `if` or `while`", &tokens[*i].location);
        } else {
            Self::inc(i); // skip if
        }

        let condition = self.get_expression(tokens, i, Self::PARSING_FOR_STATEMENT);
        Self::dec(i);
        let body = self.get_code_block(tokens, i, false);
        
        if tokens.get(*i).is_some_and(|t| t.token_type == TokenType::RBrace) && tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::Else) {
            Self::inc(i);
        }
        
        return (condition, body);
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

        let mut node_parameters: NodeParameters = NodeParameters { parameters: vec![], token: tokens.clone().get(*i + 1).unwrap_or(&tokens[*i]).clone() };
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
                if tokens.get(*i + 1).is_some() && tokens[*i + 1].token_type == TokenType::RParen {
                    Self::inc(i);
                    Self::inc(i);
                } else {
                    node_parameters = self.get_node_parameters(tokens, i, Self::PARSING_FOR_FUNCTION);
                }
            }
            else {
                self.error(line!(), "Invalid token for funciton call", format!("Expected either `<` or `(` for function call, got: `{}`", next_token.value).as_str(), &tokens[*i].location);
            }
        }

        let function_call = FunctionCall {
            parameters: node_parameters,
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

        if tokens.get(*i).is_some() && tokens[*i].token_type.is_arrow_or_dot() { // is chaining
            let scope_type  = tokens[*i].token_type.scope_type();
            /*  I've changed the way function chaining works:
                Now I will store the scope as a vector of identifiers, where the first one is the root and every index is apart of the scope or chain.
                The new `Identifier` struct will have an optional `expression` field, which will be the expression that is chained to the identifier.
                This will allow for a more dynamic way of chaining, where the scope can be changed at any point in the chain.
            */
            Self::inc(i);

            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);

            scope.scope.push(Identifier {
                expression: chained_expression.clone(),
                type_parameters: None,
                scope_type,
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
        let mut scope = ScopedIdentifier { scope: vec![] };
        let valid_lt_as_type_parameter = self.get_ident_scope(tokens, i, &mut scope, &mut last_punc); 
        Self::inc(i);

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) && valid_lt_as_type_parameter {
            let gt_pos = tokens.iter().skip(*i).rposition(|t| t.token_type == TokenType::GreaterThan);
            let lp_pos = tokens.iter().skip(gt_pos.unwrap() + *i).rposition(|t| t.token_type == TokenType::LParen);
            let lb_pos = tokens.iter().skip(*i).rposition(|t| t.token_type == TokenType::LBrace);
            if lb_pos.is_none() {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // if no `{` -> function call
            } else if lp_pos.is_none() {
                scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc) // if no `(` -> object instantiation
            } else if lp_pos.unwrap() < lb_pos.unwrap() {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // if `(` is before `{` -> function call
            } else if lp_pos.unwrap() > lb_pos.unwrap() {
                scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc) // if `{` is before `(` -> object instantiation
            } else {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // assume function call if unsure
            }
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

    fn scope_call_with_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, mut scope: ScopedIdentifier, mut last_punc: Option<ScopeType>, is_in_statement: bool) -> ScopedIdentifier {
        //Self::dec(i);

        let valid_lt_as_type_parameter = self.get_ident_scope(tokens, i, &mut scope, &mut last_punc); 
        Self::inc(i);

        if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LParen) {
            scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement)
        }
        else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::LessThan) && valid_lt_as_type_parameter {
            let gt_pos = tokens.iter().skip(*i).rposition(|t| t.token_type == TokenType::GreaterThan);
            let lp_pos = tokens.iter().skip(gt_pos.unwrap() + *i).rposition(|t| t.token_type == TokenType::LParen);
            let lb_pos = tokens.iter().skip(*i).rposition(|t| t.token_type == TokenType::LBrace);
            if lb_pos.is_none() {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // if no `{` -> function call
            } else if lp_pos.is_none() {
                scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc) // if no `(` -> object instantiation
            } else if lp_pos.unwrap() < lb_pos.unwrap() {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // if `(` is before `{` -> function call
            } else if lp_pos.unwrap() > lb_pos.unwrap() {
                scope = self.get_object_instantiation(tokens, Some(scope), i, last_punc) // if `{` is before `(` -> object instantiation
            } else {
                scope = self.function_call(tokens, Some(scope), i, last_punc, is_in_statement) // assume function call if unsure
            }
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
        if tokens.get(*i).map_or(false, |t| t.token_type.is_arrow_or_dot()) {
            let scope_type = tokens[*i].token_type.scope_type();
            Self::inc(i);
            Self::inc(i);
            let call = self.scope_call(tokens, i, is_in_statement);
            let mut push = vec![];
            for (i, c) in call.scope.iter().enumerate() {
                if i == 0 {
                    push.push(Identifier {
                        expression: c.expression.clone(),
                        type_parameters: None,
                        scope_type: scope_type.clone(),
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
                    node: Box::new(NodeType::ArrayExpression(array_nodes)),
                }),
                scope_type: None,
                type_parameters: None
            }],
        };

        // Handle chaining if the next token is a dot
        if tokens.get(*i).map_or(false, |t| t.token_type.is_arrow_or_dot()) {
            let scope_type = tokens[*i].token_type.scope_type();
            Self::inc(i);
            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);
            scope.scope.push(Identifier {
                expression: chained_expression,
                type_parameters: None,
                scope_type,
            });
        }

        scope
    }

    fn get_object_instantiation(&mut self, tokens: &mut Vec<Box<Token>>, scope: Option<ScopedIdentifier>, i: &mut usize, last_punc: Option<ScopeType>) -> ScopedIdentifier {
        let name_token = tokens.get(*i - 1).unwrap_or(&self.try_get_last_token(tokens)).clone();
        let type_parameters;
        if tokens.get(*i).is_some_and(|t| t.token_type == TokenType::LessThan) {
            type_parameters = Some(self.get_type_parameters(tokens, i));
        } else {
            type_parameters = None;
        }
        let scope_is_empty = scope.is_none(); // because if not, it's coming from scope_call_with_scope and there is another `Parser::dec(i)` in there that will cause issues
        
        if self.__curent_parsing_line >= self.lines.len() {
            self.error(line!(), "Couldn't parse object instantiation", "Expected closing brace for object instantiation: `NAME { PROPERTY = VALUE, }`", &tokens[*i].location);
        }
        self.__curent_parsing_line += 1;
        *tokens = self.lines[self.__curent_parsing_line].clone();
        *i = 0;

        let raw_properties = self.get_node_parameters(tokens, i, Self::PARSING_FOR_OBJECT_INSTANTIATION);

        let mut properties = vec![];
        for p in raw_properties.parameters {
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
            object_type: name_token.clone(),
            type_parameters
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

        if tokens.get(*i).map_or(false, |t| t.token_type.is_arrow_or_dot()) || self.lines.get(self.__curent_parsing_line + 1).map_or(false, |l| l.get(0).map_or(false, |t| t.token_type.is_arrow_or_dot())) {
            let scope_type;
            let next_lines_token_is_chain = self.lines.get(self.__curent_parsing_line + 1).map_or(None, |l| l.get(0).map_or(None, |t| Some(t.token_type.clone())));
            if next_lines_token_is_chain.is_some() && next_lines_token_is_chain.clone().unwrap().is_arrow_or_dot() {
                scope_type = next_lines_token_is_chain.unwrap().scope_type();
                self.__curent_parsing_line += 1;
                *tokens = self.lines[self.__curent_parsing_line].clone();
                *i = 1;
            } else {
                scope_type = tokens[*i].token_type.scope_type();
                Self::inc(i);
            }

            let chained_expression = self.get_expression(tokens, i, Self::PARSING_FOR_SCOPING);
            scope.scope.push(Identifier {
                expression: chained_expression,
                type_parameters: None,
                scope_type,
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
        let is_tuple = nodes.parameters.len() > 1;

        let tuple = Box::new(ASTNode {
            token: first_token,
            node: Box::new(NodeType::TupleExpression(nodes)),
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

        let mut body = self.get_node_parameters(tokens, i, Self::PARSING_FOR_CODE_BLOCK).parameters;

        if last_is_return {
            if let Some(last) = body.last_mut() {
                if let NodeType::ReturnExpression(_) = last.node.as_ref() {
                    // nothing
                } else {
                    last.node = Box::new(NodeType::ReturnExpression(Some(Box::new(ASTNode {
                        token: last.token.clone(),
                        node: last.as_ref().node.clone(),
                    }))));
                }
            }
        }
        BodyRegion {
            body
        }
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
    const PARSING_FOR_SHEBANG: u8 = 11;
    const PARSING_FOR_SHEBANG_UNTIL: [TokenType; 1] = [TokenType::RightArrow];
    const PARSING_FOR_TYPE_CONSTRAINT: u8 = 12;
    const PARSING_FOR_TYPE_CONSTRAINT_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::GreaterThan];
    const PARSING_FOR_ENUM: u8 = 13;
    const PARSING_FOR_ENUM_UNTIL: [TokenType; 2] = [TokenType::Comma, TokenType::RBrace];

    fn get_expression(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, until: u8) -> Box<ASTNode> {
        let mut expr_stack: Vec<Box<ASTNode>> = vec![];
        let mut op_stack: Vec<Box<Token>> = vec![];
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
                } else if Self::PARSING_FOR_SCOPING == until && !last_was_ident && matches!(&token.token_type, operator_tokens!() | TokenType::QuestionMark | TokenType::LBrace) {
                    break;
                } else if Self::PARSING_FOR_FOR_LOOP_STATEMENT == until && Self::PARSING_FOR_FOR_LOOP_STATEMENT_UNTIL.iter().any(|t| t == &token.token_type) {
                    break;
                } else if Self::PARSING_FOR_MATCH_CASE == until && Self::PARSING_FOR_MATCH_CASE_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::inc(i);
                    break;
                } else if Self::PARSING_FOR_SHEBANG == until && Self::PARSING_FOR_SHEBANG_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::inc(i);
                    break;
                } else if Self::PARSING_FOR_TYPE_CONSTRAINT == until && Self::PARSING_FOR_TYPE_CONSTRAINT_UNTIL.iter().any(|t| t == &token.token_type) {
                    Self::dec(i);
                    break;
                } else if Self::PARSING_FOR_ENUM == until && Self::PARSING_FOR_ENUM_UNTIL.iter().any(|t| t == &token.token_type) {
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
                    let variable = self.declaring_variable(tokens, i, is_in_for, true);
                    if variable != ASTNode::err() {
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(variable));
                    } else {
                        self.error(line!(), "Variable declaration error", "Unable to parse variable declaration expression", &token.location);
                    }
                    // if !is_in_for {
                    //     if self.__curent_parsing_line >= self.lines.len() {
                    //         break;
                    //     }
                    //     self.__curent_parsing_line += 1;
                    //     *tokens = self.lines[self.__curent_parsing_line].clone();
                    //     *i = Parser::SET_I_TO_ZERO;
                    // }
                    break;
                } else if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get(*i + 1).is_none() {
                    // it's a variable declaration with no assignment
                    
                    // remove any errors that occured before the variable declaration.
                    self.output.messages.retain(|x| x.location.line != token.location.line);

                    // parse the variable
                    Self::inc(i); // is on `=`
                    Self::inc(i); // is on first token of expression
                    let variable = self.declaring_variable(tokens, i, false, false);
                    if variable != ASTNode::err() {
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(variable));
                    } else {
                        self.error(line!(), "Variable declaration error", "Unable to parse variable declaration expression", &token.location);
                    }
                    break;
                } else {
                    Self::dec(i);
                    break;
                }
            } else if token.token_type.is_constant() {
                if last_was_ident {
                    self.error(line!(), "Unexpected constant", "Did not expect constant token after identifier token", &token.location);
                    return Box::new(ASTNode::err());
                } else if *i > 0 && tokens.get(*i - 1).is_some_and(|t| t.token_type.is_constant() || matches!(t.token_type, TokenType::Identifier | TokenType::RightArrow | TokenType::Dot | TokenType::RBracket | TokenType::RParen | TokenType::RBrace)) {
                    self.error(line!(), "Unexpected constant", "Did not expect constant token here", &token.location);
                    return Box::new(ASTNode::err());
                }

                last_was_unary_operator = false;
                let mut node = Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::None),
                });

                let constant_type = self.get_constant(&token);
                node.node = Box::new(NodeType::Constant(ConstantNode {
                    value: token.clone(),
                    constant_type: constant_type.clone(),
                }));

                if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::Dot) {
                    last_was_ident = true;
                } else {
                    expr_stack.push(node);
                }
            } else if token.token_type == TokenType::Identifier {
                if last_was_ident {
                    self.error(line!(), "Unexpected identifier", "Did not expect this identifier token after preceding identifier token", &token.location);
                    return Box::new(ASTNode::err());
                } else if *i > 0 && tokens.get(*i - 1).is_some_and(|t| t.token_type.is_constant() || matches!(t.token_type, TokenType::RBracket | TokenType::RParen | TokenType::RBrace)) {
                    self.error(line!(), "Unexpected identifier", "Did not expect identifier token here", &token.location);
                    return Box::new(ASTNode::err());
                }
                last_was_unary_operator = false;
                if tokens.get(*i + 1).is_some() && (tokens[*i + 1].token_type == TokenType::DoubleColon || (tokens[*i + 1].token_type == TokenType::LBrace && (until != Self::PARSING_FOR_STATEMENT && until != Self::PARSING_FOR_SCOPING)) || tokens[*i + 1].token_type.is_arrow_or_dot() || tokens[*i + 1].token_type == TokenType::LParen || tokens[*i + 1].token_type == TokenType::LessThan) && !(until != 0 && (until == Self::PARSING_FOR_FUNCTION && Self::PARSING_FOR_FUNCTION_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type) || until == Self::PARSING_FOR_INSIDE_PARENTHESIS && Self::PARSING_FOR_INSIDE_PARENTHESIS_UNTIL.iter().any(|x| x == &tokens[*i + 1].token_type))) {
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
                                        t.token_type == TokenType::LBrace || 
                                        t.token_type == TokenType::DoubleColon || 
                                        t.token_type == TokenType::Dot || 
                                        t.token_type == TokenType::RightArrow || 
                                        { // is apart of variable declaration.
                                            let position_of_colon = tokens.iter().position(|a| a.token_type == TokenType::Colon);
                                            if let Some(pos) = position_of_colon {
                                                pos > *i && 
                                                tokens.get(pos + 2).map_or(false, |a| a.token_type == TokenType::Assign) ||
                                                (tokens.get(pos + 1).map_or(false, |a| a.token_type == TokenType::Identifier && tokens.get(pos + 2).map_or(false, |b| matches!(b.token_type, TokenType::LParen | TokenType::LessThan))))
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
                if !matches!(until, Self::PARSING_FOR_STATEMENT | Self::PARSING_FOR_OBJECT_INSTANTIATION | Self::PARSING_FOR_ENUM) {
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
                        
                        expr_stack.clear();
                        op_stack.clear();
                        expr_stack.push(Box::new(ASTNode{
                            token: token.clone(),
                            node: Box::new(NodeType::Assignment(assign))
                        }));
                        if self.__curent_parsing_line >= self.lines.len() {
                            break;
                        }
                        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                        // self.__curent_parsing_line += 1;
                        // *tokens = self.lines[self.__curent_parsing_line].clone();
                        // *i = Parser::SET_I_TO_ZERO;
                        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                        break;
                    }
                }
                // check if it is actually part of a variable declaration
                if last_was_ident {
                    let assign_position = tokens.iter().rposition(|t| t.token_type == TokenType::Assign);
                    if let Some(position) = assign_position {
                        if position > 2 && position > *i && tokens.get(position - 1).map_or(false, |t| t.token_type == TokenType::Identifier) && tokens.get(position - 2).map_or(false, |t| t.token_type == TokenType::Colon) {
                            // part of variable declaration: TYPE: NAME = VALUE
                            *i = position - 2;
                            continue;
                        }
                    } else {
                        let colon_position = tokens.iter().rposition(|t| t.token_type == TokenType::Colon);
                        if let Some(position) = colon_position {
                            if position > *i && tokens.get(position - 1).is_some_and(|t| t.token_type == TokenType::GreaterThan && tokens.get(position + 1).is_some_and(|t| t.token_type == TokenType::Identifier) && tokens.get(position + 2).is_some_and(|t| matches!(t.token_type, TokenType::LParen | TokenType::LessThan))) {
                                // part of function declaration: TYPE: NAME()
                                *i = position;
                                continue;
                            }
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
                    return Box::new(ASTNode::err());
                }
                last_was_ident = false;
            } else if token.token_type == TokenType::RightArrow {
                last_was_unary_operator = false;
                if last_was_ident {
                    // scope traversal
                    expr_stack.push(Box::new(ASTNode {
                        token: tokens[if *i >= 1 { *i - 1 } else { *i }].clone(),
                        node: Box::new(NodeType::ScopedExpression(self.scope_call(tokens, i, until == Self::PARSING_FOR_STATEMENT))),
                    }));
                }
                else {
                    self.error(line!(), "Expected identifier before pointer arrow", "Expected identifier before pointer arrow: `ptr->member`", &token.location);
                    return Box::new(ASTNode::err());
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
                    return Box::new(ASTNode::err());
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
                        if tokens.get(*i + 1).map_or(false, |t| t.token_type.is_arrow_or_dot()) {
                            let last_punc = tokens[*i + 1].token_type.scope_type();
                            // scope traversal
                            let tuple_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: tuple,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };
                            Self::inc(i);

                            let scope = self.scope_call_with_scope(tokens, i, tuple_scope, last_punc, until == Self::PARSING_FOR_STATEMENT);

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

                        if tokens.get(*i + 1).map_or(false, |t| t.token_type.is_arrow_or_dot()) {
                            let last_punc = tokens[*i + 1].token_type.scope_type();
                            // scope traversal
                            Self::inc(i);
                            let parenthesis_scope = ScopedIdentifier {
                                scope: vec![Identifier {
                                    expression: node,
                                    scope_type: None,
                                    type_parameters: None
                                }],
                            };

                            let scope = self.scope_call_with_scope(tokens, i, parenthesis_scope, last_punc, until == Self::PARSING_FOR_STATEMENT);

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
                }
            } else if token.token_type == TokenType::RBrace {
                //self.error(line!(), "Missing delimeter", "Expected opening brace `{`", &tokens[*i].location);
                break;
            } else if token.token_type == TokenType::QuestionMark {
                // ternary operator a ? b : c
                
                let mut left_operand = self.expression_stacks_to_ast_node(&mut op_stack, &mut expr_stack).unwrap_or_else(|| {
                    self.error(line!(), "Ternary operator has no left operand", "Ternary operator must have a left operand: `a ? b : c`", &token.location);
                    return Box::new(ASTNode::err());
                });
                if until == Self::PARSING_FOR_OBJECT_INSTANTIATION {
                    let right_half;
                    if let NodeType::Operator(op) = left_operand.node.as_ref() {
                        if op.operator.token_type != TokenType::Assign {
                            self.error(line!(), "Error parsing ternary operator in object instantiation", "This operator has the highest precedence, but expected `=` due to the object instantiation: `A { B = C ? D : E }`", &op.operator.location);
                            return Box::new(ASTNode::err());
                        }
                        op_stack.push(op.operator.clone());
                        expr_stack.push(op.left.clone());
                        right_half = op.right.clone();
                    } else {
                        self.error(line!(), "Error parsing ternary operator in object instantiation", "Expected operator `=` due to the object instantiation, but didn't even find an operator: `A { B = C ? D : E }`", &token.location);
                        return Box::new(ASTNode::err());
                    }
                    left_operand = right_half;
                }
                
                let ternary = self.get_ternary(tokens, i, left_operand, until == Self::PARSING_FOR_STATEMENT);

                if *i > 0 && tokens.get(*i - 1).map_or(false, |t| t.token_type == TokenType::RParen || t.token_type == TokenType::RBracket || t.token_type == TokenType::RBrace) && !tokens.get(*i).map_or(false, |t| t.token_type == TokenType::RParen || t.token_type == TokenType::RBracket || t.token_type == TokenType::RBrace) {
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
                let return_node = self.get_expression(tokens, i, until);
                if *return_node == ASTNode::err() {
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ReturnExpression(None))
                    }));
                } else {
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ReturnExpression(Some(return_node)))
                    }));
                }
            } else if matches!(token.token_type, TokenType::ReturnTrue | TokenType::ReturnFalse) {
                let return_true = token.token_type == TokenType::ReturnTrue;
                let return_string = if return_true { "return_true" } else { "return_false" };
                Self::inc(i);
                let return_node = self.get_expression(tokens, i, until);
                if *return_node == ASTNode::err() {
                    self.error(line!(), "Return expression error", format!("{return_string} expression must have a value: `{return_string} EXPR`").as_str(), &token.location);
                    return Box::new(ASTNode::err());
                } else {
                    expr_stack.push(Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ReturnConditionalExpression(ReturnConditional { condition: return_true, value: return_node }))
                    }));
                }
            } else if token.token_type == TokenType::Defer {
                Self::inc(i);
                let expression = self.get_expression(tokens, i, until);
                expr_stack.push(Box::new(ASTNode {
                    token: token.clone(),
                    node: Box::new(NodeType::DeferStatement(expression))
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
            }  else if token.token_type == TokenType::Enum {
                expr_stack.push(Box::new(self.get_enum(tokens, i)));
            } else if token.token_type == TokenType::Else {
                self.error(line!(), "Else statement is by itself", "Else statement must be after if statement: `if EXPR {} else {}`", &tokens[0].location);
                return Box::new(ASTNode::err());
            } else if token.token_type == TokenType::Shebang {
                if *i != 0 {
                    self.error(line!(), "Unexpected Shebang token", "Shebang must be at the start of a line: `#! ...`", &token.location);
                    return Box::new(ASTNode::err());
                }
                Self::inc(i);
                if tokens.len() == 1 {
                    self.error(line!(), "Unexpected Shebang token", "Shebang doesn't have any content: `#! ...`", &token.location);
                    return Box::new(ASTNode::err());
                }
                
                if !tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Identifier) {
                    self.error(line!(), "Unexpected Shebang token", "Shebang doesn't have correct content, expected identifier token after shebang: `#! ...`", &token.location);
                    return Box::new(ASTNode::err());    
                }
                
                match tokens.get(*i).map_or("", |t| t.value.as_str()) {
                    "allow"| "warn" | "warning" | "err" | "error" => {
                        let token = tokens[*i].clone();
                        Self::inc(i);
                        if tokens.get(*i).is_some_and(|t| t.token_type != TokenType::Identifier) {
                            self.error(line!(), "Unexpected Shebang token", "Shebang `#! allow` must have an identifier after it: `#! allow IDENTIFIER`", &token.location);
                            return Box::new(ASTNode::err());
                        }
                        let identifier = tokens[*i].clone();
                        let awe_message = match identifier.value.as_str() {
                            "unused" => ShebangAWEMessage::Unused,
                            "unreachable" => ShebangAWEMessage::Unreachable,
                            "unimplemented" => ShebangAWEMessage::Unimplemented,
                            "undeclared" => ShebangAWEMessage::Undeclared,
                            "deprecated" => ShebangAWEMessage::Deprecated,
                            "no_entrance" => ShebangAWEMessage::NoEntrance,
                            "unsafe" => ShebangAWEMessage::Unsafe,
                            _ => {
                                let messages = ["unused", "unreachable", "unimplemented", "undeclared", "deprecated", "no_entrance"];
                                self.error(line!(), "Unexpected Shebang token", format!("Shebang `#! {}` has invalid message: `{:?}`", token.value, messages).as_str(), &tokens[*i].location);
                                return Box::new(ASTNode::err());
                            }   
                        };
                        Self::inc(i);
                        match token.value.as_str() {
                            "allow" => {
                                return Box::new(ASTNode {
                                    token: token.clone(),
                                    node: Box::new(NodeType::Shebang(ShebangType::Allow(awe_message)))
                                });
                            } 
                            "warn" | "warning" => {
                                return Box::new(ASTNode {
                                    token: token.clone(),
                                    node: Box::new(NodeType::Shebang(ShebangType::Warning(awe_message)))
                                });
                            }
                            "err" | "error" => {
                                return Box::new(ASTNode {
                                    token: token.clone(),
                                    node: Box::new(NodeType::Shebang(ShebangType::Err(awe_message)))
                                });
                            }
                            _ => unreachable!()
                        }
                    }
                    "insert" => {
                        Self::inc(i);
                        if tokens.get(*i).is_none() || tokens[*i].token_type != TokenType::StringConstant {
                            self.error(line!(), "Unexpected Shebang token", "Shebang `#! insert` must have a string constant after it: `#! insert \"CODE\"`", &token.location);
                            return Box::new(ASTNode::err());
                        }
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shebang(ShebangType::Insert(tokens[*i].clone())))
                        });
                    }
                    "pragma" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shebang(ShebangType::Pragma(tokens.iter().skip(*i).map(|t| t.clone()).collect())))
                        });
                    }
                    "C" => {
                        Self::inc(i);
                        return Box::new(ASTNode {
                            token: token.clone(),
                            node: Box::new(NodeType::Shebang(ShebangType::C(tokens.iter().skip(*i).map(|t| t.clone()).collect())))
                        });
                    }
                    "crumb" | "deprecated" | "version"| "expose" | "entry"| "alias" => {
                        let tag_tokens: Vec<Box<Token>> = tokens.iter().skip(*i).map(|t| t.clone()).collect();
                        match tag_tokens.first().map_or("", |t| t.value.as_str()) {
                            "crumb" => {
                                self.__current_tags.push(Tag::Crumb);
                            }
                            "expose" => {
                                self.__current_tags.push(Tag::Expose);
                            }
                            "deprecated" => {
                                self.__current_tags.push(Tag::Deprecated);
                            }
                            "version" => {
                                if let Some(version_token) = tag_tokens.get(1) {
                                    self.__current_tags.push(Tag::Version(version_token.clone()));
                                } else {
                                    self.error(line!(), "Unexpected tag syntax", "Version tag requires version number: `#! version 0.1`", &token.location);
                                }
                            }
                            "alias" => {
                                if let Some(alias_token) = tag_tokens.get(1) {
                                    self.__current_tags.push(Tag::Alias(alias_token.clone()));
                                } else {
                                    self.error(line!(), "Unexpected tag syntax", "alias tag requires alias name: `#! alias my_alias`", &token.location);
                                }
                            }
                            _ => {
                                self.error(line!(), "Unexpected tag", "Tag not recognized, expected `crumb`, `expose`, `entry`, `deprecated`, `alias` or `version", &token.location);
                            }
                        }
                        if self.lines.get(self.__curent_parsing_line + 1).is_some_and(|l| !l.iter().any(|t| matches!(t.token_type, TokenType::Class | TokenType::Struct | TokenType::Trait))) {
                            self.__curent_parsing_line += 1;
                            *tokens = self.lines[self.__curent_parsing_line].clone();
                            *i = Self::SET_I_TO_ZERO;
                        } else {
                            return Box::new(ASTNode::err()); // not error, but returning empty
                        }
                    }
                    _ => {
                        self.error(line!(), "Unexpected Shebang token", "Shebang has unexpected content, expected `#! allow`, `#! warn`, `#! err`, `#! insert` or `#! C`. Any other messages are exped in header or should have been handled in lexer", &token.location);
                        return Box::new(ASTNode::err());
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
                    vec![parameters_node.clone()]
                }
            },
            token: parameters_node.token.clone(),
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
            if *expression == ASTNode::err() {
                BodyRegion {
                    body: vec![Box::new(ASTNode {
                        token,
                        node: Box::new(NodeType::ReturnExpression(None)),
                    })]
                }   
            } else {
                BodyRegion {
                    body: vec![Box::new(ASTNode {
                        token,
                        node: Box::new(NodeType::ReturnExpression(Some(expression))),
                    })]
                }
            }
        };

        LambdaExpression {
            parameters,
            body,
        }
    }

    fn get_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, until: u8) -> NodeParameters {
        // this allows this function to be reused for both functions and arrays
        let until_token = match until {
            Self::PARSING_FOR_FUNCTION => TokenType::RParen,
            Self::PARSING_FOR_ARRAY => TokenType::RBracket,
            Self::PARSING_FOR_STATEMENT => TokenType::LBrace,
            Self::PARSING_FOR_OBJECT_INSTANTIATION | Self::PARSING_FOR_CODE_BLOCK | Self::PARSING_FOR_ENUM => TokenType::RBrace,
            _  => { 
                self.error(line!(), "INTERNAL ERROR", "Internal parsing error, caused when invalid argument was passed while trying to get node parameters in `Parser::get_node_parameters`. Expected a valid `until` argument. Try using `PARSING_FOR_FUNCTION`, `PARSING_FOR_ARRAY`, `PARSING_FOR_STATEMENT`, `PARSING_FOR_CODE_BLOCK`, or `PARSING_FOR_ENUM`, or `PARSING_FOR_OBJECT_INSTANTIATION`", &tokens[*i].location);
                return NodeParameters { parameters: vec![], token: tokens.get(*i).unwrap_or(tokens.get(0).unwrap_or(&Box::new(Token::new_empty()))).clone() };
            }
        };

        if tokens.get(*i).is_some_and(|t| t.token_type == until_token) {
            // empty parameters
            Self::inc(i);
            return NodeParameters { parameters: vec![], token: tokens.get(*i).unwrap_or(tokens.get(0).unwrap_or(&Box::new(Token::new_empty()))).clone() };
        }

        let mut parameters = vec![];
        let token = tokens.get(*i).unwrap_or(tokens.get(0).unwrap_or(&Box::new(Token::new_empty()))).clone();

        if until == Self::PARSING_FOR_ARRAY || until == Self::PARSING_FOR_FUNCTION {
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
                //debug!(tokens.get(*i));
                if tokens[*i].token_type == until_token {
                    // ends
                    Self::inc(i);
                    done = true;
                    break;
                }
                else if tokens[*i].token_type == TokenType::Comma {
                    // add to stack
                    Self::inc(i);

                    // this is for: `[a, b,]` so that `b` can have a comma without causing an infinite loop
                    if tokens.get(*i).is_some_and(|t| t.token_type == until_token) {
                        Self::inc(i);
                        done = true;
                        break;
                    }
                    let expression = self.get_expression(tokens, i, until);
                    parameters.push(expression);
                }
                else if until == Self::PARSING_FOR_CODE_BLOCK && tokens[*i].token_type != until_token {
                    let expression = self.get_expression(tokens, i, until);
                    
                    // this is for: `if a { b } else { c }` so that `b` doesn't need to have a semicolon at the end
                    if parameters.last().is_some() && parameters.last().unwrap() == &expression && tokens.get(*i + 1).map_or(false, |t| t.token_type == until_token) {
                        Self::inc(i);
                        done = true;
                        break;
                    }
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
                } else if until != Self::PARSING_FOR_STATEMENT && until != Self::PARSING_FOR_ENUM && *i != 0 {
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

        NodeParameters { parameters, token }
    }

    fn get_tuple_node_parameters(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize) -> NodeParameters {
        // (Tuple, Type)
        let token = tokens.get(*i).unwrap_or(tokens.get(0).unwrap_or(&Box::new(Token::new_empty()))).clone();
        let all_tokens = self.get_node_parameters_for_tuple(tokens, i);
        
        let mut parameters: Vec<Box<ASTNode>> = vec![];
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            let mut zero = 0;
            parameters.push(self.get_type_idententifier(&mut tokens, &mut zero, false));
        }

        NodeParameters { parameters, token: token.clone() }
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
                let is_ptr_or_ref = vec![];
                while copy_tokens.get(*i).map_or(false, |t| t.token_type == TokenType::Ampersand) {
                    //is_ptr_or_ref.push(TypeModifier::Ref);
                    self.error(line!(), "References are not supported in Scone", "References aren't supported in scone, use pointers instead", &copy_tokens[*i].location);
                    Self::inc(i);
                }

                if copy_tokens.iter().skip(*i).len() == 1 {
                    let token = copy_tokens.get(*i);
                    if token.is_none() {
                        self.error(line!(), "Expected type", "Parser error, expected type", &first_token.location);
                        return Box::new(ASTNode::err());
                    }
                    let token = token.unwrap();
                    return Box::new(ASTNode {
                        token: token.clone(),
                        node: Box::new(NodeType::ScopedType(ScopedType {
                            token: token.clone(),
                            scope: vec![TypeIdentifier {
                                name: token.clone(),
                                type_parameters: None,
                                scope_type: None,
                            }],
                            type_modifiers: is_ptr_or_ref,
                        }))
                    });
                }
                if copy_tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::DoubleColon || t.token_type == TokenType::Dot || t.token_type == TokenType::RightArrow || t.token_type == TokenType::LessThan || t.token_type == TokenType::LBracket || t.token_type == TokenType::Star || t.token_type == TokenType::Ampersand) {
                    let scope_and_types = self.get_scoped_typed(&mut copy_tokens, i, is_ptr_or_ref);
                    if from_as_is_operators {
                        Self::dec(i);
                    }
                    return Box::new(ASTNode {
                        token: first_token.clone(),
                        node: Box::new(NodeType::ScopedType(scope_and_types)),
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
                    node: Box::new(NodeType::TupleDeclaration(TupleDeclaration {
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
                let v = vec![];

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

    fn get_scoped_typed(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, mut is_ptr_or_ref: Vec<TypeModifier>) -> ScopedType {
        let token = tokens[*i].clone();
        let scope = self.get_type_scope(tokens, i);

        while *i < tokens.len() {
            if tokens[*i].token_type == TokenType::Star {
                is_ptr_or_ref.push(TypeModifier::Ptr);
            } else if tokens[*i].token_type == TokenType::LBracket && tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::RBracket) {
                is_ptr_or_ref.push(TypeModifier::Array);
            } else if tokens[*i].token_type == TokenType::LBracket && !tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::RBracket) {
                self.error(line!(), "Error getting scoped type for array", "Arrays `[]` do not have defined length, they get transpiled: `int[]` to `Vec<int>`", &tokens[*i].location);
            } else {
                break;
            }
            Self::inc(i);
        }

        ScopedType {
            token,
            scope,
            type_modifiers: is_ptr_or_ref,
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
                    parameters: vec![],
                    token: Box::new(Token::new_empty()),
                };
            }
        }

        let token_parameter = tokens.get(*i).unwrap_or(tokens.get(0).unwrap_or(&Box::new(Token::new_empty()))).clone();

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
            if angle_bracket_level == 0 && parenthesis_level == 0 && brace_level == 0 && bracket_level == 0 {
                break;
            }
        }

        let mut return_tokens: Vec<Box<ASTNode>> = vec![];
        for tokens in all_tokens {
            let mut tokens = tokens.clone();
            return_tokens.push(self.get_type_idententifier(&mut tokens, &mut 0, false));
        }

        NodeParameters {
            parameters: return_tokens,
            token: token_parameter,
        }
    }

    fn get_ident_scope(&mut self, tokens: &mut Vec<Box<Token>>, i: &mut usize, scope: &mut ScopedIdentifier, last_punc: &mut Option<ScopeType>) -> bool { // valid_lt_as_generic
        let mut first_token = true;
        let mut keep_last_punc = None;
        let mut last_identifier_index = *i;

        while *i < tokens.len() {
            let token = tokens[*i].clone();
            match token.token_type {
                TokenType::StringConstant | TokenType::CharConstant | TokenType::NumberConstant | TokenType::BoolConstant if first_token => {
                    if tokens.get(*i + 1).is_some_and(|t| t.token_type.is_arrow_or_dot()) {
                        *last_punc = tokens[*i + 1].token_type.scope_type();
                    } else if tokens.get(*i + 1).is_some_and(|t| t.token_type == TokenType::DoubleColon) {
                        self.error(line!(), "Scope has incorrect type", "Expected or `.` before identifier and after constant. Constants are instances and cannot use `::` directly after them to scope", &token.location);
                    } else {
                        self.error(line!(), "Scope has incorrect type", "Expected or `.` before identifier and after constant. Use `.` to separate scope levels.", &token.location);
                    }
                    Self::inc(i);
                    Self::inc(i);
                    scope.scope.push(Identifier { 
                        expression: Box::new(ASTNode { 
                            token: token.clone(), 
                            node: Box::new(NodeType::Constant(ConstantNode {
                                constant_type: self.get_constant(&token),
                                value: token.clone()
                            })) 
                        }), 
                        scope_type: None, 
                        type_parameters: None 
                    });
                    return true;
                }
                TokenType::DoubleColon | TokenType::Dot | TokenType::RightArrow => {
                    Self::inc(i);
                    if (last_punc.is_some() || first_token) && !(last_punc.is_some() && scope.scope.len() != 0 && first_token) {
                        self.error(line!(), "Scope has incorrect type", format!("Consecutive `{A}` found. Use `{A}` only between valid names, for example `A{A}B{A}C`", A = token.value).as_str(), &token.location);
                        return true;
                    }
                    *last_punc = token.token_type.scope_type();
                    keep_last_punc = token.token_type.scope_type();
                }
                TokenType::Identifier => {
                    last_identifier_index = *i;
                    Self::inc(i);
                    let mut type_parameters = Some(NodeParameters { parameters: vec![], token: token.clone() });

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
                                    if tokens.get(j + 1).map_or(false, |t| t.token_type == TokenType::LParen || t.token_type == TokenType::LBrace || tokens[j + 1].token_type == TokenType::DoubleColon || tokens[j + 1].token_type == TokenType::Dot || tokens[j + 1].token_type == TokenType::RightArrow) {
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
                        return true;
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
                    return true;
                }
                TokenType::LessThan => {
                    *i = last_identifier_index;
                    *last_punc = keep_last_punc;
                    scope.scope.pop();
                    
                    // the false basically means that the next token is not the start of a type parameter list, even though it is a less than operator
                    // if it were, it would have been handled above in the TokenType::Identifier case
                    return false; 
                }
                _ => {
                    *i = last_identifier_index;
                    *last_punc = keep_last_punc;
                    scope.scope.pop();
                    return true;
                }
            }
        }

        if last_punc.is_some() {
            self.error(line!(), "Scope has incorrect type", "Trailing punctuation found. A valid identifier must follow `::` or `.`", &tokens.last().unwrap().location);
            return true;
        }

        return true;
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
                    self.error(line!(), "Incorrect Scoping syntax", "Expected `::` for type scoping, found `.`", &token.location);
                    break;
                    // Self::inc(i);
                    // if last_punc.is_some() {
                    //     self.error(line!(), "Scope has incorrect type", "Consecutive `.` found. Use `.` only between valid names, for example `A.B.C`", &token.location);
                    //     return scope;
                    // }
                    // last_punc = Some(ScopeType::Dot);
                    // first_token = false;
                }
                TokenType::RightArrow => {
                    self.error(line!(), "Incorrect Scoping syntax", "Expected `::` for type scoping, found `->`", &token.location);
                    break;
                }
                TokenType::Identifier => {
                    let mut maybe_type_parameters = NodeParameters { parameters: vec![], token: token.clone() };
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
            // first check the `const`
            let mut is_const = false;
            if tokens[*i].token_type == TokenType::Const {
                is_const = true;
                Self::inc(i);
            }

            // next get the type
            // go through tokens until `:`
            let mut type_tokens = vec![];
            while *i < tokens.len() && tokens[*i].token_type != TokenType::Colon {
                type_tokens.push(tokens[*i].clone());
                Self::inc(i);
            }
            let ty = self.get_type_idententifier(&mut type_tokens, &mut 0, false);

            // next is the name
            Self::inc(i);
            let name = tokens.get(*i).cloned().unwrap_or_else(|| {
                self.error(line!(), "Error parsing defined node parameters", "Expected next token to be name: `TYPE: NAME`", &error_token.location);
                Box::new(Token::new_empty())
            }).clone();
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
            
            let params;
            if tokens.get(*i).map_or(false, |t| t.token_type == TokenType::DotDotDot) {
                params = true;
                Self::inc(i);
            } else {
                params = false;
            }

            parameters.push(DefinedNodeParameter { name, ty, default_value, params, is_const });

            // check if next token is `,` or `)` 
            if let Some(nt) = tokens.get(*i) {
                match nt.token_type {
                    TokenType::Comma => {
                        Self::inc(i);
                        if params {
                            self.error(line!(), "Error parsing defined node parameters", "Expected next token to be `,` or `)`. The `...` parameter must be the last parameter", tokens.get(*i - 2).map_or(&nt.location, |t| &t.location));
                        }
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

    pub fn node_expr_to_string(node: &ASTNode, mut tab_level: usize) -> String {
        match *node.node {
            NodeType::VariableDeclaration(ref value) => {
                let var_type = Self::node_expr_to_string(&value.var_type, tab_level);
                let var_name = value.var_name.value.clone();
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
                if let Some(v) = &value.var_value {
                    let var_value = Self::node_expr_to_string(v, tab_level);
                    format!("{}{}{}: {} = {}", tags, access_modifiers, var_type, var_name, var_value)
                } else {
                    format!("{}{}{}: {}", tags, access_modifiers, var_type, var_name)
                }
            }
            NodeType::ScopedType(ref value) => {
                let mut pointer = "".to_string();
                let mut reference = "".to_string();
                for modifier in value.type_modifiers.iter() {
                    match modifier {
                        TypeModifier::Ptr => {
                            pointer += "*";
                        },
                        TypeModifier::Array => {
                            pointer += "[]";
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
                format!("{}{}{}", reference, scope, pointer)
            }
            NodeType::TupleDeclaration(ref value) => {
                let mut tuple = "".to_string();
                for (index, param) in value.parameters.parameters.iter().enumerate() {
                    tuple += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.parameters.parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
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

                if value.operator.value == "-" && operand.starts_with("-") {
                    format!("({}({}))", value.operator.value, operand)
                } else {
                    format!("({}{})", value.operator.value, operand)
                }
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
                if matches!(value.constant_type, ConstantType::String | ConstantType::Char) {
                    let mut text = "".to_string();
                    for t in value.value.value.chars() {
                        match t {
                            '\n' => text += "\\n",
                            '\r' => text += "\\r",
                            '\t' => text += "\\t",
                            '\\' => text += "\\\\",
                            '\"' => text += "\\\"",
                            '\'' => text += "\\\'",
                            '\0' => text += "\\0",
                            _ => text += &t.to_string(), 
                        }
                    }

                    if value.constant_type == ConstantType::String {
                        format!("\"{}\"", text)
                    } else if value.constant_type == ConstantType::Char {
                        format!("'{}'", text)
                    } else {
                        unreachable!()
                    }   
                } else {
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
                for (i, param) in value.index.parameters.iter().enumerate() {
                    index += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.index.parameters.get(i + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                format!("{}[{}]", object, index)
            }
            NodeType::ObjectInstantiation(ref value) => {
                let mut object = "".to_string();
                let object_type = &value.object_type.value;
                for (index, param) in value.properties.iter().enumerate() {
                    object += format!("{} = {}{}", param.name.value, Self::node_expr_to_string(&param.value, tab_level).as_str(), value.properties.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                }
                let mut type_parameters = "".to_string();
                if value.type_parameters.is_some() {
                    type_parameters += "<";
                    for (index, param) in value.type_parameters.clone().unwrap().parameters.iter().enumerate() {
                        type_parameters += format!("{}{}", Self::node_expr_to_string(param, tab_level).as_str(), value.type_parameters.clone().unwrap().parameters.get(index + 1).is_some().then(|| ", ").unwrap_or("")).as_str();
                    }
                    type_parameters += ">";
                }
                if object.is_empty() {
                    format!("{} {{}}", object_type)
                }
                else {
                    format!("{}{} {{ {} }}", object_type, type_parameters, object)
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
                if value.is_some() {
                    format!("return {}", Self::node_expr_to_string(&value.clone().unwrap(), tab_level))
                } else {
                    "return".to_string()
                }
            }
            NodeType::ReturnConditionalExpression(ref value) => {
                if value.condition {
                    format!("return_true {}", Self::node_expr_to_string(&value.value.clone(), tab_level))
                } else {
                    format!("return_false {}", Self::node_expr_to_string(&value.value.clone(), tab_level))
                }
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

                format!("{} {} {}{}{}", if_name, condition, body, else_if_string, else_string)
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

                    cases = format!("{}{},", cases, c);
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
            NodeType::Shebang(ref value) => {
                match value {
                    ShebangType::Allow(s) => format!("#! allow {}", s.to_string()),
                    ShebangType::Warning(s) => format!("#! warn {}", s.to_string()),
                    ShebangType::Err(s) => format!("#! err {}", s.to_string()),
                    ShebangType::Insert(s) => format!("#! insert \"{}\"", s.value),
                    ShebangType::C(s) => format!("#! C {}", s.iter().map(|x| x.value.clone()).collect::<Vec<String>>().join(" ")),
                    ShebangType::Pragma(s) => format!("#! pragma {}", s.iter().map(|x| x.value.clone()).collect::<Vec<String>>().join(" ")),
                }
            }
            NodeType::FunctionDeclaration(ref value) => {
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
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
                    if param.is_const {
                        parameters += "const ";
                    }
                    parameters += &Self::node_expr_to_string(&param.ty, tab_level);
                    parameters += ": ";
                    parameters += &param.name.value;

                    if param.default_value.is_some() {
                        parameters += " = ";
                        parameters += &Self::node_expr_to_string(&param.default_value.clone().unwrap(), tab_level);
                    }

                    if param.params {
                        parameters += " ...";
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
                format!("{}{}{}: {}{}{} {}", tags, access_modifiers, ty, name, generics, parameters, body)
            }
            NodeType::TypeDef(ref value) => {
                let expression = Self::node_expr_to_string(&value.type_definition, tab_level);
                let name = value.name.value.clone();
                format!("typedef {}: {}", expression, name)
            }
            NodeType::EnumDeclaration(ref value) => {
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
                let name = value.name.value.clone();
                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.iter() {
                    if param.1.is_none() {
                        body += format!("{}{},\n", "    ".repeat(tab_level), param.0.value.clone()).as_str();
                    } else {
                        body += format!("{}{} = {},\n", "    ".repeat(tab_level), param.0.value.clone(), Self::node_expr_to_string(param.1.as_ref().unwrap(), tab_level)).as_str();
                    }
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));
                format!("{}enum {} {}", tags, name, body)
            }
            NodeType::ClassDeclaration(ref value) => {
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
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
                let mut extends = "".to_string();
                if value.extends.len() > 0 {
                    extends += " -> ";
                    extends += &value.extends.iter().map(|x| x.value.clone()).collect::<Vec<String>>().join(", ");
                }
                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{};\n", "    ".repeat(tab_level), Self::node_expr_to_string(param, tab_level)).as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));

                format!("{}{}class {}{}{} {}", tags, access_modifiers, name, generics, extends, body)
            }
            NodeType::StructDeclaration(ref value) => {
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
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
                let mut extends = "".to_string();
                if value.extends.len() > 0 {
                    extends += " -> ";
                    extends += &value.extends.iter().map(|x| x.value.clone()).collect::<Vec<String>>().join(", ");
                }
                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{};\n", "    ".repeat(tab_level), Self::node_expr_to_string(param, tab_level)).as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));

                format!("{}{}struct {}{}{} {}", tags, access_modifiers, name, generics, extends, body)
            }
            NodeType::TraitDeclaration(ref value) => {
                let tags = value.tags.iter().map(|x| x.to_string() + "\n" + &"    ".repeat(tab_level)).collect::<String>();
                let access_modifiers = value.access_modifier.iter().map(|x| x.to_string() + " ").collect::<String>();
                let name = value.name.value.clone();
                let mut extends = "".to_string();
                if value.extends.len() > 0 {
                    extends += " -> ";
                    extends += &value.extends.iter().map(|x| x.value.clone()).collect::<Vec<String>>().join(", ");
                }
                let mut body = "".to_string();
                tab_level += 1;
                for param in value.body.body.iter() {
                    body += format!("{}{};\n", "    ".repeat(tab_level), Self::node_expr_to_string(param, tab_level)).as_str();
                }
                tab_level -= 1;
                body = format!("{{\n{}{}}}", body, "    ".repeat(tab_level));

                format!("{}{}trait {}{} {}", tags, access_modifiers, name, extends, body)
            }
            NodeType::DeferStatement(ref value) => {
                let expression = Self::node_expr_to_string(&value, tab_level);
                format!("defer {};", expression)
            }
            _ => {
                node.token.value.clone()
            }
        }
    }

    fn get_constant(&mut self, token: &Token) -> ConstantType {
        if let Ok(constant_type) = Self::constant_type(token) {
            if token.value.contains("0x") || token.value.contains("0b") || token.value.contains("e") {
                return constant_type
            }
            let number = token.value.to_lowercase().replace("_", "").replace("u8", "").replace("i8", "").replace("u16", "").replace("i16", "").replace("u32", "").replace("i32", "").replace("u64", "").replace("i64", "").replace("f32", "").replace("f64", "");
            if constant_type.is_number() {
                if let ConstantType::U8 = constant_type {
                    if let Ok(value) = number.parse::<u8>() {
                        if value >= u8::MIN && value <= u8::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::I8 = constant_type {
                    if let Ok(value) = number.parse::<i8>() {
                        if value >= i8::MIN && value <= i8::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::U16 = constant_type {
                    if let Ok(value) = number.parse::<u16>() {
                        if value >= u16::MIN && value <= u16::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::I16 = constant_type {
                    if let Ok(value) = number.parse::<i16>() {
                        if value >= i16::MIN && value <= i16::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::U32 = constant_type {
                    if let Ok(value) = number.parse::<u32>() {
                        if value >= u32::MIN && value <= u32::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::I32 = constant_type {
                    if let Ok(value) = number.parse::<i32>() {
                        if value >= i32::MIN && value <= i32::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::U64 = constant_type {
                    if let Ok(value) = number.parse::<u64>() {
                        if value >= u64::MIN && value <= u64::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::I64 = constant_type {
                    if let Ok(value) = number.parse::<i64>() {
                        if value >= i64::MIN && value <= i64::MAX {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::F32 = constant_type {
                    if let Ok(value) = number.parse::<f32>() {
                        if value.is_finite() {
                            return constant_type;
                        } 
                    }
                } else if let ConstantType::F64 = constant_type {
                    if let Ok(value) = number.parse::<f64>() {
                        if value.is_finite() {
                            return constant_type;
                        } 
                    }
                }
                self.error(line!(), "Constant type is not valid", "Expected constant number to be within its type's range, for example: `100_u8` is valid but `300_u8` is not because u8 range is from 0 to 255", &token.location);
            } 
            return constant_type;
        } else {
            self.error(line!(), "Error getting constant type", "Could not parse constant. This may be because of invalid type in number. As an example of valid number typing: `100_f32`", &token.location);
            ConstantType::U8
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
                else if number.ends_with("f64") && !number.starts_with("0x") { return Ok(ConstantType::F64) }
                else if number.ends_with("f32") && !number.starts_with("0x") { return Ok(ConstantType::F32) }
                else if number.starts_with("0x") { return Ok(ConstantType::I32) }
                else if number.starts_with("0b") { return Ok(ConstantType::I32) }
                else if number.contains("e") { return Ok(ConstantType::F64) }
                else if let Ok(_) = number.parse::<u8>() { return Ok(ConstantType::U8) }
                else if let Ok(_) = number.parse::<i8>() { return Ok(ConstantType::I8)}
                else if let Ok(_) = number.parse::<u16>() { return Ok(ConstantType::U16) }
                else if let Ok(_) = number.parse::<i16>() { return Ok(ConstantType::I16) }
                else if let Ok(_) = number.parse::<u32>() { return Ok(ConstantType::U32) }
                else if let Ok(_) = number.parse::<i32>() { return Ok(ConstantType::I32) }
                else if let Ok(_) = number.parse::<u64>() { return Ok(ConstantType::U64) }
                else if let Ok(_) = number.parse::<i64>() { return Ok(ConstantType::I64) }
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