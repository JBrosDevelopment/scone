use serde::Serialize;
use crate::macros::Macros;

#[allow(unused_imports)]
use crate::error_handling::{ErrorHandling, DEBUGGING};
#[allow(unused_imports)]
use crate::debug;

pub fn lex(code: &String, path: Option<String>, macros: Option<Macros>) -> (Vec<Token>, ErrorHandling, Macros) {
    let mut lexer = Lexer::new(code, path, macros.unwrap_or(Macros::new()));
    let tokens = lexer.lex();

    lexer.output.print_messages();
    (tokens, lexer.output, lexer.macros)
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum TokenType {
    // operators
    Plus,
    Dash,
    Star,
    Slash,
    Pipe,
    Carrot,
    Ampersand,
    Tilda,
    Modulas,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Assign,
    And,
    Or,
    Not,
    Hashtag,
    AtSymbol,
    
    // brackets
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // punctuation
    Comma,
    Dot,
    Colon,
    DoubleColon,
    DollarSign,
    RightArrow,
    DoubleArrow,
    Shebang,
    QuestionMark,
    DotDotDot,
    BackTick,

    // keywords
    Return,
    ReturnTrue,
    ReturnFalse,
    If,
    Else,
    For,
    While,
    Break,
    Continue,
    Class,
    Struct,
    Enum,
    Use,
    Trait,
    In,
    As,
    Is,
    Const,
    Static,
    Pub,
    Priv,
    Virtual,
    Override,
    Extern,
    Match,
    Unsafe,
    Safe,
    TypeDef,
    Defer,

    // constants
    BoolConstant,
    StringConstant,
    CharConstant,
    NumberConstant,

    // other
    Underscore,
    Identifier,
    EndOfLine,
    None
}

#[macro_export]
macro_rules! operator_tokens {
    () => {
        TokenType::Plus |
        TokenType::Dash |
        TokenType::Star |
        TokenType::Slash |
        TokenType::Pipe |
        TokenType::Carrot |
        TokenType::Ampersand |
        TokenType::Tilda |
        TokenType::Modulas |
        TokenType::Equal |
        TokenType::NotEqual |
        TokenType::GreaterThan |
        TokenType::GreaterThanOrEqual |
        TokenType::LessThan |
        TokenType::LessThanOrEqual |
        TokenType::Assign |
        TokenType::And |
        TokenType::Or |
        TokenType::Not |
        TokenType::Is |
        TokenType::As |
        TokenType::In
    };
}

impl TokenType {
    pub fn is_access_modifier(&self) -> bool {
        matches!(self, TokenType::Pub | TokenType::Priv | TokenType::Virtual | TokenType::Override | TokenType::Extern | TokenType::Unsafe | TokenType::Safe | TokenType::Const | TokenType::Static)
    }
    pub fn is_operator(&self) -> bool {
        matches!(self,
            operator_tokens!()
        )
    }
    pub fn operator_boolean(&self) -> bool {
        matches!(self, TokenType::Equal | TokenType::NotEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual | TokenType::LessThan | TokenType::LessThanOrEqual | TokenType::And | TokenType::Or | TokenType::Not | TokenType::Is | TokenType::As)
    }
    pub fn operator_assignable(&self) -> bool {
        matches!(self, TokenType::Plus | TokenType::Dash | TokenType::Star | TokenType::Slash | TokenType::Pipe | TokenType::Carrot | TokenType::Ampersand | TokenType::Modulas | TokenType::And | TokenType::Or)
    }
    pub fn is_constant(&self) -> bool {
        matches!(self, TokenType::BoolConstant | TokenType::StringConstant | TokenType::CharConstant | TokenType::NumberConstant)
    }
    pub fn is_arrow_or_dot(&self) -> bool {
        matches!(self, TokenType::RightArrow | TokenType::Dot)
    }
    pub fn scope_type(&self) -> Option<crate::ast::ScopeType> {
        match self {
            TokenType::Dot => Some(crate::ast::ScopeType::Dot),
            TokenType::DoubleColon => Some(crate::ast::ScopeType::DoubleColon),
            TokenType::RightArrow => Some(crate::ast::ScopeType::Arrow),
            _ => None
        }
    }
    pub fn precedence(&self) -> i32 {
        match self {
            TokenType::And | TokenType::Or => 1,
            TokenType::Ampersand | TokenType::Pipe => 2,
            TokenType::Equal | TokenType::NotEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual | TokenType::LessThan | TokenType::LessThanOrEqual => 3,
            TokenType::Plus | TokenType::Dash => 4,
            TokenType::Star | TokenType::Slash | TokenType::Modulas => 5, 
            TokenType::Is | TokenType::As => 6,
            TokenType::LParen | TokenType::RParen => 0, 
            _ => -1, 
        }
    }
    pub fn is_unary_operator(&self) -> bool {
        matches!(self, TokenType::Dash | TokenType::Not | TokenType::Star | TokenType::Ampersand | TokenType::Tilda)
    }
    
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Location {
    pub line: i32,
    pub column: i32,
    pub length: i32
}
impl Location {
    pub fn new(line: i32, column: i32, length: i32) -> Location {
        Location {
            line,
            column,
            length
        }
    }
    pub fn advance(&mut self, length: i32) {
        self.column = self.column + self.length;
        self.length = length;
    }
    pub fn advance_line(&mut self) {
        self.line = self.line + 1;
        self.column = 1;
        self.length = 0;
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub location: Location
}
impl Token {
    pub fn new(token_type: TokenType, value: String, location: Location) -> Token {
        Token {
            token_type,
            value,
            location
        }
    }
    pub fn new_empty() -> Token {
        Token {
            token_type: TokenType::None,
            value: String::new(),
            location: Location::new(0, 0, 0)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    output: ErrorHandling,
    macros: Macros,
    // code at: self.output.full_code
}

impl Lexer {
    pub fn new(code: &String, path: Option<String>, macros: Macros) -> Lexer {
        Lexer {
            output: ErrorHandling::new(path.clone(), code.clone()),
            macros
        }
    }
    pub fn error(&mut self, message: &str, help: &str, location: &Location) {
        self.output.error("lexer error", message, help, location);
    }
    pub fn warning(&mut self, message: &str, help: &str, location: &Location) {
        self.output.warning("lexer warning", message, help, location);
    }
    pub fn message(&mut self, message: &str, help: &str, location: &Location) {
        self.output.message("lexer message", message, help, location);
    }
    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let chars: Vec<char> = self.output.full_code.chars().collect::<Vec<char>>();
        let mut current_location: Location = Location::new(1, 1, 0);

        let mut is_in_shebang_line = false;
        let mut macro_if_value: Option<bool> = None; // reflects value of `if` in shebangs
        let mut macro_if_has_been_true = None;

        // if the last token was `(`  `{`  `[`  `,`  `..`  `OPERATORS`, `END_OF_LINE`6
        // set to 3 if true, so that it can go down every time it finds a new token
        // finds it at `3`, finds negative at `2` moves to `-1` if found, and number at `1`
        let mut last_was_negatable_ability = 2; // is at 2 now because already found ability
    
        let mut i = 0;
        while i < chars.len() {
            let c = chars[i];
            if c == '#' {
                if chars.get(i + 1) == Some(&'!') {
                    i += 2;

                    // handle shebang
                    let mut j = i;
                    let mut message = "".to_string();
                    while j < chars.len() {
                        if chars[j] == '\n' {
                            break;
                        }
                        message.push(chars[j]);
                        j += 1;
                    }
                    let message = message.trim().to_string();
                    let directive = message.trim().split(" ").collect::<Vec<&str>>().first().cloned().unwrap_or(&"");
                    match directive {
                        "allow" | "warn" | "warning" | "err" | "error" | "deprecated" | "crumb" | "insert" | "C" | "pragma" | "version" | "expose" | "alias" | "entry" => {
                            current_location.advance(2);
                            tokens.push(Token::new(TokenType::Shebang, "#!".to_string(), current_location.clone()));
                            is_in_shebang_line = true;
                            continue;
                        }
                        "if" => {
                            if macro_if_value.is_some() || macro_if_has_been_true.is_some() {
                                self.error("Unexpected `if` directive", "Nesting of `if` or `ifn` is not supported", &current_location);
                                continue;
                            }
                            match self.macros.parse_if_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(v);
                                    if v {
                                        macro_if_has_been_true = Some(true);
                                    } else {
                                        macro_if_has_been_true = Some(false);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `if` directive", &e, &current_location);
                                }
                            }
                        } 
                        "ifn" => {
                            if macro_if_value.is_some() || macro_if_has_been_true.is_some() {
                                self.error("Unexpected `ifn` directive", "Nesting of `if` or `ifn` is not supported", &current_location);
                                continue;
                            }
                            match self.macros.parse_if_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(!v);
                                    if !v {
                                        macro_if_has_been_true = Some(true);
                                    } else {
                                        macro_if_has_been_true = Some(false);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `ifn` directive", &e, &current_location);
                                }
                            }
                        }
                        "elif" => {
                            if macro_if_value.is_none() || macro_if_has_been_true.is_none() {
                                self.error("Unexpected `elif` directive", "Expected `if` or `elif` directive before `elif`", &current_location);
                                continue;
                            }
                            if macro_if_has_been_true.unwrap() == true {
                                macro_if_value = Some(false);
                                continue; // skip this elif
                            }
                            match self.macros.parse_if_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(v);
                                    if v {
                                        macro_if_has_been_true = Some(true);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `elif` directive", &e, &current_location);
                                }
                            }
                        }
                        "elifn" => {
                            if macro_if_value.is_none() || macro_if_has_been_true.is_none() {
                                self.error("Unexpected `elifn` directive", "Expected `if` or `elif` directive before `elifn`", &current_location);
                                continue;
                            }
                            if macro_if_has_been_true.unwrap() == true {
                                macro_if_value = Some(false);
                                continue; // skip this elifn
                            }
                            match self.macros.parse_if_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(!v);
                                    if !v {
                                        macro_if_has_been_true = Some(true);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `elifn` directive", &e, &current_location);
                                }
                            }
                        }
                        "elifdef" => {
                            if macro_if_value.is_none() || macro_if_has_been_true.is_none() {
                                self.error("Unexpected `elifdef` directive", "Expected `if` or `elif` directive before `elifdef`", &current_location);
                                continue;
                            }
                            if macro_if_has_been_true.unwrap() == true {
                                macro_if_value = Some(false);
                                continue; // skip this elif
                            }
                            match self.macros.parse_if_define_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(v);
                                    if v {
                                        macro_if_has_been_true = Some(true);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `elifdef` directive", &e, &current_location);
                                }
                            }
                        }
                        "elifndef" => {
                            if macro_if_value.is_none() || macro_if_has_been_true.is_none() {
                                self.error("Unexpected `elifndef` directive", "Expected `if` or `elif` directive before `elifndef`", &current_location);
                                continue;
                            }
                            if macro_if_has_been_true.unwrap() == true {
                                macro_if_value = Some(false);
                                continue; // skip this elifn
                            }
                            match self.macros.parse_if_define_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(!v);
                                    if !v {
                                        macro_if_has_been_true = Some(true);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `elifndef` directive", &e, &current_location);
                                }
                            }
                        }
                        "else" => {
                            if macro_if_value.is_none() ||  macro_if_has_been_true.is_none() {
                                self.error("Unexpected `else` directive", "Expected `if` or `elif` directive before `else`", &current_location);
                                continue;
                            }
                            if macro_if_has_been_true.unwrap() == true {
                                macro_if_value = Some(false);
                            } else if macro_if_has_been_true.unwrap() == false {
                                macro_if_value = Some(true);
                                macro_if_has_been_true = Some(true);
                            }
                        } 
                        "endif" => {
                            macro_if_value = None; // end of if directive, reset value
                            macro_if_has_been_true = None;
                        }
                        "ifdef" => {
                            if macro_if_value.is_some() || macro_if_has_been_true.is_some() {
                                self.error("Unexpected `ifdef` directive", "Nesting of `if` or `ifdef` is not supported", &current_location);
                                continue;
                            }
                            match self.macros.parse_if_define_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(v);
                                    if v {
                                        macro_if_has_been_true = Some(true);
                                    } else {
                                        macro_if_has_been_true = Some(false);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `ifdef` directive", &e, &current_location);
                                }
                            }
                        }
                        "ifndef" => {
                            if macro_if_value.is_some() || macro_if_has_been_true.is_some() {
                                self.error("Unexpected `ifndef` directive", "Nesting of `if` or `ifndef` is not supported", &current_location);
                                continue;
                            }
                            match self.macros.parse_if_define_directive(message) {
                                Ok(v) => {
                                    macro_if_value = Some(!v);
                                    if !v {
                                        macro_if_has_been_true = Some(true);
                                    } else {
                                        macro_if_has_been_true = Some(false);
                                    }
                                },
                                Err(e) => {
                                    self.error("Error parsing `ifdef` directive", &e, &current_location);
                                }
                            }
                        }
                        "def" | "define" => {
                            match self.macros.parse_define_directive(message) {
                                Ok(_) => {},
                                Err(e) => {
                                    self.error("Error parsing `def` directive", &e, &current_location);
                                }
                            }
                        }
                        "undef" | "undefine" => {
                            match self.macros.parse_undefine_directive(message) {
                                Ok(_) => {},
                                Err(e) => {
                                    self.error("Error parsing `undef` directive", &e, &current_location);
                                }
                            }
                        }
                        _ => {
                            self.error("Unknown shebang directive", "`if`, `ifn`, `elif`, `elifn`, `else`, `endif`, `ifdef`, `ifndef`, `def`, `undef` is supported in shebangs", &current_location);
                        }
                    }
                    while i < chars.len() && chars[i] != '\n' {
                        i += 1;
                        current_location.advance(1);
                    }

                    current_location.advance_line();
                    tokens.push(Token::new(TokenType::EndOfLine, "\n".to_string(), current_location.clone()));
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Hashtag, "#".to_string(), current_location.clone()));
                }
            }
            
            if c == ' ' {
                current_location.advance(1);
                last_was_negatable_ability += 1;
            }
            else if c == '\t' {
                current_location.advance('\t'.to_string().len() as i32);
            }
            else if c == '\r' {
                // nothing
            }
            else if c == '\n' {
                current_location.advance_line();
                last_was_negatable_ability = 3;
                if is_in_shebang_line {
                    is_in_shebang_line = false;
                    tokens.push(Token::new(TokenType::EndOfLine, "\n".to_string(), current_location.clone()));
                }
            }

            if macro_if_value.is_some_and(|x| x == false) {
                i += 1;
                current_location.advance(1);
                continue;
            }

            if matches!(c, ' ' | '\t' | '\r' | '\n' | '#') {
                // already handled above
            } 
            else if c == ';' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::EndOfLine, ";".to_string(), current_location.clone()));
                last_was_negatable_ability = 3;
            }
            else if char::is_alphabetic(c.clone()) || c == '_' {
                let mut name = String::new();
        
                while i < chars.len() {
                    if char::is_alphanumeric(chars[i]) || char::is_numeric(chars[i]) || chars[i] == '_' {
                        name.push(chars[i]);
                        i += 1;
                    } else {
                        break;
                    }
                }
    
                current_location.advance(name.len() as i32);

                if let Some(v) = self.macros.get_variable(&name){
                    let mut lexer = Lexer::new(&v.value, None, self.macros.clone());
                    let lexer_tokens = lexer.lex();
                    for t in lexer_tokens {
                        tokens.push(t);
                    }
                    last_was_negatable_ability -= 1;
                    continue;
                }
    
                let new_token = match name.as_str() {
                    "true" | "false" => Token::new(TokenType::BoolConstant, name.clone(), current_location.clone()),
                    "return" => Token::new(TokenType::Return, name.clone(), current_location.clone()),
                    "return_true" => Token::new(TokenType::ReturnTrue, name.clone(), current_location.clone()),
                    "return_false" => Token::new(TokenType::ReturnFalse, name.clone(), current_location.clone()),
                    "if" => Token::new(TokenType::If, name.clone(), current_location.clone()),
                    "else" => Token::new(TokenType::Else, name.clone(), current_location.clone()),
                    "for" => Token::new(TokenType::For, name.clone(), current_location.clone()),
                    "while" => Token::new(TokenType::While, name.clone(), current_location.clone()),
                    "break" => Token::new(TokenType::Break, name.clone(), current_location.clone()),
                    "continue" => Token::new(TokenType::Continue, name.clone(), current_location.clone()),
                    "class" => Token::new(TokenType::Class, name.clone(), current_location.clone()),
                    "struct" => Token::new(TokenType::Struct, name.clone(), current_location.clone()),
                    "enum" => Token::new(TokenType::Enum, name.clone(), current_location.clone()),
                    "use" => Token::new(TokenType::Use, name.clone(), current_location.clone()),
                    "trait" => Token::new(TokenType::Trait, name.clone(), current_location.clone()),
                    "in" => Token::new(TokenType::In, name.clone(), current_location.clone()),
                    "as" => Token::new(TokenType::As, name.clone(), current_location.clone()),
                    "is" => Token::new(TokenType::Is, name.clone(), current_location.clone()),
                    "const" => Token::new(TokenType::Const, name.clone(), current_location.clone()),
                    "static" => Token::new(TokenType::Static, name.clone(), current_location.clone()),
                    "pub" => Token::new(TokenType::Pub, name.clone(), current_location.clone()),
                    "priv" => Token::new(TokenType::Priv, name.clone(), current_location.clone()),
                    "virtual" => Token::new(TokenType::Virtual, name.clone(), current_location.clone()),
                    "override" => Token::new(TokenType::Override, name.clone(), current_location.clone()),
                    "match" => Token::new(TokenType::Match, name.clone(), current_location.clone()),
                    "extern" => Token::new(TokenType::Extern, name.clone(), current_location.clone()),
                    "unsafe" => Token::new(TokenType::Unsafe, name.clone(), current_location.clone()),
                    "safe" => Token::new(TokenType::Safe, name.clone(), current_location.clone()),
                    "typedef" => Token::new(TokenType::TypeDef, name.clone(), current_location.clone()),
                    "defer" => Token::new(TokenType::Defer, name.clone(), current_location.clone()),
                    "_" => Token::new(TokenType::Underscore, name.clone(), current_location.clone()),
                    _ => Token::new(TokenType::Identifier, name.clone(), current_location.clone()),
                };
    
                tokens.push(new_token);
                i -= 1;
            }
            else if char::is_numeric(c.clone()) {
                let mut number = String::new();

                if last_was_negatable_ability == -2 {
                    number.push('-'); // add `-` to number
                    tokens.pop(); // remove `-` from tokens
                    current_location.column -= 1; // remove `-` from location
                }
        
                let mut one_dot = false;
                let mut hex = false;
                let mut binary = false;
                let mut exponent = false;
                while i < chars.len() {
                    if char::is_numeric(chars[i]) {
                        if binary && chars[i] != '0' && chars[i] != '1' {
                            self.error("invalid binary number", "Binary prefix `0b` must be followed by only `0` or `1`", &current_location);
                            break;
                        }
                        number.push(chars[i]);
                        i += 1;
                    } else if chars[i] == '.' && one_dot == false {
                        if !chars.get(i + 1).is_some_and(|x| x.is_numeric()) {
                            break;
                        }
                        number.push('.');
                        one_dot = true;
                        i += 1;
                    } else if chars[i] == '_' {
                        number.push('_');
                        i += 1;
                    } else if (chars[i] == 'f' && !hex) || chars[i] == 'u' || chars[i] == 'i' {
                        let type_c = chars[i];
                        let mut j = i + 1;
                        let mut adds = String::new();
                        while chars.get(j).is_some_and(|x| x.is_numeric()) {
                            adds.push(chars[j]);
                            j += 1;
                        }
                        if j != i + 1 {
                            number.push(type_c);
                            number.push_str(&adds);
                            i = j;
                            one_dot = true;
                        } else {
                            break;
                        }
                    } else if chars[i] == 'x' && hex == false {
                        hex = true;
                        number.push(chars[i]);
                        i += 1;
                    } else if chars[i] == 'b' && binary == false {
                        binary = true;
                        number.push(chars[i]);
                        i += 1;
                    } else if chars[i] == 'e' && exponent == false && hex == false {
                        exponent = true;
                        number.push(chars[i]);
                        if chars.get(i + 1).is_some_and(|x| x == &'+' || x == &'-') {
                            number.push(chars[i + 1]);
                            i += 1;
                        }
                        i += 1;
                    } else if chars[i].is_alphabetic() && hex {
                        if chars[i] == 'a' || chars[i] == 'b' || chars[i] == 'c' || chars[i] == 'd' || chars[i] == 'e' || chars[i] == 'f' {
                            number.push(chars[i]);
                            i += 1;
                        } else {
                            self.error("invalid hex number", "Hex prefix `0x` must be followed by only '0' to '9' and `a` to `f`", &current_location);
                            break;
                        }
                    } else {
                        break;
                    }
                }
                current_location.advance(number.len() as i32);
                let new_token = Token::new(TokenType::NumberConstant, number, current_location.clone());
                tokens.push(new_token);
                i -= 1;
            }
            else if c == ',' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Comma, ",".to_string(), current_location.clone()));
                last_was_negatable_ability = 3;
            }
            else if c == '{' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::LBrace, "{".to_string(), current_location.clone()));
                tokens.push(Token::new(TokenType::EndOfLine, "{".to_string(), current_location.clone()));
                last_was_negatable_ability = 3;
            }
            else if c == '}' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::RBrace, "}".to_string(), current_location.clone()));
                tokens.push(Token::new(TokenType::EndOfLine, "}".to_string(), current_location.clone()));
            }
            else if c == '(' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::LParen, "(".to_string(), current_location.clone()));
                last_was_negatable_ability = 3;
            }
            else if c == ')' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::RParen, ")".to_string(), current_location.clone()));
            }
            else if c == '[' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::LBracket, "[".to_string(), current_location.clone()));
                last_was_negatable_ability = 3;
            }
            else if c == ']' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::RBracket, "]".to_string(), current_location.clone()));
            }
            else if c == '$' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::DollarSign, "$".to_string(), current_location.clone()));
            }
            else if c == '?' {
                last_was_negatable_ability = 3;
                current_location.advance(1);
                tokens.push(Token::new(TokenType::QuestionMark, "?".to_string(), current_location.clone()));
            }
            else if c == '@' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::AtSymbol, "@".to_string(), current_location.clone()));
            }
            else if c == '`' {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::BackTick, "`".to_string(), current_location.clone()));
            }
            else if c == '.' {
                if chars.get(i + 1) == Some(&'.') {
                    if chars.get(i + 2) == Some(&'.') {
                        current_location.advance(3);
                        tokens.push(Token::new(TokenType::DotDotDot, "...".to_string(), current_location.clone()));
                        i += 2;
                    }
                    else {
                        current_location.advance(2);
                        tokens.push(Token::new(TokenType::DotDotDot, "..".to_string(), current_location.clone()));
                        i += 1;
                        last_was_negatable_ability = 3;
                    }
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Dot, ".".to_string(), current_location.clone()));
                }
            }
            else if c == '~' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Tilda, "~=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Tilda, "~".to_string(), current_location.clone()));
                }
            }
            else if c == '^' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Carrot, "^=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Carrot, "^".to_string(), current_location.clone()));
                }
            }
            else if c == '*' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Star, "*=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Star, "*".to_string(), current_location.clone()));
                }
            }
            else if c == '%' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Modulas, "%=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Modulas, "%".to_string(), current_location.clone()));
                }
            }
            else if c == '/' {
                if chars.get(i + 1) == Some(&'/') {
                    last_was_negatable_ability = 3;
                    i += 1;
                    
                    while i < chars.len() {
                        if chars[i] == '\n' {
                            i -= 1;
                            break;
                        }
                        i += 1;
                    }
                }
                else if chars.get(i + 1) == Some(&'*') {
                    let mut is_star = false;
                    let mut lines = 0;
                    let mut length = 0;
                    i += 1;
    
                    while i < chars.len() {    
                        if chars[i] == '\n' {
                            lines += 1;
                            length = 0;
                        } else if chars[i] == '*' {
                            is_star = true;
                        }
                        else if chars[i] == '/' && is_star {
                            break;
                        } else if chars[i] != '\r' {
                            length += 1;
                            is_star = false;    
                        }
    
                        i += 1;
                    }
    
                    for _ in 0..lines {
                        current_location.advance_line();
                    }
    
                    current_location.advance(length as i32);
                }
                else {
                    current_location.advance(1);
                    last_was_negatable_ability = 3;
                    tokens.push(Token::new(TokenType::Slash, "/".to_string(), current_location.clone()));
                }
            }
            else if c == '=' {
                if chars.get(i + 1) == Some(&'=') {
                    last_was_negatable_ability = 3;
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Equal, "==".to_string(), current_location.clone()));
                    i += 1;
                }
                else if chars.get(i + 1) == Some(&'>') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::DoubleArrow, "=>".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    last_was_negatable_ability = 3;
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Assign, "=".to_string(), current_location.clone()));
                }
            }
            else if c == '+' {
                if chars.get(i + 1) == Some(&'+') {
                    // INCREMENT is not allowed in scone
                    current_location.advance(2);
                    i += 1;
                    self.error("Scone has no increment operator", "not a valid operator, use `+=` instead", &current_location);
                }
                else if chars.get(i + 1) == Some(&'=') {
                    last_was_negatable_ability = 3;
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Plus, "+=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    last_was_negatable_ability = 3;
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Plus, "+".to_string(), current_location.clone()));
                }
            }
            else if c == '-' {
                if chars.get(i + 1) == Some(&'-') {
                    // DECREMENT is not allowed in scone
                    current_location.advance(2);
                    i += 1;
                    self.error("Scone has no decrement operator", "not a valid operator, use `-=` instead", &current_location);
                }
                else if chars.get(i + 1) == Some(&'>') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::RightArrow, "->".to_string(), current_location.clone()));
                    i += 1;
                }
                else if chars.get(i + 1) == Some(&'=') {
                    last_was_negatable_ability = 3;
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Dash, "-=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    if last_was_negatable_ability == 2 {
                        last_was_negatable_ability = -1;
                    } else {
                        last_was_negatable_ability = 0;
                    }
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Dash, "-".to_string(), current_location.clone()));
                }
            }
            else if c == '>' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::GreaterThanOrEqual, ">=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::GreaterThan, ">".to_string(), current_location.clone()));
                }
            }
            else if c == '<' {
                if chars.get(i + 1) == Some(&'=') {
                    last_was_negatable_ability = 3;
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::LessThanOrEqual, "<=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    last_was_negatable_ability = 3;
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::LessThan, "<".to_string(), current_location.clone()));
                }
            }
            else if c == '!' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::NotEqual, "!=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Not, "!".to_string(), current_location.clone()));
                }
            }
            else if c == '&' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'&') {
                    i += 1;
                    if chars.get(i + 1) == Some(&'=') {
                        current_location.advance(3);    
                        tokens.push(Token::new(TokenType::And, "&&=".to_string(), current_location.clone()));
                        i += 1;
                    }
                    else {
                        tokens.push(Token::new(TokenType::And, "&&".to_string(), current_location.clone()));
                        current_location.advance(2);
                    }
                }
                else if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Ampersand, "&=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Ampersand, "&".to_string(), current_location.clone()));
                }
            }
            else if c == '|' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&'|') {
                    i += 1;
                    if chars.get(i + 1) == Some(&'=') {
                        current_location.advance(3);    
                        tokens.push(Token::new(TokenType::Or, "||=".to_string(), current_location.clone()));
                        i += 1;
                    }
                    else {
                        tokens.push(Token::new(TokenType::Or, "||".to_string(), current_location.clone()));
                        current_location.advance(2);
                    }
                }
                else if chars.get(i + 1) == Some(&'=') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::Pipe, "|=".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Pipe, "|".to_string(), current_location.clone()));
                }
            }
            else if c == ':' {
                last_was_negatable_ability = 3;
                if chars.get(i + 1) == Some(&':') {
                    current_location.advance(2);
                    tokens.push(Token::new(TokenType::DoubleColon, "::".to_string(), current_location.clone()));
                    i += 1;
                }
                else {
                    current_location.advance(1);
                    tokens.push(Token::new(TokenType::Colon, ":".to_string(), current_location.clone()));
                }
            }
            else if c == '\'' {
                let mut char = String::new();
                let mut escape = false;
                let mut length = 1;
                i += 1;
    
                while i < chars.len() {
                    if chars[i] == '\'' && !escape {
                        length += 1;
                        break;
                    }
                    else if chars[i] == '\\' && !escape {
                        escape = true;
                    }
                    else if escape {
                        match chars[i] {
                            'n' => char.push('\n'),
                            't' => char.push('\t'),
                            'r' => char.push('\r'),
                            '\'' => char.push('\''),
                            '\\' => char.push('\\'),
                            '0' => char.push('\0'),
                            _ => break
                        }
                        escape = false;
                        length += 1;
                    }
                    else {
                        char.push(chars[i]);
                        length += 1;
                    }
                    i += 1;
                }
                
                if char.len() != 1 {
                    self.error("Invalid escape sequence", "", &current_location);
                    i += 2;
                    continue;
                }
                
                current_location.advance(length);
                tokens.push(Token::new(TokenType::CharConstant, char, current_location.clone()));
            }
            else if c == '"' {
                let mut string = String::new();
                let mut escape = false;
                let mut length = 1;
                i += 1;
    
                while i < chars.len() {
                    if chars[i] == '"' && !escape {
                        length += 1;
                        break;
                    }
                    else if chars[i] == '\\' && !escape {
                        escape = true;
                    }
                    else if escape {
                        match chars[i] {
                            'n' => string.push('\n'),
                            't' => string.push('\t'),
                            'r' => string.push('\r'),
                            '\'' => string.push('\''),
                            '\\' => string.push('\\'),
                            '0' => string.push('\0'),
                            _ => {
                                self.error("Invalid escape sequence", "", &current_location);
                                break;
                            }
                        }
                        escape = false;
                        length += 1;
                    }
                    else {
                        length += 1;
                        string.push(chars[i]);
                    }
                    i += 1;
                }
    
                current_location.advance(length);            
                tokens.push(Token::new(TokenType::StringConstant, string, current_location.clone()));
            }
            else {
                self.error(format!("Unknown character: {}", c).as_str(), "", &current_location);
                continue;
            }
            i += 1;
            last_was_negatable_ability -= 1;
        }
    
        // Fix assignment operators, `+=` to `= NAME +`
        let mut i = 0;
        while i < tokens.len() {
            if tokens[i].token_type.operator_assignable() && tokens[i].value.ends_with("=") {
                tokens[i].value = tokens[i].value.trim_end_matches("=").to_string();
                tokens[i].location.length -= 1;
                
                let ident = tokens[i - 1].clone();
                let original = tokens[i].location.clone();
                let mut assign_location = original.clone();
                assign_location.length = 1;
                
                tokens.insert(i, Token::new(TokenType::Assign, "=".to_string(), assign_location.clone()));
                tokens.insert(i + 1, Token::new(ident.token_type.clone(), ident.value.clone(), ident.location.clone()));
                
                let new_op_location = Location {
                    line: assign_location.line,
                    column: assign_location.column + assign_location.length + 1,
                    length: assign_location.length,
                };
                if i > 0 {
                    tokens[i + 2].location.column = new_op_location.column;
                }
    
                i += 3;
            } else if tokens[i].token_type == TokenType::EndOfLine && tokens[i].value == "}" && tokens.get(i + 1).map_or(false, |t| t.token_type == TokenType::Else) {
                tokens.remove(i);
            } else {
                i += 1;
            }
        }

        tokens
    }
}