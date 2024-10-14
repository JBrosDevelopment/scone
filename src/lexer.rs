use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub enum TokenType {
    // operators
    Plus,
    Minus,
    Star,
    Slash,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    BitwiseNot,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    Modulus,
    Increment,
    Decrement,
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
    LeftArrow,
    DoubleArrow,

    // keywords
    Return,
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
    Interface,
    In,

    // constants
    BoolConstant,
    StringConstant,
    CharConstant,
    NumberConstant,

    // other
    Identifier,
    EndOfLine,
}

#[derive(Clone, Debug, Serialize)]
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

#[derive(Clone, Debug, Serialize)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub meta: String,
    pub location: Location
}
impl Token {
    pub fn new(token_type: TokenType, value: String, meta: String, location: Location) -> Token {
        Token {
            token_type,
            value,
            meta,
            location
        }
    }
}

pub fn lex(code: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = code.chars().peekable();
    let mut current_location = Location::new(1, 1, 0);

    while let Some(c) = chars.peek() {
        if *c == ' ' {
            current_location.advance(1);
            chars.next();
        }
        else if *c == '\t' {
            current_location.advance('\t'.to_string().len() as i32);
            chars.next();
        }
        else if *c == '\r' {
            chars.next();
        }
        else if *c == '\n' {
            current_location.advance('\n'.to_string().len() as i32);
            tokens.push(Token::new(TokenType::EndOfLine, "\n".to_string(), String::new(), current_location.clone()));
            current_location.advance_line();
            chars.next();
        }
        else if *c == ';' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::EndOfLine, ";".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if char::is_alphabetic(c.clone()) {
            let mut name = String::new();
    
            while let Some(next_c) = chars.peek() {
                if char::is_alphanumeric(*next_c) || char::is_numeric(*next_c) || *next_c == '_' {
                    name.push(*next_c);
                    chars.next();
                } else {
                    break;
                }
            }
            current_location.advance(name.len() as i32);

            let new_token = match name.as_str() {
                "true" | "false" => Token::new(TokenType::BoolConstant, name.clone(), String::new(), current_location.clone()),
                "return" => Token::new(TokenType::Return, name.clone(), String::new(), current_location.clone()),
                "if" => Token::new(TokenType::If, name.clone(), String::new(), current_location.clone()),
                "else" => Token::new(TokenType::Else, name.clone(), String::new(), current_location.clone()),
                "for" => Token::new(TokenType::For, name.clone(), String::new(), current_location.clone()),
                "while" => Token::new(TokenType::While, name.clone(), String::new(), current_location.clone()),
                "break" => Token::new(TokenType::Break, name.clone(), String::new(), current_location.clone()),
                "continue" => Token::new(TokenType::Continue, name.clone(), String::new(), current_location.clone()),
                "class" => Token::new(TokenType::Class, name.clone(), String::new(), current_location.clone()),
                "struct" => Token::new(TokenType::Struct, name.clone(), String::new(), current_location.clone()),
                "enum" => Token::new(TokenType::Enum, name.clone(), String::new(), current_location.clone()),
                "use" => Token::new(TokenType::Use, name.clone(), String::new(), current_location.clone()),
                "interface" => Token::new(TokenType::Interface, name.clone(), String::new(), current_location.clone()),
                "in" => Token::new(TokenType::In, name.clone(), String::new(), current_location.clone()),
                _ => Token::new(TokenType::Identifier, name.clone(), String::new(), current_location.clone()),
            };
            tokens.push(new_token);
        }
        else if char::is_numeric(c.clone()) {
            let mut number = String::new();
    
            while let Some(next_c) = chars.peek() {
                if char::is_numeric(*next_c) {
                    number.push(*next_c);
                    chars.next();
                } else {
                    break;
                }
            }
            current_location.advance(number.len() as i32);
            let new_token = Token::new(TokenType::NumberConstant, number, String::new(), current_location.clone());
            tokens.push(new_token);
        }
        else if *c == ',' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::Comma, ",".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '.' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::Dot, ".".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '{' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::LBrace, "{".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '}' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::RBrace, "}".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '(' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::LParen, "(".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == ')' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::RParen, ")".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '[' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::LBracket, "[".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == ']' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::RBracket, "]".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '$' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::DollarSign, "$".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '~' {
            current_location.advance(1);
            tokens.push(Token::new(TokenType::BitwiseNot, "~".to_string(), String::new(), current_location.clone()));
            chars.next();
        }
        else if *c == '^' {
            chars.next();
            if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::BitwiseXor, "^=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::BitwiseXor, "^".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '*' {
            chars.next();
            if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Star, "*=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Star, "*".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '%' {
            chars.next();
            if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Modulus, "%=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Modulus, "%".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '/' {
            chars.next();
            if chars.peek() == Some(&'/') {
                chars.next();
                
                while let Some(next_c) = chars.peek() {
                    if *next_c == '\n' {
                        break;
                    }
                    chars.next();
                }
            }
            if chars.peek() == Some(&'*') {
                chars.next();
                let mut is_star = false;
                let mut lines = 0;
                let mut length = 0;

                while let Some(next_c) = chars.peek() {
                    if *next_c == '*' {
                        is_star = true;
                    }
                    else if *next_c == '/' && is_star {
                        chars.next();
                        break;
                    }

                    if *next_c == '\n' {
                        lines += 1;
                        length = 0;
                    }
                    if *next_c != '\r' {
                        length += 1;
                    }

                    chars.next();
                }

                for _ in 0..lines {
                    current_location.advance_line();
                }

                current_location.advance(length as i32);
            }
            else if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Slash, "/=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Slash, "/".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '=' {
            chars.next();
            let next = chars.peek();
            if next == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Equal, "==".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if next == Some(&'>') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::DoubleArrow, "=>".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Assign, "=".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '+' {
            chars.next();
            if chars.peek() == Some(&'+') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Increment, "++".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Plus, "+=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Plus, "+".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '-' {
            chars.next();
            let next = chars.peek();
            if next == Some(&'-') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Decrement, "--".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if next == Some(&'>') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::RightArrow, "->".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::Minus, "-=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Minus, "-".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '>' {
            chars.next();
            let next = chars.peek();
            if next == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::GreaterThanOrEqual, ">=".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if next == Some(&'>') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::BitwiseShiftRight, ">>".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::GreaterThan, ">".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '<' {
            chars.next();
            let next = chars.peek();
            if next == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::LessThanOrEqual, "<=".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if next == Some(&'<') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::BitwiseShiftLeft, "<<".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else if next == Some(&'-') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::LeftArrow, "<-".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::LessThan, "<".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
        }
        else if *c == '!' {
            chars.next();
            if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::NotEqual, "!=".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Not, "!".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '&' {
            chars.next();
            if chars.peek() == Some(&'&') {
                chars.next();
                if chars.peek() == Some(&'=') {
                    current_location.advance(3);    
                    tokens.push(Token::new(TokenType::And, "&&=".to_string(), "Assign".to_string(), current_location.clone()));
                    chars.next();
                }
                else {
                    tokens.push(Token::new(TokenType::And, "&&".to_string(), String::new(), current_location.clone()));
                    current_location.advance(2);
                }
            }
            if chars.peek() == Some(&'=') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::BitwiseAnd, "&=".to_string(), "Assign".to_string(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::BitwiseAnd, "&".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '|' {
            chars.next();
            if chars.peek() == Some(&'|') {
                chars.next();
                if chars.peek() == Some(&'=') {
                    current_location.advance(3);    
                    tokens.push(Token::new(TokenType::Or, "||=".to_string(), "Assign".to_string(), current_location.clone()));
                    chars.next();
                }
                else {
                    tokens.push(Token::new(TokenType::Or, "||".to_string(), String::new(), current_location.clone()));
                    current_location.advance(2);
                }
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::BitwiseOr, "|".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == ':' {
            chars.next();
            if chars.peek() == Some(&':') {
                current_location.advance(2);
                tokens.push(Token::new(TokenType::DoubleColon, "::".to_string(), String::new(), current_location.clone()));
                chars.next();
            }
            else {
                current_location.advance(1);
                tokens.push(Token::new(TokenType::Colon, ":".to_string(), String::new(), current_location.clone()));
            }
        }
        else if *c == '\'' {
            let mut char = String::new();
            let mut escape = false;
            let mut length = 1;
            chars.next();

            while let Some(next_c) = chars.next() {
                if next_c == '\'' && !escape {
                    length += 1;
                    break;
                }
                else if next_c == '\\' && !escape {
                    escape = true;
                }
                else if escape {
                    match next_c {
                        'n' => char.push('\n'),
                        't' => char.push('\t'),
                        'r' => char.push('\r'),
                        '\'' => char.push('\''),
                        '\\' => char.push('\\'),
                        '0' => char.push('\0'),
                        _ => return Err(format!("Invalid escape sequence at: {:?}", current_location.clone()))
                    }
                    escape = false;
                    length += 1;
                }
                else {
                    char.push(next_c);
                    length += 1;
                }
            }
            
            if char.len() != 1 {
                return Err(format!("Invalid character constant at: {:?}", current_location.clone()));
            }
            
            current_location.advance(length);
            tokens.push(Token::new(TokenType::CharConstant, char, String::new(), current_location.clone()));
        }
        else if *c == '"' {
            let mut string = String::new();
            let mut escape = false;
            let mut length = 1;
            chars.next();

            while let Some(next_c) = chars.next() {
                if next_c == '"' && !escape {
                    length += 1;
                    break;
                }
                else if next_c == '\\' && !escape {
                    escape = true;
                }
                else if escape {
                    match next_c {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '\'' => string.push('\''),
                        '\\' => string.push('\\'),
                        '0' => string.push('\0'),
                        _ => return Err(format!("Invalid escape sequence at: {:?}", current_location.clone()))
                    }
                    escape = false;
                    length += 1;
                }
                else {
                    length += 1;
                    string.push(next_c);
                }

            }

            current_location.advance(length);            
            tokens.push(Token::new(TokenType::StringConstant, string, String::new(), current_location.clone()));
        }
        else {
            return Err(format!("Unkown character: '{}' at: {:?}", c ,current_location.clone()));
        }
    }

    Ok(tokens)
}
