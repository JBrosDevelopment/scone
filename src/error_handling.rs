use crate::lexer::Location;

#[derive(Debug, Clone)]
pub struct ErrorHandling {
    pub messages: Vec<Message>,
    pub file: Option<String>,
    pub full_code: String,
} 
impl ErrorHandling {
    pub fn new(file: Option<String>, full_code: String) -> ErrorHandling { ErrorHandling { messages: Vec::new(), file, full_code } }

    pub fn error(&mut self, title: &str, message: &str, help: &str, location: &Location) {
        let message = Self::output(title, MessageType::Error, message, help, location, &self.full_code, &self.file);
        self.messages.push(message);
    }

    pub fn warning(&mut self, title: &str, message: &str, help: &str, location: &Location) {
        let message = Self::output(title, MessageType::Warning, message, help, location, &self.full_code, &self.file);
        self.messages.push(message);
    }

    pub fn message(&mut self, title: &str, message: &str, help: &str, location: &Location) {
        let message = Self::output(title, MessageType::Message, message, help, location, &self.full_code, &self.file);
        self.messages.push(message);
    }

    pub fn has_errors(&self) -> bool { 
        self.errors().len() > 0 
    }

    pub fn output(message_title: &str, message_type: MessageType, message: &str, help: &str, location: &Location, code: &String, file: &Option<String>) -> Message {
        let message_color = match message_type {
            MessageType::Error => (200, 50, 50),
            MessageType::Warning => (225, 175, 75),
            MessageType::Message => (160, 160, 180),
        };
        
        let error_colored = Self::colored_text(message_color.0, message_color.1, message_color.2, message_title, true);
        let message_colored = Self::colored_text(255, 255, 255, message, true);
        let line_column = format!("{}:{}", location.line, location.column);
        let blue_arrow = Self::colored_text(175, 175, 250, "  -->", true);
        
        let file_info = file.as_deref().unwrap_or("");
        let formatted_message = format!("{}: {} \n{} {}:{}\n", error_colored, message_colored, blue_arrow, file_info, line_column);
        
        let line_with_issue = code.lines().nth((location.line - 1) as usize).unwrap_or("");
        
        let pipe_colored = Self::colored_text(175, 175, 250, "|", true);
        let line_number_colored = Self::colored_text(175, 175, 250, &location.line.to_string(), true);
        
        let mut line_marker = "  ".to_string();
        let code_line = format!("{} {} {}\n", line_number_colored, pipe_colored, line_with_issue);
        
        let mut pointer_line = format!("  {}", pipe_colored);
        let mut pointer_position = " ".repeat(location.column as usize);
        pointer_position.push_str(&Self::colored_text(200, 200, 100, &"^".repeat(location.length as usize), true));
        pointer_position.push_str(&Self::colored_text(200, 200, 100, help, true));
        
        let mut padding_left = location.line;
        let mut left = 9;
        while padding_left > left {
            padding_left -= left;
            left *= 10;
            pointer_line.insert_str(0, " ");
            line_marker.insert_str(0, " ");
        }
        line_marker = format!("{}{}\n", line_marker, pipe_colored);
                
        let pointer_line = format!("{}{}\n", pointer_line, pointer_position);
        
        let output = format!("{formatted_message}{line_marker}{code_line}{pointer_line}");
        
        Message {
            output,
            message_type,
            message: message.to_string(),
            help: help.to_string(),
            location: location.clone(),
            code: line_with_issue.to_string(),
            file: file.clone(),
        }
    }

    pub fn messages(&self) -> Vec<&Message> { 
        self.messages.iter().filter(|m| m.message_type == MessageType::Message).collect()
    }
    pub fn warnings(&self) -> Vec<&Message> { 
        self.messages.iter().filter(|m| m.message_type == MessageType::Warning).collect()
    }
    pub fn errors(&self) -> Vec<&Message> { 
        self.messages.iter().filter(|m| m.message_type == MessageType::Error).collect()
    }
    
    pub fn colored_text(r: i32, g: i32, b: i32, text: &str, bold: bool) -> String {
        let bold_code = if bold { "\x1B[1m" } else { "" };
        format!("{}{}\x1B[38;2;{};{};{}m{}\x1B[0m", bold_code, "\x1B[38;2", r, g, b, text)
    }
    pub fn print_messages(&self) {
        if self.messages.len() == 0 {
            return;
        }

        for message in self.messages().iter() {
            println!("{}", message.output);
        }
        for warning in self.warnings().iter() {
            println!("{}", warning.output);
        }
        for error in self.errors().iter() {
            println!("{}", error.output);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Message {
    pub output: String,
    pub message_type: MessageType,
    pub message: String,
    pub help: String,
    pub location: Location,
    pub code: String,
    pub file: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageType {
    Error,
    Warning,
    Message
}

#[cfg(debug_assertions)]
pub const DEBUGGING: bool = true;

#[cfg(not(debug_assertions))]
pub const DEBUGGING: bool = false;

#[macro_export]
macro_rules! debug {
    ($obj:expr) => {
        if DEBUGGING {
            println!("DEBUG: TRUE     FILE: {}:{}     OUT: {:#?}", file!(), line!(), $obj);
        }
    };
    ($msg:expr, $obj:expr) => {
        if DEBUGGING {
            println!("DEBUG: TRUE     FILE: {}:{}     MESSAGE: {}     OUT: {:#?}", file!(), line!(), $msg, $obj);
        }
    };
}