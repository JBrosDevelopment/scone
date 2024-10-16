use std::fmt::Debug;
use crate::lexer::Token;

pub type AnonymousTypeParameter = (String, Vec<Type>);
pub type Tuple = Vec<TupleNode>;
pub type EnumIndex = (String, i32);
pub type Type = (Option<String>, Option<Vec<Tuple>>);

#[derive(Clone, Debug)] pub enum AccessModifier { Private, Public, Internal, None }
#[derive(Clone, Debug)] pub enum VariableModifier { Const, Static, None }
#[derive(Clone, Debug)] pub enum StatementType { If, Else, ElseIf, While, For }


#[derive(Clone, Debug)]
pub struct ObjectPath {
    pub name: String,
    pub child: Option<Box<ObjectPath>>,
}

#[derive(Clone, Debug)]
pub struct InterfaceNode {
    pub path: ObjectPath,
    pub argument_types: Vec<Type>
}

#[derive(Clone, Debug)]
pub struct TupleNode {
    pub name: String,
    pub children: Option<Vec<Box<TupleNode>>>
}

pub trait ASTNode: Debug {
    fn get_meta(&self) -> String;
    fn clone_box(&self) -> Box<dyn ASTNode>; 
}
impl Clone for Box<dyn ASTNode> {
    fn clone(&self) -> Box<dyn ASTNode> {
        self.clone_box() 
    }
}

// --------------------------- Constant --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeConstant {
    pub token: Token,
}
impl ASTNode for ASTNodeConstant {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Constant --------------------------- //

// --------------------------- Expression --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeExpression {
    pub token: Token,
    pub lhs: Option<Box<dyn ASTNode>>,
    pub rhs: Option<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeExpression {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Expression --------------------------- //

// --------------------------- Function Call --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeFunctionCall {
    pub token: Token,
    pub argumments: Vec<Box<dyn ASTNode>>,
    pub path: ObjectPath,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
}
impl ASTNode for ASTNodeFunctionCall {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Function Call --------------------------- //

// --------------------------- Variable Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeVariableDeclaration {
    pub token: Token,
    pub name: String,
    pub value: Option<Box<dyn ASTNode>>,
    pub type_identifier: Type,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub variable_modifier: VariableModifier
}
impl ASTNode for ASTNodeVariableDeclaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Variable Declaration --------------------------- //

// --------------------------- Undefined Variable Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeUndefinedVariable {
    pub token: Token,
    pub name: String,
    pub type_identifier: Type,
}
impl ASTNode for ASTNodeUndefinedVariable {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Undefined Variable Declaration --------------------------- //

// --------------------------- Return Statement --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeReturnStatement {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Return Statement --------------------------- //

// --------------------------- Enum Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeEnumDelcaration {
    pub token: Token,
    pub name: String,
    pub values: Vec<EnumIndex>,
    pub description: Option<String>
}
impl ASTNode for ASTNodeEnumDelcaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Enum Declaration --------------------------- //

// --------------------------- Control --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeControl {
    pub token: Token
}
impl ASTNode for ASTNodeControl {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Control --------------------------- //

// --------------------------- Class Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeClassDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub interfaces: Vec<InterfaceNode>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeClassDeclaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Class Declaration --------------------------- //

// --------------------------- Struct Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeStructDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeStructDeclaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Struct Declaration --------------------------- //

// --------------------------- Interface Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeInterfaceDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub interfaces: Vec<InterfaceNode>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeInterfaceDeclaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Interface Declaration --------------------------- //

// --------------------------- Function Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeFunctionDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub arguments: Vec<ASTNodeUndefinedVariable>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub return_type: Option<Tuple>,
    pub type_identifier: Type,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeFunctionDeclaration {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Function Declaration --------------------------- //

// --------------------------- Statement --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeStatement {
    pub token: Token,
    pub statement_type: StatementType,
    pub argument: Option<Box<dyn ASTNode>>,
    pub body: Vec<Box<dyn ASTNode>>
}
impl ASTNode for ASTNodeStatement {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Statement --------------------------- //

// --------------------------- Object Creation --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeObjectCreation {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub properties: Vec<(String, Box<dyn ASTNode>)>,
}
impl ASTNode for ASTNodeObjectCreation {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Object Creation --------------------------- //

// --------------------------- Lambda Expression --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeLambdaExpression {
    pub token: Token,
    pub arguments: Vec<ASTNodeUndefinedVariable>,
    pub body: Vec<Box<dyn ASTNode>>
}
impl ASTNode for ASTNodeLambdaExpression {
    fn get_meta(&self) -> String { self.token.meta.clone() }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
}
// --------------------------- Lambda Expression --------------------------- //


