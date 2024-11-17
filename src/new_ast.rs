use serde::Serialize;
use crate::lexer::Token;

//    if let Some(NodeType::FunctionCall(_)) = a.node_type.as_deref() {
//        println!("It's a FunctionCall!");
//    }

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum NodeType {
    // constants
    Constant(ConstantNode),
    Operator(Box<ASTNode>),

    // identifiers
    Identifier(Box<ASTNode>),
    TypeIdentifier(ScopeToIdentifier),
    AnonymousType(AnonymousType),

    // assignment
    Assignment(Assignment),

    // expression
    FunctionCall(FunctionCall),
    TupleExpression(NodeParameters),
    ArrayExpression(NodeParameters),
    ReturnExpression(Box<ASTNode>),

    // flow
    If(ConditionalRegion),
    ElseIf(ConditionalRegion),
    Else(BodyRegion),
    While(ConditionalRegion),
    For(ForLoop),
    Match(MatchRegion),

    // control
    Break(Box<ASTNode>),
    Continue(Box<ASTNode>),

    // other
    Use(ScopeToIdentifier),
    LoadLib(LoadLib),
    AsCast(Box<ASTNode>),
    IsCheck(Box<ASTNode>),
    
    // declare
    TupleDeclaration(NodeParameters),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    StructDeclaration(StructDeclaration),
    InterfaceDeclaration(InterfaceDeclaration),
    EnumDeclaration(EnumDeclaration),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum ConstantType {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    String,
    Char,
    Bool,
    Array(Box<ConstantType>),
    Range(Box<ConstantType>, Box<ConstantType>),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum AccessModifier {
    None,
    Public,
    Private,
    Override,
    Virtual,
    Static,
    Const,
    Extern
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ASTNode {
    pub token: Box<Token>,
    pub children: Vec<Box<ASTNode>>,
    pub node: Box<NodeType>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct NodeParameters {
    pub parameters: Vec<Box<ASTNode>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct DefinedNodeParameters {
    pub types: Vec<Box<ASTNode>>,
    pub names: Vec<Box<ASTNode>>,
    pub values: Vec<Option<Box<ASTNode>>>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionCall {
    pub name: Box<Token>,
    pub parameters: NodeParameters,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub return_type: Box<NodeType>,
    pub scope: Option<ScopeToIdentifier>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ConstantNode {
    pub value: Box<Token>,
    pub constant_type: ConstantType,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub var_type: Box<NodeType>,
    pub var_name: Box<Token>,
    pub var_value: Box<ASTNode>,
    pub description: Option<Token>,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub return_type: Box<NodeType>,
    pub parameters: DefinedNodeParameters,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub body: Option<BodyRegion>,
    pub description: Option<Token>,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ClassDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Token>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct StructDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Token>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct InterfaceDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Token>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Box<Token>,
    pub description: Option<Token>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct AnonymousType {
    pub name: Box<Token>,
    pub constraints: Option<ASTNode>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct AnonymousTypeParameters {
    pub parameters: Vec<AnonymousType>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Assignment {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ScopeToIdentifier {
    pub child: Option<Box<ScopeToIdentifier>>,
    pub identifier: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ConditionalRegion {
    pub condition: Box<ASTNode>,
    pub body: BodyRegion,
    pub else_conditional_regions: Option<Vec<Box<ConditionalRegion>>>,
    pub else_region: Option<BodyRegion>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct BodyRegion {
    pub body: Vec<Box<ASTNode>>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ForLoop {
    pub iter_value: Box<ASTNode>,
    pub iter_range: Box<ASTNode>,
    pub body: BodyRegion,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct LoadLib {
    pub alias: Box<Token>,
    pub path: Box<Token>,
    pub description: Option<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct MatchRegion {
    pub match_value: Box<ASTNode>,
    pub match_cases: Vec<MatchCase>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct MatchCase {
    pub pattern: Box<ASTNode>,
    pub body: BodyRegion,
}