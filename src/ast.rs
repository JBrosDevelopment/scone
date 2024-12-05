use serde::Serialize;
use crate::lexer::Token;

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum NodeType {
    None,

    // constants
    Constant(ConstantNode),
    Operator(Expression),

    // identifiers
    Identifier(ScopeToIdentifier),
    TypeIdentifier(TypeIdentifier),
    AnonymousType(AnonymousType),

    // assignment
    Assignment(Assignment),

    // expression
    FunctionCall(FunctionCall),
    TupleExpression(NodeParameters),
    ArrayExpression(NodeParameters),
    ReturnExpression(Box<ASTNode>),
    TernaryOperator(ConditionalRegion),
    UnaryOperator(UnaryExpression),
    ObjectInstantiation(ObjectInstantiation),

    // flow
    If(ConditionalRegion),
    ElseIf(ConditionalRegion),
    Else(BodyRegion),
    While(ConditionalRegion),
    For(ForLoop),
    Match(MatchRegion),

    // control
    Break(Box<Token>),
    Continue(Box<Token>),

    // other
    Use(ScopeToIdentifier),
    LoadLib(LoadLib),
    AsCast(Expression),
    IsCheck(Expression),
    
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
    Abstract,
    Static,
    Const,
    Extern
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ASTNode {
    pub token: Box<Token>,
    pub node: Box<NodeType>,
}
impl ASTNode {
    pub fn none() -> ASTNode {
        ASTNode {
            token: Box::new(Token::new_empty()),
            node: Box::new(NodeType::None),
        }
    }
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
pub struct NodeProperties {
    pub parameters: Vec<Box<ASTNode>>,
    pub values: Vec<Box<ASTNode>>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionCall {
    pub parameters: NodeParameters,
    pub type_parameters: Option<Vec<Box<ASTNode>>>,
    pub scope: ScopeToIdentifier,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ConstantNode {
    pub value: Box<Token>,
    pub constant_type: ConstantType,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub var_type: Box<ASTNode>,
    pub var_name: Box<Token>,
    pub var_value: Box<ASTNode>,
    pub description: Option<Box<Token>>,
    pub access_modifier: Vec<AccessModifier>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub return_type: Box<NodeType>,
    pub parameters: DefinedNodeParameters,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub body: Option<BodyRegion>,
    pub description: Option<Box<Token>>,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ClassDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Box<Token>>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct StructDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Box<Token>>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct InterfaceDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub description: Option<Box<Token>>,
    pub body: BodyRegion,
    pub access_modifier: Option<Vec<AccessModifier>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Box<Token>,
    pub description: Option<Box<Token>>,
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
pub enum ScopeType { Dot, DoubleColon }
impl ScopeType {
    pub fn to_string(&self) -> String {
        match self {
            ScopeType::Dot => ".".to_string(),
            ScopeType::DoubleColon => "::".to_string(),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ScopeToIdentifier {
    pub child: Option<Box<ScopeToIdentifier>>,
    pub identifier: Box<Token>,
    pub scope_type: Option<ScopeType> // scope for type before, meaning: Scope::Into.dot -> Scope has none, Into has DoubleColon, dot has Dot
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
    pub description: Option<Box<Token>>,
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

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Expression {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
    pub operator: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TypeIdentifier {
    pub scope: ScopeToIdentifier,
    pub types: Option<NodeParameters>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct UnaryExpression {
    pub operand: Box<ASTNode>,
    pub operator: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ObjectInstantiation {
    pub object_type: Box<ASTNode>,
    pub properties: NodeProperties,
}