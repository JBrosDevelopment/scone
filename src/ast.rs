use serde::Serialize;
use crate::lexer::Token;

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum NodeType {
    None,

    // constants
    Constant(ConstantNode),
    Operator(Expression),

    // identifiers
    Identifier(Box<Token>),
    TypeIdentifier(ScopedType),
    AnonymousType(AnonymousType),

    // assignment
    Assignment(Assignment),

    // expression
    ScopedExpression(ScopedIdentifier),
    FunctionCall(FunctionCall),
    TupleExpression(NodeParameters),
    ReturnExpression(Box<ASTNode>),
    TernaryOperator(TernaryConditional),
    UnaryOperator(UnaryExpression),
    ArrayExpression(NodeParameters),
    Indexer(IndexingExpression),
    ObjectInstantiation(ObjectInstantiation),
    LambdaExpression(LambdaExpression),

    // flow
    If(ConditionalRegion),
    While(ConditionalRegion),
    For(ForLoop),
    Match(MatchRegion),

    // control
    Break(Box<Token>),
    Continue(Box<Token>),

    // other
    Use(ScopedIdentifier),
    LoadLib(LoadLib),
    AsCast(Expression),
    IsCheck(Expression),
    CodeBlock(BodyRegion),
    
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
    Object
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
impl AccessModifier {
    pub fn to_string(&self) -> String {
        match self {
            AccessModifier::None => "".to_string(),
            AccessModifier::Public => "pub".to_string(),
            AccessModifier::Private => "priv".to_string(),
            AccessModifier::Override => "override".to_string(),
            AccessModifier::Virtual => "virtual".to_string(),
            AccessModifier::Abstract => "abstract".to_string(),
            AccessModifier::Static => "static".to_string(),
            AccessModifier::Const => "const".to_string(),
            AccessModifier::Extern => "extern".to_string(),
        }
    }
    
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
pub struct NodeProperty {
    pub name: Box<Token>,
    pub value: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionCall {
    pub parameters: NodeParameters,
    pub type_parameters: Option<Vec<Box<ASTNode>>>,
    pub name: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ConstantNode {
    pub value: Box<Token>,
    pub constant_type: ConstantType,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub access_modifier: Vec<AccessModifier>,
    pub var_type: Box<ASTNode>,
    pub var_name: Box<Token>,
    pub var_value: Box<ASTNode>,
    pub description: Option<Box<Token>>,
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
pub enum ScopeType { Dot, DoubleColon }  // scope for type before, meaning: Scope::Into.dot -> Scope has none, Into has DoubleColon, dot has Dot
impl ScopeType {
    pub fn to_string(&self) -> String {
        match self {
            ScopeType::Dot => ".".to_string(),
            ScopeType::DoubleColon => "::".to_string(),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)] 
pub struct ScopedIdentifier { 
    pub scope: Vec<Identifier>, // index 0 is root, index n is in chain or scoped
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Identifier {
    pub expression: Box<ASTNode>,
    pub scope_type: Option<ScopeType>, 
    pub type_parameters: Option<NodeParameters>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ConditionalRegion {
    pub condition: Box<ASTNode>,
    pub body: BodyRegion,
    pub else_if_regions: Option<Vec<Box<ConditionalRegion>>>,
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
    pub name: Box<Token>,
    pub scope_type: Option<ScopeType>,
    pub type_parameters: Option<NodeParameters>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ScopedType {
    pub scope: Vec<TypeIdentifier>,
    pub is_ptr_or_ref: Vec<TypeSuffix>,
    pub is_array: bool,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum TypeSuffix {
    Ptr,
    Ref
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct UnaryExpression {
    pub operand: Box<ASTNode>,
    pub operator: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ObjectInstantiation {
    pub object_type: Box<ASTNode>,
    pub properties: Vec<NodeProperty>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TernaryConditional {
    pub condition: Box<ASTNode>,
    pub then: Box<ASTNode>,
    pub else_then: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct IndexingExpression {
    pub object: Box<ASTNode>,
    pub index: Vec<Box<ASTNode>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct LambdaExpression {
    pub parameters: NodeParameters,
    pub body: BodyRegion,
}