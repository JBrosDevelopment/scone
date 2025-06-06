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
    ForEach(ForEachLoop),
    For(ForLoop),
    Match(MatchRegion),

    // control
    Break(Box<Token>),
    Continue(Box<Token>),

    // other
    Use(Box<Token>),
    AsCast(Expression),
    IsCheck(Expression),
    CodeBlock(BodyRegion),
    Discard(Box<Token>),
    Shebang(ShebangType),
    
    // declare
    TypeDefinition(TypeDefinition),
    TupleDeclaration(TupleDeclaration),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    StructDeclaration(StructDeclaration),
    TraitDeclaration(TraitDeclaration),
    EnumDeclaration(EnumDeclaration),
    TypeDef(TypeDefDeclaration),
}

impl NodeType {
    pub fn to_string(&self) -> String {
        match self {
            NodeType::None => "None".to_string(),
            NodeType::Constant(_) => "Constant".to_string(),
            NodeType::Operator(_) => "Operator".to_string(),
            NodeType::Identifier(_) => "Identifier".to_string(),
            NodeType::TypeIdentifier(_) => "TypeIdentifier".to_string(),
            NodeType::AnonymousType(_) => "AnonymousType".to_string(),
            NodeType::Assignment(_) => "Assignment".to_string(),
            NodeType::ScopedExpression(_) => "ScopedExpression".to_string(),
            NodeType::FunctionCall(_) => "FunctionCall".to_string(),
            NodeType::TupleExpression(_) => "TupleExpression".to_string(),
            NodeType::ReturnExpression(_) => "ReturnExpression".to_string(),
            NodeType::TernaryOperator(_) => "TernaryOperator".to_string(),
            NodeType::UnaryOperator(_) => "UnaryOperator".to_string(),
            NodeType::ArrayExpression(_) => "ArrayExpression".to_string(),
            NodeType::Indexer(_) => "Indexer".to_string(),
            NodeType::ObjectInstantiation(_) => "ObjectInstantiation".to_string(),
            NodeType::LambdaExpression(_) => "LambdaExpression".to_string(),
            NodeType::If(_) => "If".to_string(),
            NodeType::While(_) => "While".to_string(),
            NodeType::ForEach(_) => "ForEach".to_string(),
            NodeType::For(_) => "For".to_string(),
            NodeType::Match(_) => "Match".to_string(),
            NodeType::Break(_) => "Break".to_string(),
            NodeType::Continue(_) => "Continue".to_string(),
            NodeType::Use(_) => "Use".to_string(),
            NodeType::AsCast(_) => "AsCast".to_string(),
            NodeType::IsCheck(_) => "IsCheck".to_string(),
            NodeType::CodeBlock(_) => "CodeBlock".to_string(),
            NodeType::Discard(_) => "Discard".to_string(),
            NodeType::Shebang(_) => "Shebang".to_string(),
            NodeType::TypeDefinition(_) => "TypeDefinition".to_string(),
            NodeType::TupleDeclaration(_) => "TupleDeclaration".to_string(),
            NodeType::VariableDeclaration(_) => "VariableDeclaration".to_string(),
            NodeType::FunctionDeclaration(_) => "FunctionDeclaration".to_string(),
            NodeType::ClassDeclaration(_) => "ClassDeclaration".to_string(),
            NodeType::StructDeclaration(_) => "StructDeclaration".to_string(),
            NodeType::TraitDeclaration(_) => "TraitDeclaration".to_string(),
            NodeType::EnumDeclaration(_) => "EnumDeclaration".to_string(),
            NodeType::TypeDef(_) => "TypeDef".to_string(),
        }
    }
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
    Object,
    None
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
    Extern,
    Safe,
    Unsafe
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
            AccessModifier::Safe => "safe".to_string(),
            AccessModifier::Unsafe => "unsafe".to_string(),
        }
    }
    
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ASTNode {
    pub token: Box<Token>,
    pub node: Box<NodeType>,
}
impl ASTNode {
    pub fn err() -> ASTNode {
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
pub struct DefinedNodeParameter {
    pub ty: Box<ASTNode>,
    pub name: Box<Token>,
    pub default_value: Option<Box<ASTNode>>,
    pub params: bool
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
    pub var_value: Option<Box<ASTNode>>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub name: Box<Token>,
    pub return_type: Box<ASTNode>,
    pub parameters: Vec<DefinedNodeParameter>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub body: Option<BodyRegion>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ClassDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub body: BodyRegion,
    pub access_modifier: Vec<AccessModifier>,
    pub extends: Vec<Box<Token>>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct StructDeclaration {
    pub name: Box<Token>,
    pub type_parameters: Option<AnonymousTypeParameters>,
    pub body: BodyRegion,
    pub access_modifier: Vec<AccessModifier>,
    pub extends: Vec<Box<Token>>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TraitDeclaration {
    pub name: Box<Token>,
    pub extends: Vec<Box<Token>>,
    pub body: BodyRegion,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Box<Token>,
    pub access_modifier: Vec<AccessModifier>,
    pub body: Vec<(Box<Token>, Option<Box<ASTNode>>)>,
    pub tags: Vec<Vec<Box<Token>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct AnonymousType {
    pub name: Box<Token>,
    pub constraints: Option<Box<ASTNode>>
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
    pub is_while: bool
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct BodyRegion {
    pub body: Vec<Box<ASTNode>>
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ForLoop {
    pub index_segment: Option<Box<Token>>,
    pub set_segment: Box<ASTNode>,
    pub condition_segment: Box<ASTNode>,
    pub increment_segment: Box<ASTNode>,
    pub body: BodyRegion,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ForEachLoop {
    pub index_segment: Option<Box<Token>>,
    pub iter_value: Box<ASTNode>,
    pub iter_range: Box<ASTNode>,
    pub body: BodyRegion,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct MatchRegion {
    pub match_value: Box<ASTNode>,
    pub match_cases: Vec<MatchCase>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct MatchCase {
    pub pattern: Box<ASTNode>,
    pub body: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Expression {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
    pub operator: Box<Token>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TupleDeclaration {
    pub parameters: NodeParameters,
    pub is_array: Vec<Vec<Box<ASTNode>>>,
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
    pub is_ptr_or_ref: Vec<TypeMemoryModifier>,
    pub is_array: Vec<Vec<Box<ASTNode>>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum TypeMemoryModifier {
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

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TypeDefinition {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TypeDefDeclaration {
    pub name: Box<Token>,
    pub type_definition: Box<ASTNode>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum ShebangType {
    Allow(ShebangAWEMessage),
    Warning(ShebangAWEMessage),
    Err(ShebangAWEMessage),
    Insert(Box<Token>),
    C(Vec<Box<Token>>),
    Pragma(Vec<Box<Token>>),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum ShebangAWEMessage {
    Unused, 
    Unreachable,
    Unimplemented,
    Deprecated,
    NoEntrance,
}

impl ShebangAWEMessage {
    pub fn to_string(&self) -> String {
        match self {
            ShebangAWEMessage::Unused => "unused".to_string(),
            ShebangAWEMessage::Unreachable => "unreachable".to_string(),
            ShebangAWEMessage::Unimplemented => "unimplemented".to_string(),
            ShebangAWEMessage::Deprecated => "deprecated".to_string(),
            ShebangAWEMessage::NoEntrance => "no_entrance".to_string(),
        }
    }
}