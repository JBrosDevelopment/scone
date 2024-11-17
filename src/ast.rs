use std::any::Any;
use serde::Serialize;

use crate::lexer::Token;

pub type Tuple = Vec<AnonymousTypeParameter>;
pub type EnumIndex = (String, i32);
pub type Type = (Option<AnonymousTypeParameter>, Option<Vec<Tuple>>);

#[derive(Clone, Debug, Serialize, Copy)] pub enum AccessModifier { Private, Public, Internal, None }
#[derive(Clone, Debug, Serialize, Copy)] pub enum VariableModifier { Const, Static, None }
#[derive(Clone, Debug, Serialize, Copy)] pub enum StatementType { If, Else, ElseIf, While, For }


#[derive(Clone, Debug, Serialize)]
pub struct AnonymousTypeParameter {
    pub type_simple: Option<Token>,
    pub type_complex: Option<Vec<Type>>
}

#[derive(Clone, Debug, Serialize)]
pub struct ObjectPath {
    pub name: String,
    pub child: Option<Box<ObjectPath>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct InterfaceNode {
    pub path: ObjectPath,
    pub argument_types: Vec<Type>
}

pub trait ASTNode: std::fmt::Debug {
    fn clone_box(&self) -> Box<dyn ASTNode>; 
    fn get_data(&self) -> ASTNodeALLVALUE;
    fn as_any(&self) -> &dyn Any;
}
impl Clone for Box<dyn ASTNode> {
    fn clone(&self) -> Box<dyn ASTNode> {
        self.clone_box() 
    }
}
pub trait IntoNode: std::fmt::Debug {
    fn into_constant(self) -> ASTNodeConstant;
    fn into_expression(self) -> ASTNodeExpression;
    fn into_function_call(self) -> ASTNodeFunctionCall;
    fn into_variable_declaration(self) -> ASTNodeVariableDeclaration;
    fn into_undefined_variable(self) -> ASTNodeUndefinedVariable;
    fn into_return_statement(self) -> ASTNodeReturnStatement;
    fn into_enum_declaration(self) -> ASTNodeEnumDeclaration;
    fn into_control(self) -> ASTNodeControl;
    fn into_class_declaration(self) -> ASTNodeClassDeclaration;
    fn into_struct_declaration(self) -> ASTNodeStructDeclaration;
    fn into_interface_declaration(self) -> ASTNodeInterfaceDeclaration;
    fn into_function_declaration(self) -> ASTNodeFunctionDeclaration;
    fn into_statement(self) -> ASTNodeStatement;
    fn into_object_creation(self) -> ASTNodeObjectCreation;
    fn into_lambda_expression(self) -> ASTNodeLambdaExpression;
    fn into_tuple_expression(self) -> ASTNodeTupleExpression;
    fn into_type_identifier(self) -> ASTNodeTypeIdentifier;
}

impl IntoNode for Box<dyn ASTNode> {
    fn into_constant(self) -> ASTNodeConstant { 
        ASTNodeConstant { token: self.get_data().token.unwrap() } 
    }
    fn into_expression(self) -> ASTNodeExpression {
        ASTNodeExpression { token: self.get_data().token.unwrap(), lhs: self.get_data().lhs, rhs: self.get_data().rhs }
    }
    fn into_function_call(self) -> ASTNodeFunctionCall {
        ASTNodeFunctionCall { token: self.get_data().token.unwrap(), argumments: self.get_data().argumments.unwrap(), path: self.get_data().path.unwrap(), type_parameters: self.get_data().type_parameters }
    }
    fn into_variable_declaration(self) -> ASTNodeVariableDeclaration {
        ASTNodeVariableDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), value: self.get_data().value, type_identifier: self.get_data().type_identifier.unwrap(), access_modifier: self.get_data().access_modifier.unwrap(), variable_modifier: self.get_data().variable_modifier.unwrap(), description: self.get_data().description }
    }
    fn into_undefined_variable(self) -> ASTNodeUndefinedVariable {
        ASTNodeUndefinedVariable { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), type_identifier: self.get_data().type_identifier.unwrap() }
    }
    fn into_return_statement(self) -> ASTNodeReturnStatement {
        ASTNodeReturnStatement { token: self.get_data().token.unwrap(), value: self.get_data().value }
    }
    fn into_enum_declaration(self) -> ASTNodeEnumDeclaration {
        ASTNodeEnumDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), values: self.get_data().values.unwrap(), description: self.get_data().description }
    }
    fn into_control(self) -> ASTNodeControl {
        ASTNodeControl { token: self.get_data().token.unwrap() }
    }
    fn into_class_declaration(self) -> ASTNodeClassDeclaration {
        ASTNodeClassDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), body: self.get_data().body.unwrap(), type_parameters: self.get_data().type_parameters, interfaces: self.get_data().interfaces.unwrap(), description: self.get_data().description, access_modifier: self.get_data().access_modifier.unwrap() }
    }
    fn into_struct_declaration(self) -> ASTNodeStructDeclaration {
        ASTNodeStructDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), body: self.get_data().body.unwrap(), type_parameters: self.get_data().type_parameters, description: self.get_data().description, access_modifier: self.get_data().access_modifier.unwrap() }
    }
    fn into_interface_declaration(self) -> ASTNodeInterfaceDeclaration {
        ASTNodeInterfaceDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), body: self.get_data().body.unwrap(), description: self.get_data().description, access_modifier: self.get_data().access_modifier.unwrap(), interfaces: self.get_data().interfaces.unwrap(), type_anonymous_parameters: self.get_data().type_anonymous_parameters }
    }
    fn into_function_declaration(self) -> ASTNodeFunctionDeclaration {
        ASTNodeFunctionDeclaration { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), type_parameters: self.get_data().type_parameters, parameters: self.get_data().parameters.unwrap(), description: self.get_data().description, access_modifier: self.get_data().access_modifier.unwrap(), type_identifier: self.get_data().type_identifier.unwrap(), body: self.get_data().body }
    }
    fn into_statement(self) -> ASTNodeStatement {
        ASTNodeStatement { token: self.get_data().token.unwrap(), body: self.get_data().body.unwrap(), statement_type: self.get_data().statement_type.unwrap(), argument: self.get_data().argument }
    }
    fn into_object_creation(self) -> ASTNodeObjectCreation {
        ASTNodeObjectCreation { token: self.get_data().token.unwrap(), name: self.get_data().name.unwrap(), type_parameters: self.get_data().type_parameters, properties: self.get_data().properties.unwrap() }
    }
    fn into_lambda_expression(self) -> ASTNodeLambdaExpression {
        ASTNodeLambdaExpression { token: self.get_data().token.unwrap(), body: self.get_data().body.unwrap(), parameters: self.get_data().parameters.unwrap() }
    }
    fn into_tuple_expression(self) -> ASTNodeTupleExpression {
        ASTNodeTupleExpression { token: self.get_data().token.unwrap(), type_parameters: self.get_data().type_parameters.unwrap() }
    }
    fn into_type_identifier(self) -> ASTNodeTypeIdentifier {
        ASTNodeTypeIdentifier { token: self.get_data().token.unwrap(), type_identifier: self.get_data().type_identifier.unwrap() }
    }
}

// --------------------------- INTERMEDIATE VALUE --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeALLVALUE {
    pub token: Option<Token>,
    pub name: Option<String>,
    pub lhs: Option<Box<dyn ASTNode>>,
    pub rhs: Option<Box<dyn ASTNode>>,
    pub argumments: Option<Vec<Box<dyn ASTNode>>>,
    pub path: Option<ObjectPath>,
    pub type_parameters: Option<Vec<Type>>,
    pub description: Option<String>,
    pub access_modifier: Option<AccessModifier>,
    pub variable_modifier: Option<VariableModifier>,
    pub type_identifier: Option<Type>,
    pub value: Option<Box<dyn ASTNode>>,
    pub statement_type: Option<StatementType>,
    pub body: Option<Vec<Box<dyn ASTNode>>>,
    pub interfaces: Option<Vec<InterfaceNode>>,
    pub properties: Option<Vec<ASTNodeVariableDeclaration>>,
    pub argument: Option<Box<dyn ASTNode>>,
    pub values: Option<Vec<EnumIndex>>,
    pub type_anonymous_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub parameters: Option<Vec<ASTNodeUndefinedVariable>>,
    pub node_type_name: String
}
// --------------------------- INTERMEDIATE VALUE --------------------------- //


// --------------------------- Constant --------------------------- //
#[derive(Clone, Debug, Serialize)]
pub struct ASTNodeConstant {
    pub token: Token,
}
impl ASTNode for ASTNodeConstant {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "Constant".to_string()
        }
        
    }
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
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "Expression".to_string()
        }
    }
}
// --------------------------- Expression --------------------------- //

// --------------------------- Function Call --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeFunctionCall {
    pub token: Token,
    pub argumments: Vec<Box<dyn ASTNode>>,
    pub path: ObjectPath,
    pub type_parameters: Option<Vec<Type>>,
}
impl ASTNode for ASTNodeFunctionCall {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: Some(self.argumments.clone()),
            path: Some(self.path.clone()),
            type_parameters: self.type_parameters.clone(),
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "FunctionCall".to_string()
        }
    }
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
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: self.value.clone(),
            argumments: None,
            path: None,
            type_parameters: None,
            description: self.description.clone(),
            access_modifier: Some(self.access_modifier),
            variable_modifier: Some(self.variable_modifier),
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "VariableDeclaration".to_string()
        }
    }
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
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: Some(self.type_identifier.clone()),
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "UndefinedVariable".to_string()
        }
    }
}
// --------------------------- Undefined Variable Declaration --------------------------- //

// --------------------------- Return Statement --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeReturnStatement {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: self.value.clone(),
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "ReturnStatement".to_string()
        }
    }
}
// --------------------------- Return Statement --------------------------- //

// --------------------------- Enum Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeEnumDeclaration {
    pub token: Token,
    pub name: String,
    pub values: Vec<EnumIndex>,
    pub description: Option<String>
}
impl ASTNode for ASTNodeEnumDeclaration {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: self.description.clone(),
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: Some(self.values.clone()),
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "EnumDeclaration".to_string()
        }
    }
}
// --------------------------- Enum Declaration --------------------------- //

// --------------------------- Control --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeControl {
    pub token: Token
}
impl ASTNode for ASTNodeControl {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "Control".to_string()
        }
    }
}
// --------------------------- Control --------------------------- //

// --------------------------- Class Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeClassDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<Type>>,
    pub interfaces: Vec<InterfaceNode>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeClassDeclaration {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: self.type_parameters.clone(),
            description: self.description.clone(),
            access_modifier: Some(self.access_modifier.clone()),
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: Some(self.body.clone()),
            interfaces: Some(self.interfaces.clone()),
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "ClassDeclaration".to_string()
        }
    }
}
// --------------------------- Class Declaration --------------------------- //

// --------------------------- Struct Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeStructDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<Type>>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeStructDeclaration {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: self.type_parameters.clone(),
            description: self.description.clone(),
            access_modifier: Some(self.access_modifier.clone()),
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: Some(self.body.clone()),
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "StructDeclaration".to_string()
        }
    }
}
// --------------------------- Struct Declaration --------------------------- //

// --------------------------- Interface Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeInterfaceDeclaration {
    pub token: Token,
    pub name: String,
    pub type_anonymous_parameters: Option<Vec<AnonymousTypeParameter>>,
    pub interfaces: Vec<InterfaceNode>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub body: Vec<Box<dyn ASTNode>>,
}
impl ASTNode for ASTNodeInterfaceDeclaration {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: self.description.clone(),
            access_modifier: Some(self.access_modifier.clone()),
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: Some(self.body.clone()),
            interfaces: Some(self.interfaces.clone()),
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: self.type_anonymous_parameters.clone(),
            parameters: None,
            node_type_name: "InterfaceDeclaration".to_string()
        }
    }
}
// --------------------------- Interface Declaration --------------------------- //

// --------------------------- Function Declaration --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeFunctionDeclaration {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<Type>>,
    pub parameters: Vec<ASTNodeUndefinedVariable>,
    pub description: Option<String>,
    pub access_modifier: AccessModifier,
    pub type_identifier: Type,
    pub body: Option<Vec<Box<dyn ASTNode>>>,
}
impl ASTNode for ASTNodeFunctionDeclaration {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: self.type_parameters.clone(),
            description: self.description.clone(),
            access_modifier: Some(self.access_modifier.clone()),
            variable_modifier: None,
            type_identifier: Some(self.type_identifier.clone()),
            value: None,
            statement_type: None,
            body: self.body.clone(),
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: Some(self.parameters.clone()),
            node_type_name: "FunctionDeclaration".to_string()
        }
    }
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
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: Some(self.statement_type.clone()),
            body: Some(self.body.clone()),
            interfaces: None,
            properties: None,
            argument: self.argument.clone(),
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "Statement".to_string()
        }
    }
}
// --------------------------- Statement --------------------------- //

// --------------------------- Object Creation --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeObjectCreation {
    pub token: Token,
    pub name: String,
    pub type_parameters: Option<Vec<Type>>,
    pub properties: Vec<ASTNodeVariableDeclaration>,
}
impl ASTNode for ASTNodeObjectCreation {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: Some(self.name.clone()),
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: self.type_parameters.clone(),
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: Some(self.properties.clone()),
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "ObjectCreation".to_string()
        }   
    }
}
// --------------------------- Object Creation --------------------------- //

// --------------------------- Lambda Expression --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeLambdaExpression {
    pub token: Token,
    pub parameters: Vec<ASTNodeUndefinedVariable>,
    pub body: Vec<Box<dyn ASTNode>>
}
impl ASTNode for ASTNodeLambdaExpression {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: Some(self.body.clone()),
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: Some(self.parameters.clone()),
            node_type_name: "LambdaExpression".to_string()
        }
    }
}
// --------------------------- Lambda Expression --------------------------- //

// --------------------------- Tuple Expression --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeTupleExpression {
    pub token: Token,
    pub type_parameters: Vec<Type>
}
impl ASTNode for ASTNodeTupleExpression {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: Some(self.type_parameters.clone()),
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: None,
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "TupleExpression".to_string()
        }
    }
}
// --------------------------- Tuple Parameters --------------------------- //

// --------------------------- Type Identifier --------------------------- //
#[derive(Clone, Debug)]
pub struct ASTNodeTypeIdentifier {
    pub token: Token,
    pub type_identifier: Type
}
impl ASTNode for ASTNodeTypeIdentifier {
    fn as_any(&self) -> &dyn Any { self }
    fn clone_box(&self) -> Box<dyn ASTNode> { Box::new(self.clone()) }
    fn get_data(&self) -> ASTNodeALLVALUE {
        ASTNodeALLVALUE {
            token: Some(self.token.clone()),
            name: None,
            lhs: None,
            rhs: None,
            argumments: None,
            path: None,
            type_parameters: None,
            description: None,
            access_modifier: None,
            variable_modifier: None,
            type_identifier: Some(self.type_identifier.clone()),
            value: None,
            statement_type: None,
            body: None,
            interfaces: None,
            properties: None,
            argument: None,
            values: None,
            type_anonymous_parameters: None,
            parameters: None,
            node_type_name: "TypeIdentifier".to_string()
        }
    }
}
// --------------------------- Type Identifier --------------------------- //

