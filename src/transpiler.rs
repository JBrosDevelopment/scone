use serde::Serialize;
#[allow(unused_imports)]
use crate::{ast::{ASTNode, AccessModifier}, codegen, error_handling::ErrorHandling, macros::Macros};

pub fn transpile(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let transpiler = Transpiler::new(ast, code, path, macros);
    let code = codegen::generate_code(&transpiler);

    transpiler.output.print_messages();
    (code, transpiler.output)
}

pub struct Transpiler {
    ast: Vec<ASTNode>,
    output: ErrorHandling,
    macros: Macros,
}

impl Transpiler {
    pub fn new(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> Transpiler {
        Transpiler { ast, 
            output: ErrorHandling::new(path.clone(), 
            code.clone()), 
            macros 
        }
    }
}

pub fn variable_is_static(var: VariableHolder) -> bool {
    var.access_modifier.contains(&AccessModifier::Static)
}
pub fn function_is_static(func: FunctionHolder) -> bool {
    func.access_modifier.contains(&AccessModifier::Static)
}

type Id = u32;
type Scope = u32;

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct VariableHolder {
    pub name: String,
    pub id: Id,
    pub type_id: Id,
    pub has_value: bool,
    pub requires_free: bool,
    pub access_modifier: Vec<AccessModifier>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TypeParameterHolder {
    pub name: String,
    pub type_id: Id,
    pub scope: Scope
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ParameterHolder {
    pub name: String,
    pub id: Id,
    pub type_id: Id,
    pub default_value: Option<String>,
    pub is_params: bool,
    pub is_const: bool
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct FunctionHolder {
    pub name: String,
    pub id: Id,
    pub type_id: Id,
    pub has_body: bool,
    pub type_parameters: Vec<TypeParameterHolder>,
    pub parameters: Vec<ParameterHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct StructHolder {
    pub name: String,
    pub id: Id,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub type_parameters: Vec<TypeParameterHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub inherits: Vec<TraitHolder>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TraitHolder {
    pub name: String,
    pub id: Id,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub inherits: Vec<TraitHolder>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct EnumMemberHolder {
    pub name: String,
    pub id: Id,
    pub index: Option<u32>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct EnumHolder {
    pub name: String,
    pub id: Id,
    pub members: Vec<EnumMemberHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct TypeHolder {
    pub name: String,
    pub type_id: Id,
    pub scope: Scope,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct CodegenTable {
    types: Vec<TypeHolder>,
    type_parameters: Vec<TypeParameterHolder>,

    enums: Vec<EnumHolder>,
    structs: Vec<StructHolder>,
    traits: Vec<TraitHolder>,
    functions: Vec<FunctionHolder>,
    variables: Vec<VariableHolder>,
    
    last_id: Id,
    last_type_id: Id,
    scope: Scope
}

pub enum IdentifierType {
    Enum(EnumHolder),
    Struct(StructHolder),
    Trait(TraitHolder),
    Function(FunctionHolder),
    Variable(VariableHolder),
}
pub enum TypeType {
    Type(TypeHolder),
    TypeParameter(TypeParameterHolder)
}

impl CodegenTable {
    pub fn new() -> CodegenTable {
        CodegenTable {
            types: vec![],
            type_parameters: vec![],
            enums: vec![],
            structs: vec![],
            traits: vec![],
            functions: vec![],
            variables: vec![],
            last_id: 0,
            last_type_id: 0,
            scope: 0
        }
    }
    pub fn get_identifer_type(&self, id: Id) -> Result<IdentifierType, ()> {
        if let Some(enum_) = self.enums.iter().find(|enum_| enum_.id == id) {
            Ok(IdentifierType::Enum(enum_.clone()))
        } else if let Some(struct_) = self.structs.iter().find(|struct_| struct_.id == id) {
            Ok(IdentifierType::Struct(struct_.clone()))
        } else if let Some(trait_) = self.traits.iter().find(|trait_| trait_.id == id) {
            Ok(IdentifierType::Trait(trait_.clone()))
        } else if let Some(function) = self.functions.iter().find(|function| function.id == id) {
            Ok(IdentifierType::Function(function.clone()))
        } else if let Some(variable) = self.variables.iter().find(|variable| variable.id == id) {
            Ok(IdentifierType::Variable(variable.clone()))
        } else {
            Err(())
        }
    }

    pub fn get_identifer_type_by_name(&self, name: &str) -> Result<IdentifierType, ()> {
        if let Some(enum_) = self.enums.iter().find(|enum_| enum_.name == name) {
            Ok(IdentifierType::Enum(enum_.clone()))
        } else if let Some(struct_) = self.structs.iter().find(|struct_| struct_.name == name) {
            Ok(IdentifierType::Struct(struct_.clone()))
        } else if let Some(trait_) = self.traits.iter().find(|trait_| trait_.name == name) {
            Ok(IdentifierType::Trait(trait_.clone()))
        } else if let Some(function) = self.functions.iter().find(|function| function.name == name) {
            Ok(IdentifierType::Function(function.clone()))
        } else if let Some(variable) = self.variables.iter().find(|variable| variable.name == name) {
            Ok(IdentifierType::Variable(variable.clone()))
        } else {
            Err(())
        }
    }

    pub fn add_identifier_scope(&mut self, identifier: IdentifierType) {
        match identifier {
            IdentifierType::Enum(enum_) => self.enums.push(enum_),
            IdentifierType::Struct(struct_) => self.structs.push(struct_),
            IdentifierType::Trait(trait_) => self.traits.push(trait_),
            IdentifierType::Function(function) => self.functions.push(function),
            IdentifierType::Variable(variable) => self.variables.push(variable),
        }
    }

    pub fn add_type_scope(&mut self, type_: TypeType) { 
        match type_ {
            TypeType::Type(type_) => self.types.push(type_),
            TypeType::TypeParameter(type_parameter) => self.type_parameters.push(type_parameter),
        }
    }

    fn remove_all_scope_index(&mut self, scope: Scope) {
        self.types = self.types.iter().filter(|x| x.scope < scope).cloned().collect();
        self.type_parameters = self.type_parameters.iter().filter(|x| x.scope < scope).cloned().collect();
        self.enums = self.enums.iter().filter(|x| x.scope < scope).cloned().collect();
        self.structs = self.structs.iter().filter(|x| x.scope < scope).cloned().collect();
        self.traits = self.traits.iter().filter(|x| x.scope < scope).cloned().collect();
        self.functions = self.functions.iter().filter(|x| x.scope < scope).cloned().collect();
        self.variables = self.variables.iter().filter(|x| x.scope < scope).cloned().collect();
    }

    pub fn increase_scope(&mut self) {
        self.scope += 1;
    }

    pub fn decrease_scope(&mut self) {
        self.remove_all_scope_index(self.scope);
        self.scope -= 1;
    }

    pub fn get_type(&mut self, name: String) -> Result<TypeType, ()> {
        if let Some(type_) = self.types.iter().find(|type_| type_.name == name) {
            Ok(TypeType::Type(type_.clone()))
        } else if let Some(type_) = self.type_parameters.iter().find(|type_| type_.name == name) {
            Ok(TypeType::TypeParameter(type_.clone()))
        } else {
            Err(())
        }
    }

    pub fn generate_variable(&mut self, name: String, type_id: Id, has_value: bool, requires_free: bool, access_modifier: Vec<AccessModifier>) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Variable(VariableHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            type_id,
            has_value,
            requires_free,
            access_modifier,
        })
    }

    pub fn generate_function(&mut self, name: String, type_id: Id, access_modifier: Vec<AccessModifier>, has_body: bool, parameters: Vec<ParameterHolder>, type_parameters: Vec<TypeParameterHolder>) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Function(FunctionHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            type_id,
            access_modifier,
            has_body,
            parameters,
            type_parameters
        })
    }

    pub fn generate_parameter(&mut self, name: String, type_id: Id, is_const: bool, is_params: bool, default_value: Option<String>) -> ParameterHolder {
        self.last_id += 1;
        ParameterHolder {
            id: self.last_id,
            name,
            type_id,
            default_value,
            is_const,
            is_params
        }
    }

    pub fn generate_type(&mut self, name: String) -> TypeType {
        self.last_type_id += 1;
        TypeType::Type(TypeHolder {
            type_id: self.last_type_id,
            scope: self.scope,
            name
        })
    }

    pub fn generate_type_parameter(&mut self, name: String) -> TypeType {
        self.last_type_id += 1;
        TypeType::TypeParameter(TypeParameterHolder {
            type_id: self.last_type_id,
            scope: self.scope,
            name,
        })
    }

    pub fn generate_struct(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, access_modifier: Vec<AccessModifier>, type_parameters: Vec<TypeParameterHolder>, inherits: Vec<TraitHolder>) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Struct(StructHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            methods,
            members,
            access_modifier,
            type_parameters,
            inherits
        })
    }

    pub fn generate_trait(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, access_modifier: Vec<AccessModifier>, inherits: Vec<TraitHolder>) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Trait(TraitHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            methods,
            members,
            access_modifier,
            inherits
        })
    }

    pub fn generate_enum(&mut self, name: String, access_modifier: Vec<AccessModifier>, members: Vec<EnumMemberHolder>) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Enum(EnumHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            members,
            access_modifier
        })
    }

    pub fn generate_enum_member(&mut self, name: String, index: Option<u32>) -> EnumMemberHolder {
        self.last_id += 1;
        EnumMemberHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            index
        }
    }
}