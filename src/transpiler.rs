#[allow(unused_imports)]
use crate::{debug, ast::{ASTNode, AccessModifier, Tag}, lexer::Location, codegen, error_handling::ErrorHandling, macros::Macros, error_handling::DEBUGGING};

pub fn transpile(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let mut transpiler = Transpiler::new(ast, code, path, macros);
    let code = codegen::generate_code(&mut transpiler);
    transpiler.output.print_messages();
    (code, transpiler.output)
}

#[derive(Clone, Debug)]
pub struct Transpiler {
    pub ast: Vec<ASTNode>,
    pub output: ErrorHandling,
    pub macros: Macros,
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

pub type Id = u32;
pub type TypeId = u32;
pub type Scope = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableHolder {
    pub name: String,
    pub id: Id,
    pub type_id: TypeId,
    pub has_value: bool,
    pub requires_free: bool,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub scope: Scope,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeParameterHolder {
    pub name: String,
    pub type_id: TypeId,
    pub scope: Scope
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParameterHolder {
    pub name: String,
    pub id: Id,
    pub type_id: TypeId,
    pub default_value: Option<String>,
    pub is_params: bool,
    pub is_const: bool,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionHolder {
    pub name: String,
    pub id: Id,
    pub type_id: TypeId,
    pub has_body: bool,
    pub type_parameters: Vec<TypeParameterHolder>,
    pub parameters: Vec<ParameterHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub scope: Scope,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructHolder {
    pub name: String,
    pub id: Id,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub type_parameters: Vec<TypeParameterHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub inherits: Vec<TraitHolder>,
    pub scope: Scope,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitHolder {
    pub name: String,
    pub id: Id,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub inherits: Vec<TraitHolder>,
    pub scope: Scope,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumMemberHolder {
    pub name: String,
    pub id: Id,
    pub index: Option<u32>,
    pub scope: Scope,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumHolder {
    pub name: String,
    pub id: Id,
    pub members: Vec<EnumMemberHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub scope: Scope,
    pub location: Location,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeHolder {
    pub name: String,
    pub type_parameter_count: usize,
    pub type_id: TypeId,
    pub compatible_types: Vec<TypeId>,
    pub scope: Scope,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CodegenTable {
    types: Vec<TypeHolder>,
    type_parameters: Vec<TypeParameterHolder>,

    enums: Vec<EnumHolder>,
    structs: Vec<StructHolder>,
    traits: Vec<TraitHolder>,
    functions: Vec<FunctionHolder>,
    variables: Vec<VariableHolder>,
    
    last_id: Id,
    last_type_id: TypeId,
    scope: Scope,
    inside_macro: Vec<Option<String>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierType {
    Enum(EnumHolder),
    Struct(StructHolder),
    Trait(TraitHolder),
    Function(FunctionHolder),
    Variable(VariableHolder),
}

#[derive(Clone, Debug, PartialEq)]
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
            scope: 0,
            inside_macro: vec![],
        }
    }
    pub fn get_identifer_type_by_id(&self, id: Id) -> Result<IdentifierType, ()> {
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

    pub fn get_identifer_type_by_name(&self, name: &String) -> Result<IdentifierType, ()> {
        if let Some(enum_) = self.enums.iter().find(|enum_| &enum_.name == name) {
            Ok(IdentifierType::Enum(enum_.clone()))
        } else if let Some(struct_) = self.structs.iter().find(|struct_| &struct_.name == name) {
            Ok(IdentifierType::Struct(struct_.clone()))
        } else if let Some(trait_) = self.traits.iter().find(|trait_| &trait_.name == name) {
            Ok(IdentifierType::Trait(trait_.clone()))
        } else if let Some(function) = self.functions.iter().find(|function| &function.name == name) {
            Ok(IdentifierType::Function(function.clone()))
        } else if let Some(variable) = self.variables.iter().find(|variable| &variable.name == name) {
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
        //self.type_parameters = self.type_parameters.iter().filter(|x| x.scope < scope).cloned().collect();
        self.types = self.types.iter().filter(|x| x.scope < scope).cloned().collect();
        self.enums = self.enums.iter().filter(|x| x.scope < scope).cloned().collect();
        self.structs = self.structs.iter().filter(|x| x.scope < scope).cloned().collect();
        self.traits = self.traits.iter().filter(|x| x.scope < scope).cloned().collect();
        self.functions = self.functions.iter().filter(|x| x.scope < scope).cloned().collect();
        self.variables = self.variables.iter().filter(|x| x.scope < scope).cloned().collect();
    }

    pub fn increase_scope(&mut self, inside_macro: bool) {
        self.scope += 1;
        self.inside_macro.push(if inside_macro { Some("".to_string()) } else { None });
    }

    pub fn decrease_scope(&mut self) {
        self.remove_all_scope_index(self.scope);
        self.scope -= 1;
        self.inside_macro.pop();
    }

    pub fn is_inside_macro(&self) -> bool {
        self.inside_macro.iter().any(|x| x.is_some())
    }

    pub fn add_macro_header(&mut self, header: String) {
        if let Some(last_macro) = self.inside_macro.last_mut() {
            if let Some(macro_headers) = last_macro {
                *macro_headers = format!("{}\n{}", macro_headers, header);
            }
        }
    }

    pub fn get_macro_header(&self) -> String {
        let mut headers = "".to_string();
        for header in self.inside_macro.iter() {
            if let Some(header) = header {
                headers = format!("{}\n{}", headers, header);
            }
        }
        headers
    }

    pub fn scope(&self) -> Scope {
        self.scope
    }

    pub fn get_all_functions(&self) -> Vec<FunctionHolder> { 
        self.functions.clone()
    }

    pub fn get_type_name(&mut self, name: &String) -> Result<TypeType, ()> {
        if let Some(type_) = self.types.iter().find(|type_| &type_.name == name) {
            Ok(TypeType::Type(type_.clone()))
        } else if let Some(type_) = self.type_parameters.iter().find(|type_| &type_.name == name) {
            if self.scope <= type_.scope {
                Ok(TypeType::TypeParameter(type_.clone()))
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }

    pub fn get_type_id(&self, type_id: TypeId) -> Result<TypeType, ()> {
        if let Some(type_) = self.types.iter().find(|type_| type_.type_id == type_id) {
            Ok(TypeType::Type(type_.clone()))
        } else if let Some(type_) = self.type_parameters.iter().find(|type_| type_.type_id == type_id) {
            Ok(TypeType::TypeParameter(type_.clone()))
        } else {
            Err(())
        }
    }

    pub fn is_exposed(&self, identifier: &IdentifierType) -> Option<String> {
        match identifier {
            IdentifierType::Enum(enum_) => enum_.tags.contains(&Tag::Expose).then_some(enum_.name.clone()),
            IdentifierType::Struct(struct_) => struct_.tags.contains(&Tag::Expose).then_some(struct_.name.clone()),
            IdentifierType::Trait(trait_) => trait_.tags.contains(&Tag::Expose).then_some(trait_.name.clone()),
            IdentifierType::Function(function) => function.tags.contains(&Tag::Expose).then_some(function.name.clone()),
            IdentifierType::Variable(variable) => variable.tags.contains(&Tag::Expose).then_some(variable.name.clone()),
        }
    }

    pub fn get_idname_from_identifier_type(&self, identifier: &IdentifierType) -> String {
        if let Some(name) = self.is_exposed(identifier) {
            return name;
        }
        match identifier {
            IdentifierType::Enum(enum_) => format!("e{}", enum_.id),
            IdentifierType::Struct(struct_) => format!("s{}", struct_.id),
            IdentifierType::Trait(trait_) => format!("t{}", trait_.id),
            IdentifierType::Function(function) => format!("f{}", function.id),
            IdentifierType::Variable(variable) => format!("v{}", variable.id),
        }
    }

    pub fn get_idname_from_parameter_holder(identifier: &ParameterHolder) -> String {
        format!("v{}", identifier.id) // needs to act like a variable
    }

    pub fn get_idname_from_type_type(ty: &TypeType) -> String {
        match ty {
            TypeType::Type(type_) => format!("t{}", type_.type_id),
            TypeType::TypeParameter(type_parameter) => format!("a{}", type_parameter.type_id),
        }
    }

    pub fn get_type_parameter_from_type_id(&self, type_id: TypeId) -> Option<TypeParameterHolder> {
        match self.get_type_id(type_id) {
            Ok(TypeType::TypeParameter(type_parameter)) => Some(type_parameter),
            _ => None
        }
    }

    pub fn check_types_compatibility(&self, expected_type_id: TypeId, found_type_id: TypeId) -> bool {
        if expected_type_id == found_type_id {
            return true
        }
        if let Ok(type1) = self.get_type_id(expected_type_id) {
            let type1_compatible_types = match type1 {
                TypeType::Type(type_) => type_.compatible_types,
                TypeType::TypeParameter(_) => vec![]
            };
            return type1_compatible_types.contains(&found_type_id)
        }    
        return false
    }

    pub fn add_parameters_to_scope(&mut self, parameters: Vec<ParameterHolder>) { 
        for param in parameters {
            let new_var = IdentifierType::Variable(VariableHolder { 
                name: param.name,
                id: param.id,
                type_id: param.type_id,
                has_value: true,
                requires_free: false,
                access_modifier: vec![],
                tags: vec![],
                scope: self.scope,
                location: param.location 
            });
            self.add_identifier_scope(new_var);
        }
    }

    pub fn generate_variable(&mut self, name: String, type_id: TypeId, has_value: bool, requires_free: bool, access_modifier: Vec<AccessModifier>, tags: Vec<Tag>, location: Location) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Variable(VariableHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            type_id,
            has_value,
            requires_free,
            access_modifier,
            tags,
            location,
        })
    }

    pub fn generate_function(&mut self, name: String, type_id: TypeId, access_modifier: Vec<AccessModifier>, has_body: bool, parameters: Vec<ParameterHolder>, type_parameters: Vec<TypeParameterHolder>, tags: Vec<Tag>, location: Location) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Function(FunctionHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            type_id,
            access_modifier,
            tags,
            location,
            has_body,
            parameters,
            type_parameters
        })
    }

    pub fn generate_parameter(&mut self, name: String, type_id: TypeId, is_const: bool, is_params: bool, default_value: Option<String>, location: Location) -> ParameterHolder {
        self.last_id += 1;
        ParameterHolder {
            id: self.last_id,
            name,
            type_id,
            default_value,
            is_const,
            is_params,
            location,
        }
    }

    pub fn generate_type(&mut self, name: String, type_parameter_count: usize, compatible_types: Vec<TypeId>) -> TypeType {
        self.last_type_id += 1;
        TypeType::Type(TypeHolder {
            type_id: self.last_type_id,
            scope: self.scope,
            compatible_types,
            type_parameter_count,
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

    pub fn generate_struct(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, access_modifier: Vec<AccessModifier>, type_parameters: Vec<TypeParameterHolder>, inherits: Vec<TraitHolder>, tags: Vec<Tag>, location: Location) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Struct(StructHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            methods,
            members,
            access_modifier,
            tags,
            location,
            type_parameters,
            inherits
        })
    }

    pub fn generate_trait(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, access_modifier: Vec<AccessModifier>, inherits: Vec<TraitHolder>, tags: Vec<Tag>, location: Location) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Trait(TraitHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            methods,
            members,
            access_modifier,
            tags,
            location,
            inherits
        })
    }

    pub fn generate_enum(&mut self, name: String, access_modifier: Vec<AccessModifier>, members: Vec<EnumMemberHolder>, tags: Vec<Tag>, location: Location) -> IdentifierType {
        self.last_id += 1;
        IdentifierType::Enum(EnumHolder {
            id: self.last_id,
            scope: self.scope,
            name,
            members,
            access_modifier,
            tags,
            location,
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