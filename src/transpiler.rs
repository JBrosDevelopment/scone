#[allow(unused_imports)]
use crate::{debug, ast::{ASTNode, AccessModifier, Tag}, lexer::Location, codegen, codecheck, error_handling::ErrorHandling, macros::Macros, error_handling::DEBUGGING};

pub fn transpile(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> (String, ErrorHandling) {
    let mut transpiler = Transpiler::new(ast, code, path, macros);
    transpiler.table.add_default_types();
    
    codecheck::check_ast(&mut transpiler);
    transpiler.output.print_messages();
    
    if transpiler.output.has_errors() {
        return ("".to_string(), transpiler.output);
    }
    
    //let code = codegen::generate_code(&mut transpiler);
    //transpiler.output.print_messages();
    (/*code*/ "".to_string(), transpiler.output)
}

#[derive(Clone, Debug)]
pub struct Transpiler {
    pub ast: Vec<ASTNode>,
    pub output: ErrorHandling,
    pub macros: Macros,
    pub table: CodegenTable,
}

impl Transpiler {
    pub fn new(ast: Vec<ASTNode>, code: &String, path: Option<String>, macros: Macros) -> Transpiler {
        Transpiler { ast, 
            output: ErrorHandling::new(path.clone(), 
            code.clone()), 
            macros,
            table: CodegenTable::new()
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

// want to create a tree of sorts
// each object has a vertical scope and a horizontal scope
// foe example:
/*
    scope (0, 0)
    {
        scope (1, 0)
        {
            scope (2, 0)
        }
        {
            scope (2, 1)
            {
                scope (3, 1)
            }
        }
    }
*/
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope {
    pub depth: u32,
    pub index: u32
}

impl Scope {
    pub fn new() -> Self {
        Scope { depth: 0, index: 0 }
    }

    pub fn increase(&mut self) {
        self.depth += 1;
    }

    pub fn decrease_vertical(&mut self) {
        if self.depth > 0 {
            self.depth -= 1;
            self.index += 1;
        }
    }

    pub fn in_scope(&self, other: &Scope) -> bool {
        if self.depth == other.depth && self.index == other.index {
            return true;
        }
        if other.depth > self.depth && other.index == self.index {
            return true;
        }

        false
    }
}

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
    pub type_id: Option<TypeId>,
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
    pub type_id: TypeId,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub structs: Vec<StructHolder>,
    pub enums: Vec<EnumHolder>,
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
    pub type_id: TypeId,
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
    pub type_id: TypeId,
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
    inside_macro: Vec<Option<String>>,

    pub scope: Scope
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierEnum {
    Enum(EnumHolder),
    Struct(StructHolder),
    Trait(TraitHolder),
    Function(FunctionHolder),
    Variable(VariableHolder),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeEnum {
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
            scope: Scope::new(),
            inside_macro: vec![],
        }
    }
    pub fn get_identifer_enum_by_id(&self, id: Id) -> Result<IdentifierEnum, ()> {
        if let Some(enum_) = self.enums.iter().find(|enum_| enum_.id == id) {
            Ok(IdentifierEnum::Enum(enum_.clone()))
        } else if let Some(struct_) = self.structs.iter().find(|struct_| struct_.id == id) {
            Ok(IdentifierEnum::Struct(struct_.clone()))
        } else if let Some(trait_) = self.traits.iter().find(|trait_| trait_.id == id) {
            Ok(IdentifierEnum::Trait(trait_.clone()))
        } else if let Some(function) = self.functions.iter().find(|function| function.id == id) {
            Ok(IdentifierEnum::Function(function.clone()))
        } else if let Some(variable) = self.variables.iter().find(|variable| variable.id == id) {
            Ok(IdentifierEnum::Variable(variable.clone()))
        } else {
            Err(())
        }
    }

    pub fn get_identifer_enum_by_name(&self, name: &String) -> Result<IdentifierEnum, ()> {
        if let Some(enum_) = self.enums.iter().find(|enum_| &enum_.name == name) {
            Ok(IdentifierEnum::Enum(enum_.clone()))
        } else if let Some(struct_) = self.structs.iter().find(|struct_| &struct_.name == name) {
            Ok(IdentifierEnum::Struct(struct_.clone()))
        } else if let Some(trait_) = self.traits.iter().find(|trait_| &trait_.name == name) {
            Ok(IdentifierEnum::Trait(trait_.clone()))
        } else if let Some(function) = self.functions.iter().find(|function| &function.name == name) {
            Ok(IdentifierEnum::Function(function.clone()))
        } else if let Some(variable) = self.variables.iter().find(|variable| &variable.name == name) {
            Ok(IdentifierEnum::Variable(variable.clone()))
        } else {
            Err(())
        }
    }

    pub fn add_identifier_scope(&mut self, identifier: IdentifierEnum) {
        match identifier {
            IdentifierEnum::Enum(enum_) => self.enums.push(enum_),
            IdentifierEnum::Struct(struct_) => self.structs.push(struct_),
            IdentifierEnum::Trait(trait_) => self.traits.push(trait_),
            IdentifierEnum::Function(function) => self.functions.push(function),
            IdentifierEnum::Variable(variable) => self.variables.push(variable),
        }
    }

    pub fn add_type_scope(&mut self, type_: TypeEnum) { 
        match type_ {
            TypeEnum::Type(type_) => self.types.push(type_),
            TypeEnum::TypeParameter(type_parameter) => self.type_parameters.push(type_parameter),
        }
    }

    pub fn add_module_identifier_to_type_scope(&mut self, identifier: &IdentifierEnum) {
        let new_type = TypeEnum::Type(TypeHolder {
            type_id: self.last_type_id,
            scope: self.scope.clone(),
            compatible_types: vec![],
            type_parameter_count: 0,
            name: self.get_identifier_enum_name(&identifier)
        });

        self.add_type_scope(new_type);
    }

    pub fn increase_scope(&mut self, inside_macro: bool) {
        self.scope.increase();
        self.inside_macro.push(if inside_macro { Some("".to_string()) } else { None });
    }

    pub fn decrease_scope(&mut self) {
        self.scope.decrease_vertical();
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
        self.scope.clone()
    }

    pub fn get_all_functions(&self) -> Vec<FunctionHolder> { 
        self.functions.clone()
    }

    pub fn get_type_id(&self, type_id: TypeId) -> Result<TypeEnum, ()> {
        if let Some(type_) = self.types.iter().find(|type_| type_.type_id == type_id) {
            Ok(TypeEnum::Type(type_.clone()))
        } else if let Some(type_) = self.type_parameters.iter().find(|type_| type_.type_id == type_id) {
            Ok(TypeEnum::TypeParameter(type_.clone()))
        } else {
            Err(())
        }
    }

    pub fn get_type_name(&self, name: &String) -> Result<TypeEnum, ()> {
        if let Some(type_) = self.types.iter().find(|type_| type_.name == *name) {
            Ok(TypeEnum::Type(type_.clone()))
        } else if let Some(type_) = self.type_parameters.iter().find(|type_| type_.name == *name) {
            Ok(TypeEnum::TypeParameter(type_.clone()))
        } else {
            Err(())
        }
    }

    pub fn is_exposed(&self, identifier: &IdentifierEnum) -> Option<String> {
        match identifier {
            IdentifierEnum::Enum(enum_) => enum_.tags.contains(&Tag::Expose).then_some(enum_.name.clone()),
            IdentifierEnum::Struct(struct_) => struct_.tags.contains(&Tag::Expose).then_some(struct_.name.clone()),
            IdentifierEnum::Trait(trait_) => trait_.tags.contains(&Tag::Expose).then_some(trait_.name.clone()),
            IdentifierEnum::Function(function) => function.tags.contains(&Tag::Expose).then_some(function.name.clone()),
            IdentifierEnum::Variable(variable) => variable.tags.contains(&Tag::Expose).then_some(variable.name.clone()),
        }
    }

    pub fn name_is_used_in_scope(&self, name: &String) -> bool {
        self.enums.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.structs.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.traits.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.functions.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.variables.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.types.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none() &&
        self.type_parameters.iter().find(|x| &x.name == name && x.scope.in_scope(&self.scope)).is_none()
    }

    pub fn get_module_identifier_enum_id_from_type_id(&self, type_id: TypeId) -> Result<Id, ()> {
        let t = self.get_type_id(type_id)?;
        let type_name = self.get_type_enum_name(&t);
        let type_scope = self.get_type_enum_scope(&t);

        if let Some(holder) = self.enums.iter().find(|x| x.name == type_name) {
            if holder.scope.in_scope(&type_scope) {
                return Ok(holder.id)
            }
        } else if let Some(holder) = self.structs.iter().find(|x| x.name == type_name) {
            debug!(holder.scope);
            debug!(type_scope);
            if holder.scope.in_scope(&type_scope) {
                return Ok(holder.id)
            }
        }
        Err(())
    }

    pub fn get_idname_from_identifier_enum(&self, identifier: &IdentifierEnum) -> String {
        if let Some(name) = self.is_exposed(identifier) {
            return name;
        }
        match identifier {
            IdentifierEnum::Enum(enum_) => format!("e{}", enum_.id),
            IdentifierEnum::Struct(struct_) => format!("s{}", struct_.id),
            IdentifierEnum::Trait(trait_) => format!("t{}", trait_.id),
            IdentifierEnum::Function(function) => format!("f{}", function.id),
            IdentifierEnum::Variable(variable) => format!("v{}", variable.id),
        }
    }

    pub fn get_enum_from_identifier_enum(&self, identifier: &IdentifierEnum) -> Option<EnumHolder> {
        match identifier {
            IdentifierEnum::Enum(enum_) => Some(enum_.clone()),
            _ => None
        }
    }
    pub fn get_struct_from_identifier_enum(&self, identifier: &IdentifierEnum) -> Option<StructHolder> {
        match identifier {
            IdentifierEnum::Struct(struct_) => Some(struct_.clone()),
            _ => None
        }
    }
    pub fn get_trait_from_identifier_enum(&self, identifier: &IdentifierEnum) -> Option<TraitHolder> {
        match identifier {
            IdentifierEnum::Trait(trait_) => Some(trait_.clone()),
            _ => None
        }
    }
    pub fn get_function_from_identifier_enum(&self, identifier: &IdentifierEnum) -> Option<FunctionHolder> {
        match identifier {
            IdentifierEnum::Function(function) => Some(function.clone()),
            _ => None
        }
    }
    pub fn get_variable_from_identifier_enum(&self, identifier: &IdentifierEnum) -> Option<VariableHolder> {
        match identifier {
            IdentifierEnum::Variable(variable) => Some(variable.clone()),
            _ => None
        }
    }

    pub fn get_type_from_type_enum(&self, ty: &TypeEnum) -> Option<TypeHolder> {
        match ty {
            TypeEnum::Type(type_) => Some(type_.clone()),
            _ => None
        }
    }

    pub fn get_type_parameter_from_type_enum(&self, ty: &TypeEnum) -> Option<TypeParameterHolder> {
        match ty {
            TypeEnum::TypeParameter(type_parameter) => Some(type_parameter.clone()),
            _ => None
        }
    }

    pub fn get_identifier_enum_id(&self, identifier: &IdentifierEnum) -> Id {
        match identifier {
            IdentifierEnum::Enum(enum_) => enum_.id,
            IdentifierEnum::Struct(struct_) => struct_.id,
            IdentifierEnum::Trait(trait_) => trait_.id,
            IdentifierEnum::Function(function) => function.id,
            IdentifierEnum::Variable(variable) => variable.id,
        }
    }

    pub fn get_identifier_enum_name(&self, identifier: &IdentifierEnum) -> String {
        match identifier {
            IdentifierEnum::Enum(enum_) => enum_.name.clone(),
            IdentifierEnum::Struct(struct_) => struct_.name.clone(),
            IdentifierEnum::Trait(trait_) => trait_.name.clone(),
            IdentifierEnum::Function(function) => function.name.clone(),
            IdentifierEnum::Variable(variable) => variable.name.clone(),
        }
    }

    pub fn get_type_enum_type_id(&self, ty: &TypeEnum) -> TypeId {
        match ty {
            TypeEnum::Type(type_) => type_.type_id,
            TypeEnum::TypeParameter(type_parameter) => type_parameter.type_id,
        }
    }
    pub fn get_type_enum_name(&self, ty: &TypeEnum) -> String {
        match ty {
            TypeEnum::Type(type_) => type_.name.clone(),
            TypeEnum::TypeParameter(type_parameter) => type_parameter.name.clone(),
        }
    }
    pub fn get_type_enum_scope(&self, ty: &TypeEnum) -> Scope {
        match ty {
            TypeEnum::Type(type_) => type_.scope.clone(),
            TypeEnum::TypeParameter(type_parameter) => type_parameter.scope.clone(),
        }
    }

    pub fn get_idname_from_parameter_holder(identifier: &ParameterHolder) -> String {
        format!("v{}", identifier.id) // needs to act like a variable
    }

    pub fn get_idname_from_type_enum(ty: &TypeEnum) -> String {
        match ty {
            TypeEnum::Type(type_) => format!("t{}", type_.type_id),
            TypeEnum::TypeParameter(type_parameter) => format!("a{}", type_parameter.type_id),
        }
    }

    pub fn get_type_parameter_from_type_id(&self, type_id: TypeId) -> Option<TypeParameterHolder> {
        match self.get_type_id(type_id) {
            Ok(TypeEnum::TypeParameter(type_parameter)) => Some(type_parameter),
            _ => None
        }
    }

    pub fn check_types_compatibility(&self, expected_type_id: TypeId, found_type_id: TypeId) -> bool {
        if expected_type_id == found_type_id {
            return true
        }
        if let Ok(type1) = self.get_type_id(expected_type_id) {
            let type1_compatible_types = match type1 {
                TypeEnum::Type(type_) => type_.compatible_types,
                TypeEnum::TypeParameter(_) => vec![]
            };
            return type1_compatible_types.contains(&found_type_id)
        }    
        return false
    }

    pub fn add_parameters_to_scope(&mut self, parameters: Vec<ParameterHolder>) { 
        for param in parameters {
            let new_var = IdentifierEnum::Variable(VariableHolder { 
                name: param.name,
                id: param.id,
                type_id: param.type_id,
                has_value: true,
                requires_free: false,
                access_modifier: vec![],
                tags: vec![],
                scope: self.scope.clone(),
                location: param.location 
            });
            self.add_identifier_scope(new_var);
        }
    }

    pub fn generate_variable(&mut self, name: String, type_id: TypeId, has_value: bool, requires_free: bool, access_modifier: Vec<AccessModifier>, tags: Vec<Tag>, location: Location) -> IdentifierEnum {
        self.last_id += 1;
        IdentifierEnum::Variable(VariableHolder {
            id: self.last_id,
            scope: self.scope.clone(),
            name,
            type_id,
            has_value,
            requires_free,
            access_modifier,
            tags,
            location,
        })
    }

    pub fn generate_function(&mut self, name: String, type_id: Option<TypeId>, access_modifier: Vec<AccessModifier>, has_body: bool, parameters: Vec<ParameterHolder>, type_parameters: Vec<TypeParameterHolder>, tags: Vec<Tag>, location: Location) -> IdentifierEnum {
        self.last_id += 1;
        IdentifierEnum::Function(FunctionHolder {
            id: self.last_id,
            scope: self.scope.clone(),
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

    pub fn generate_type(&mut self, name: String, type_parameter_count: usize, compatible_types: Vec<TypeId>) -> TypeEnum {
        self.last_type_id += 1;
        TypeEnum::Type(TypeHolder {
            type_id: self.last_type_id,
            scope: self.scope.clone(),
            compatible_types,
            type_parameter_count,
            name
        })
    }

    pub fn generate_type_parameter(&mut self, name: String) -> TypeEnum {
        self.last_type_id += 1;
        TypeEnum::TypeParameter(TypeParameterHolder {
            type_id: self.last_type_id,
            scope: self.scope.clone(),
            name,
        })
    }

    pub fn generate_struct(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, structs: Vec<StructHolder>, enums: Vec<EnumHolder>, access_modifier: Vec<AccessModifier>, type_parameters: Vec<TypeParameterHolder>, inherits: Vec<TraitHolder>, tags: Vec<Tag>, location: Location) -> IdentifierEnum {
        self.last_id += 1;
        self.last_type_id += 1;
        IdentifierEnum::Struct(StructHolder {
            id: self.last_id,
            type_id: self.last_type_id,
            scope: self.scope.clone(),
            name,
            methods,
            members,
            structs,
            enums,
            access_modifier,
            tags,
            location,
            type_parameters,
            inherits
        })
    }

    pub fn generate_trait(&mut self, name: String, methods: Vec<FunctionHolder>, members: Vec<VariableHolder>, access_modifier: Vec<AccessModifier>, inherits: Vec<TraitHolder>, tags: Vec<Tag>, location: Location) -> IdentifierEnum {
        self.last_id += 1;
        self.last_type_id += 1;
        IdentifierEnum::Trait(TraitHolder {
            id: self.last_id,
            type_id: self.last_type_id,
            scope: self.scope.clone(),
            name,
            methods,
            members,
            access_modifier,
            tags,
            location,
            inherits
        })
    }

    pub fn generate_enum(&mut self, name: String, access_modifier: Vec<AccessModifier>, members: Vec<EnumMemberHolder>, tags: Vec<Tag>, location: Location) -> IdentifierEnum {
        self.last_id += 1;
        self.last_type_id += 1;
        IdentifierEnum::Enum(EnumHolder {
            id: self.last_id,
            type_id: self.last_type_id,
            scope: self.scope.clone(),
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
            scope: self.scope.clone(),
            name,
            index
        }
    }

    pub fn add_default_types(&mut self) {
        let _f32 = self.generate_type("f32".to_string(), 0, vec![3, 4, 5, 6, 7, 8, 9, 10]);
        self.add_type_scope(_f32); // type_id = 1

        let _f64 = self.generate_type("f64".to_string(), 0, vec![2, 3, 4, 5, 6, 7, 8, 9, 10]);
        self.add_type_scope(_f64); // type_id = 2

        let i8 = self.generate_type("i8".to_string(), 0, vec![]);
        self.add_type_scope(i8); // type_id = 3

        let _i16 = self.generate_type("i16".to_string(), 0, vec![3, 7]);
        self.add_type_scope(_i16); // type_id = 4

        let _i32 = self.generate_type("i32".to_string(), 0, vec![3, 4, 7, 8]);
        self.add_type_scope(_i32); // type_id = 5

        let _i64 = self.generate_type("i64".to_string(), 0, vec![3, 4, 5, 7, 8, 9]);
        self.add_type_scope(_i64); // type_id = 6

        let _u8 = self.generate_type("u8".to_string(), 0, vec![]);
        self.add_type_scope(_u8); // type_id = 7

        let _u16 = self.generate_type("u16".to_string(), 0, vec![7]);
        self.add_type_scope(_u16); // type_id = 8

        let _u32 = self.generate_type("u32".to_string(), 0, vec![7, 8]);
        self.add_type_scope(_u32); // type_id = 9

        let _u64 = self.generate_type("u64".to_string(), 0, vec![7, 8, 9]);
        self.add_type_scope(_u64); // type_id = 10

        let _bool = self.generate_type("bool".to_string(), 0, vec![]);
        self.add_type_scope(_bool); // type_id = 11

        let _string = self.generate_type("string".to_string(), 0, vec![]);
        self.add_type_scope(_string); // type_id = 12
        
        let _vector = self.generate_type("vector".to_string(), 1, vec![]);
        self.add_type_scope(_vector); // type_id = 13

        let _char_ptr_ptr = self.generate_type("char_ptr_ptr".to_string(), 0, vec![]);
        self.add_type_scope(_char_ptr_ptr); // type_id = 14
        
        let _entry_argc = self.generate_variable("ENTRY_ARGC".to_string(), 5, true, false, vec![AccessModifier::Const], vec![], Location { line: 0, column: 0, length: 0 });
        self.add_identifier_scope(_entry_argc); // id = 1
        
        let _entry_argv = self.generate_variable("ENTRY_ARGV".to_string(), 14, true, false, vec![AccessModifier::Const], vec![], Location { line: 0, column: 0, length: 0 });
        self.add_identifier_scope(_entry_argv); // id = 2
    }
}