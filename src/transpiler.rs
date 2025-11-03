use std::{collections::HashMap, rc::Rc};
use serde::{Serialize, ser::SerializeStruct};

#[allow(unused_imports)]
use crate::{ast::{ASTNode, AccessModifier, TypeModifier, Tag}, declarer, debug, error_handling::{self, ErrorHandling, DEBUGGING}, lexer::Location, macros::Macros, resolver };

////////////////////////////////////////////////////////////////////////////////////
// Entry point
////////////////////////////////////////////////////////////////////////////////////

pub fn transpile(ast: Vec<ASTNode>, error_handling: &mut ErrorHandling, macros: Macros) -> String {
    let mut transpiler = Transpiler::new(ast, macros);

    // declarer pass
    crate::error_handling::print_pipeline_operation("declaring pass");
    declarer::declarer_pass_on_ast(&mut transpiler, error_handling);

    // print out symbols
    crate::debut_out_contents!("src/testing/symbols.out.json", &transpiler.symbols);
    crate::debut_out_contents!("src/testing/declarer_pass.ast.out.json", &transpiler.ast);
    crate::check_if_can_continue!(error_handling, false, "".to_string());

    // resolver pass
    crate::error_handling::print_pipeline_operation("resolver pass");
    resolver::resolver_pass_on_ast(&mut transpiler, error_handling);

    // print out table
    crate::debut_out_contents!("src/testing/table.out.json", &transpiler.table);
    crate::check_if_can_continue!(error_handling, false, "".to_string());

    // codegen step placeholder
    "".to_string()
}

////////////////////////////////////////////////////////////////////////////////////
// Transpiler
////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Serialize)]
pub struct Transpiler {
    pub ast: Vec<ASTNode>,
    pub macros: Macros,
    pub symbols: HashMap<Id, Rc<Symbol>>,
    pub table: CodegenTable,
}

impl Transpiler {
    pub fn new(ast: Vec<ASTNode>, macros: Macros) -> Self {
        Self {
            ast,
            macros,
            table: CodegenTable::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn get_symbol_by_name(&self, name: &String, scope: &Scope) -> Option<Rc<Symbol>> {
        for (_key, symbol) in self.symbols.iter() {
            if symbol.name == *name && scope.in_scope(&symbol.scope) {
                return Some(symbol.clone());
            }
        }
        None
    }
}

pub type Id = u32;

////////////////////////////////////////////////////////////////////////////////////
// Scopes
////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
pub struct Scope {
    frames: Vec<Frame>,
}

#[derive(Clone, Debug)]
struct Frame {
    branch_id: u32,
    next_child_counter: u32,
    pending_sibling: bool,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame {
                branch_id: 0,
                next_child_counter: 0,
                pending_sibling: false,
            }],
        }
    }

    pub fn increase(&mut self) {
        let parent = self.frames.last_mut().unwrap();
        let child_branch_id = if parent.pending_sibling {
            let id = parent.next_child_counter;
            parent.next_child_counter = parent.next_child_counter.wrapping_add(1);
            parent.pending_sibling = false;
            id
        } else {
            if parent.next_child_counter == 0 {
                parent.next_child_counter = 1;
            }
            parent.branch_id
        };
        self.frames.push(Frame {
            branch_id: child_branch_id,
            next_child_counter: 0,
            pending_sibling: false,
        });
    }

    pub fn decrease(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
            if let Some(parent) = self.frames.last_mut() {
                parent.pending_sibling = true;
            }
        }
    }

    pub fn in_scope(&self, other: &Scope) -> bool {
        if self.frames.len() > other.frames.len() {
            return false;
        }
        for (i, f) in self.frames.iter().enumerate() {
            if f.branch_id != other.frames[i].branch_id {
                return false;
            }
        }
        true
    }

    pub fn depth(&self) -> usize {
        self.frames.len() - 1
    }

    pub fn index(&self) -> u32 {
        self.frames.last().unwrap().branch_id
    }

    pub fn coords(&self) -> (usize, u32) {
        (self.depth(), self.index())
    }
}

impl Serialize for Scope {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // serialize the struct normally
        let mut state = serializer.serialize_struct("Scope", 1)?;
        state.serialize_field("coordinates", &format!("{}, {}", self.depth(), self.index()))?;
        state.end()
    }
}

////////////////////////////////////////////////////////////////////////////////////
// Symbols
////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, Serialize)]
pub enum ObjectTypes {
    Function,
    Variable,
    Trait,
    Struct,
    Enum,
    EnumVariant,
    Identifier,
    GenericType,
}

#[derive(Clone, Debug, Serialize)]
pub struct Symbol {
    pub name: String,
    pub object_type: ObjectTypes,
    pub id: Id,
    pub scope: Scope,
}

impl Symbol {
    pub fn new(name: String, object_type: ObjectTypes, id: Id, scope: Scope) -> Self {
        Symbol {
            name,
            object_type,
            id,
            scope,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////
// Codegen Holders (using Rc and serde_with)
////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Serialize)]
pub struct TypeHolder {
    pub symbol: Rc<Symbol>,
    pub is_generic: bool,
    pub parent_id: Option<Rc<TypeHolder>>
}

impl TypeHolder {
    pub fn compatible_types(t1: &TypeHolder, t2: &TypeHolder) -> bool {
        (t1.symbol.id == t2.symbol.id) ||
        (t1.symbol.name == "i64" && matches!(t2.symbol.name.as_str(), "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
        (t1.symbol.name == "i32" && matches!(t2.symbol.name.as_str(), "i32" | "i16" | "i8" | "u8" | "u32" | "u16")) ||
        (t1.symbol.name == "i16" && matches!(t2.symbol.name.as_str(), "i16" | "i8" | "u8" | "u16")) ||
        (t1.symbol.name == "i8" && matches!(t2.symbol.name.as_str(), "i8" | "u8")) ||
        (t1.symbol.name == "u64" && matches!(t2.symbol.name.as_str(), "u64" | "u32" | "u16" | "u8")) ||
        (t1.symbol.name == "u32" && matches!(t2.symbol.name.as_str(), "u32" | "u16" | "u8")) ||
        (t1.symbol.name == "u16" && matches!(t2.symbol.name.as_str(), "u16" | "u8")) ||
        (t1.symbol.name == "f64" && matches!(t2.symbol.name.as_str(), "f32" | "f64" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16")) ||
        (t1.symbol.name == "f32" && matches!(t2.symbol.name.as_str(), "f32" | "i64" | "i32" | "i16" | "i8" | "u8" | "u64" | "u32" | "u16"))
    }
    pub fn is_compatible(&self, t2: &TypeHolder) -> bool {
        Self::compatible_types(self, t2)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeValueHolder {
    pub type_holder: Rc<TypeHolder>,
    pub type_modifiers: Vec<TypeModifier>,
}

#[derive(Debug, Serialize)]
pub struct VariableHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: TypeValueHolder,
    pub has_value: bool,
    pub requires_free: bool,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct ParameterHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: TypeValueHolder,
    pub default_value: Option<Box<ASTNode>>,
    pub is_params: bool,
    pub is_const: bool,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct FunctionHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: TypeValueHolder,
    pub has_body: bool,
    pub type_parameters: Vec<Rc<TypeHolder>>,
    pub parameters: Vec<ParameterHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct TraitHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: Rc<TypeHolder>,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub inherits: Vec<Rc<TypeHolder>>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct EnumMemberHolder {
    pub symbol: Rc<Symbol>,
    pub index: Option<u32>,
}

#[derive(Debug, Serialize)]
pub struct EnumHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: Rc<TypeHolder>,
    pub members: Vec<EnumMemberHolder>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct StructHolder {
    pub symbol: Rc<Symbol>,
    pub ttype: Rc<TypeHolder>,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub structs: Vec<StructHolder>,
    pub enums: Vec<EnumHolder>,
    pub type_parameters: Vec<Rc<TypeHolder>>,
    pub access_modifier: Vec<AccessModifier>,
    pub tags: Vec<Tag>,
    pub inherits: Vec<Rc<TypeHolder>>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct ModuleHolder { 
    pub symbol: Rc<Symbol>,
    pub methods: Vec<FunctionHolder>,
    pub members: Vec<VariableHolder>,
    pub structs: Vec<StructHolder>,
    pub traits: Vec<TraitHolder>,
    pub enums: Vec<EnumHolder>,
    pub location: Location,
}

////////////////////////////////////////////////////////////////////////////////////
// Codegen Table
////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Serialize)]
pub struct CodegenTable {
    pub types: Vec<Rc<TypeHolder>>,
    pub enums: Vec<EnumHolder>,
    pub structs: Vec<StructHolder>,
    pub traits: Vec<TraitHolder>,
    pub functions: Vec<FunctionHolder>,
    pub variables: Vec<VariableHolder>,
    pub modules: Vec<ModuleHolder>,
}

impl CodegenTable {
    pub fn new() -> Self {
        Self {
            types: vec![],
            enums: vec![],
            structs: vec![],
            traits: vec![],
            functions: vec![],
            variables: vec![],
            modules: vec![],
        }
    }
}
