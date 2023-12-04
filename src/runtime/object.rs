use crate::util::intern::InternedString;

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub name: InternedString,
    pub fields: Vec<ObjectField>,
    pub methods: Vec<ObjectMethod>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectField {
    pub name: InternedString,
    pub value: Object,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectMethod {
    pub name: InternedString,
    pub args: Vec<Object>,
    pub body: Object,
}
