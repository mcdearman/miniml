#[derive(Debug, Clone, PartialEq)]
pub struct RecordDef {
    pub name: InternedString,
    pub fields: Vec<(InternedString, Type)>,
}