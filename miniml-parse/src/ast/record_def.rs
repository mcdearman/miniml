#[derive(Debug, Clone, PartialEq)]
pub struct RecordDef {
    name: InternedString,
    fields: Vec<(InternedString, Type)>,
}

pub enum DataType {
    Record {
        name: InternedString,
        fields: Vec<(InternedString, TypeHint)>,
    },
    Sum {
        name: InternedString,
        variants: Vec<(InternedString, Vec<TypeHint>)>,
    },
    Product {
        name: InternedString,
        fields: Vec<TypeHint>,
    },
}
