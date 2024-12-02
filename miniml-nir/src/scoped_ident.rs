use super::res_id::ResId;
use miniml_utils::intern::InternedString;

#[derive(Debug, Clone, Copy, Eq, PartialOrd, Ord)]
pub struct ScopedIdent {
    pub id: ResId,
    pub name: InternedString,
}

impl ScopedIdent {
    pub fn new(id: ResId, name: InternedString) -> Self {
        Self { id, name }
    }
}

impl std::fmt::Display for ScopedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.name, self.id)
    }
}

impl PartialEq for ScopedIdent {
    fn eq(&self, other: &ScopedIdent) -> bool {
        self.id == other.id
    }
}
