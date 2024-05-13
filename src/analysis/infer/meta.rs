use super::{
    error::{InferResult, TypeError},
    meta_context::MetaContext,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::fmt::{Debug, Display};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Meta {
    Bound(Box<Type>),
    Unbound(UniqueId),
}

impl Meta {
    pub fn fresh() -> Self {
        Self::Unbound(UniqueId::gen())
    }

    pub fn id(&self) -> Option<UniqueId> {
        match self {
            Self::Bound(_) => None,
            Self::Unbound(id) => Some(*id),
        }
    }
}

impl Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[rustfmt::skip]
const ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 
    'g', 'h', 'i', 'j', 'k', 'l', 
    'm', 'n', 'o', 'p', 'q', 'r', 
    's', 't', 'u', 'v', 'w', 'x', 
    'y', 'z',
];

impl Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bound(ty) => write!(f, "{}", ty),
            Self::Unbound(id) => {
                let id = usize::from(*id);
                if id < ALPHABET.len() {
                    write!(f, "{}", ALPHABET[id])
                } else {
                    write!(
                        f,
                        "{}{}",
                        ALPHABET[id / ALPHABET.len() - 1],
                        (id + 1) % ALPHABET.len()
                    )
                }
            }
        }
    }
}
