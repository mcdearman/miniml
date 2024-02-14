#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(UniqueId);

impl TyVar {
    pub fn fresh() -> Self {
        Self(UniqueId::gen())
    }
}

impl From<UniqueId> for TyVar {
    fn from(id: UniqueId) -> Self {
        Self(id)
    }
}

impl From<usize> for TyVar {
    fn from(id: usize) -> Self {
        Self(UniqueId::from(id))
    }
}

impl Debug for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TyVar({})", self)
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

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = usize::from(self.0);
        if id < ALPHABET.len() {
            write!(f, "'{}", ALPHABET[id])
        } else {
            write!(
                f,
                "'{}{}",
                ALPHABET[id / ALPHABET.len() - 1],
                (id + 1) % ALPHABET.len()
            )
        }
    }
}
