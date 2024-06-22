use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar {
    id: u32,
}

impl TyVar {
    pub fn new(id: u32) -> Self {
        Self { id }
    }

    pub fn id(&self) -> u32 {
        self.id
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

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let id = self.id as usize;
        if id < ALPHABET.len() {
            write!(f, "{}", ALPHABET[id])
        } else {
            write!(
                f,
                "{}{}",
                ALPHABET[id as usize / ALPHABET.len() - 1],
                (id + 1) % ALPHABET.len()
            )
        }
    }
}
