use std::{
    any::Any,
    borrow::Borrow,
    collections::HashMap,
    hash::{Hash, Hasher},
};

struct Database {
    values: HashMap<Stored, ()>,
}

pub trait Query {
    type Output;
    fn exec(&self, db: &mut Database) -> Self::Output;
}

pub struct DbEntry<T: Query> {
    v: T,
    output: <T as Query>::Output,
}

pub trait ErasedDbEntry {
    fn hash_query(&self, hasher: &mut dyn Hasher);
    fn eq_query(&self, other: &dyn ErasedDbEntry) -> bool;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Query + Hash + Eq + 'static> ErasedDbEntry for DbEntry<T> {
    fn hash_query(&self, mut hasher: &mut dyn Hasher) {
        self.v.hash(&mut hasher);
    }
    fn eq_query(&self, other: &dyn ErasedDbEntry) -> bool {
        other
            .as_any()
            .downcast_ref::<DbEntry<T>>()
            .map_or(false, |other| self.v == other.v)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Stored {
    content: Box<dyn ErasedDbEntry>,
}

impl PartialEq for Stored {
    fn eq(&self, other: &Self) -> bool {
        self.content.eq_query(&*other.content)
    }
}

impl Eq for Stored {}

impl Hash for Stored {
    fn hash<H: Hasher>(&self, mut hasher: &mut H) {
        self.content.hash_query(&mut hasher);
    }
}

impl<T: Query + 'static> Borrow<T> for Stored {
    fn borrow(&self) -> &T {
        &self
            .content
            .as_any()
            .downcast_ref::<DbEntry<T>>()
            .unwrap()
            .v
    }
}

impl Database {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
    fn exec<T: Query + Hash + Eq + 'static>(&mut self, query: T) -> &T::Output
    where
        Stored: Borrow<T>,
    {
        let mut out = self.values.get_key_value(&query);
        if out.is_some() {
            let r: &T::Output = &out
                .unwrap()
                .0
                .content
                .as_any()
                .downcast_ref::<DbEntry<T>>()
                .unwrap()
                .output;
            return unsafe { core::mem::transmute(r) };
        }
        let output = query.exec(self);
        let inserted = self
            .values
            .entry(Stored {
                content: Box::new(DbEntry { v: query, output }),
            })
            .insert_entry(());
        let entry: &Stored = inserted.key();
        let entry: &Stored = unsafe { core::mem::transmute(entry) }; // need OccupiedEntry::into_key
        return &entry
            .content
            .as_any()
            .downcast_ref::<DbEntry<T>>()
            .unwrap()
            .output;
    }
}
