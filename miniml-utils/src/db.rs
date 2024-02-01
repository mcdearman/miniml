use std::collections::{BTreeMap, BTreeSet};

use crate::{interned_string::InternedString, span::Span};

#[derive(Debug, Clone)]
pub struct Database {
    name: InternedString,
    tables: Vec<Table>,
    indexes: BTreeMap<Value, BTreeSet<IndexEntry>>,
}

impl Database {
    pub fn new(name: &str) -> Database {
        Database {
            name: InternedString::from(name),
            tables: vec![],
            indexes: BTreeMap::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn tables(&self) -> &Vec<Table> {
        &self.tables
    }

    pub fn add_table(&mut self, table: Table) {
        self.tables.push(table);
    }

    pub fn remove_table(&mut self, table_name: &str) {
        self.tables.retain(|table| table.name() != table_name);
    }

    // pub fn add_index(&mut self, table_name: &str, columns: Vec<&str>) {
    //     let table_name = InternedString::from(table_name);
    //     let columns = columns
    //         .iter()
    //         .map(|name| InternedString::from(*name))
    //         .collect::<Vec<_>>();
    //     let entry = IndexEntry { table: table_name, columns };
    //     for table in &self.tables {
    //         if table.name() == table_name {
    //             for column in &columns {
    //                 for row in table.columns() {
    //                     if row.name() == *column {
    //                         let value = row.data_type().clone();
    //                         self.indexes
    //                             .entry(value)
    //                             .or_insert_with(BTreeSet::new)
    //                             .insert(entry.clone());
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }
}

#[derive(Debug, Clone)]
pub struct Table {
    name: InternedString,
    columns: Vec<Column>,
}

impl Table {
    pub fn new(name: &str) -> Table {
        Table {
            name: InternedString::from(name),
            columns: Vec::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn columns(&self) -> &Vec<Column> {
        &self.columns
    }

    pub fn add_column(&mut self, column: Column) {
        self.columns.push(column);
    }
}

#[derive(Debug, Clone)]
pub struct Column {
    name: InternedString,
    value: Value,
}

impl Column {
    pub fn new(name: &str, value: Value) -> Column {
        Column {
            name: InternedString::from(name),
            value,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data_type(&self) -> &Value {
        &self.value
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Text(InternedString),
    Bool(bool),
    Span(Span),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct IndexEntry {
    table: InternedString,
    columns: Vec<InternedString>,
}
