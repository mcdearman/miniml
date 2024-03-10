// use super::{span::Span, unique_id::UniqueId};

// #[derive(Debug, Clone, PartialEq)]
// pub struct ItemStore {
//     items: Vec<Box<dyn Item>>,
// }

// impl ItemStore {
//     pub fn new() -> Self {
//         Self { items: Vec::new() }
//     }

//     pub fn items(&self) -> &[impl Item] {
//         &self.items.iter().map(|item| item.as_ref())
//     }

//     pub fn add_item(&mut self, item: impl Item) {
//         self.items.push(item);
//     }
// }

// pub trait Item {
//     fn id(&self) -> UniqueId;
//     fn span(&self) -> Span;
// }
