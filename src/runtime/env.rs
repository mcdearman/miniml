use super::value::Value;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

pub struct Env {
    frames: Vec<HashMap<UniqueId, Value>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            frames: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.frames.push(HashMap::new());
    }

    pub fn def(&mut self, id: UniqueId, value: Value) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(id, value);
        } else {
            let mut frame = HashMap::new();
            frame.insert(id, value);
            self.frames.push(frame);
        }
    }

    pub fn get(&self, id: &UniqueId) -> Option<Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(id) {
                return Some(value.clone());
            }
        }
        None
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }
}
