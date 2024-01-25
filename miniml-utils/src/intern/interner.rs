use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());
