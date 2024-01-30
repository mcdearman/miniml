/*
 * This crate resolves names in the AST and produces an IR similar
 * to the AST but with all names resolved to their unique IDs. Names
 * that shadow names from an outer scope are given a new unique ID.
 */

pub mod env;
pub mod error;
pub mod nir;
pub mod resolver;
