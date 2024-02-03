# Bootstrap

This is the base tree-walking evaluator for MiniML. Currently, it's written in Rust.
To bootstrap, we evaluate a compiler written in MiniML then use the in-memory compiler
to compile itself, giving us a binary.