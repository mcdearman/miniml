```mermaid
classDiagram
    Compiler : +StringInterner interner
    Compiler : +Lexer lexer
    Compiler : +Parser parser
    Compiler : +NameResolver res
    Compiler : +InferenceEngine inf
    Object <|-- Compiler
    Object <|-- StringInterner
    Object <|-- Node
    Object <|-- File
    Object <|-- Span
    Object <|-- UniqueId
    Object <|-- CompilerPass

    CompilerPass <|-- Lexer
    CompilerPass <|-- Parser
    CompilerPass <|-- ContextHandler
    CompilerPass <|-- NameResolver
    CompilerPass <|-- InferenceEngine

    Node : +Span span
    Node : +File file
    Node <|-- Token
    Node <|-- SyntaxNode
    Node <|-- NameResNode
    Node <|-- InferenceNode
```
