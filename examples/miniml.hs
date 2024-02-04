-- Bootstrapped compiler

-- utils
mod Span =
  pub type Span = {
    start : Int,
    end : Int,
  }

  pub let spanToString span src = 
    let start = span.start
    let end = span.end
    String.sub src start (end - start)

  pub let spanToLineCol span src = 
    let spanToLineCol' pos line col =
      if pos >= span.start then
        (line, col)
      else if src[pos] = '\n' then
        spanToLineCol' (pos + 1) (line + 1) 0
      else
        spanToLineCol' (pos + 1) line (col + 1)
    in
    spanToLineCol' 0 0 0

  pub let spanToLineColString span src = 
    let (line, col) = spanToLineCol span src
    f"{Int.toString line}:{Int.toString col}"
end

-- Lexer
mod Lexer =
  pub type Token = {
    kind : TokenKind,
    span : Span,
  }

  pub type TokenKind
    = Eof
    | Whitespace
    | Comment
    | Ident String
    | Int Int
    | Float Float
    | Rational Rational
    | Complex Complex
    | BigInt BigInt
    | BigRational BigRational
    | String String
    | Char Char
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | Caret
    | Backslash
    | Arrow
    | FatArrow
    | Pipe
    | PipeArrow
    | Eq
    | Lt
    | Gt
    | Neq
    | Leq
    | Geq
    | LParen
    | RParen
    | LBrack
    | RBrack
    | LBrace
    | RBrace
    | Comma
    | Period
    | Colon
    | Semicolon

  pub type Span = {
    start : Int,
    end : Int,
  }

  pub type Lexer = {
    src : String,
    pos : Int,
  }

  pub let initLexer src = Lexer {
    src = src,
    pos = 0,
  }

  let isWhitespace c = c = ' ' or c = '\t' or c = '\n' or c = '\r'

  let isDigit c = c >= '0' and c <= '9'

  let consumeWhitespace lexer =
    let consumeWhitespace' pos =
      if pos < String.length lexer.src and 
        (lexer.src[pos] = ' ' or 
        lexer.src[pos] = '\t' or 
        lexer.src[pos] = '\n') then
        consumeWhitespace' (pos + 1)
      else
        pos
    in
    { lexer with pos = consumeWhitespace' lexer.pos }

  let peekChar lexer = 
    if lexer.pos < String.length lexer.src then
      Some lexer.src[lexer.pos]
    else
      None

  let nextToken (lexer : Lexer) : Token = 
    match peekChar lexer with
    | None -> { kind = Eof, span = { start = lexer.pos, end = lexer.pos } }
    | Some c -> 
      if isWhitespace c then
        let start = lexer.pos
        let lexer' = consumeWhitespace lexer
        { kind = Whitespace, span = { start, end = lexer'.pos } }
      else
        let start = lexer.pos
        let end = lexer.pos + 1
        let lexer' = { lexer with pos = lexer.pos + 1 }
        match c with
        | '+' -> { kind = Plus, span = Span { start, end } }
        | '-' -> { kind = Minus, span = { start, end } }
        | '*' -> { kind = Star, span = { start, end } }
        | '/' -> { kind = Slash, span = { start, end } }
        | '%' -> { kind = Percent, span = { start, end } }
        | '^' -> { kind = Caret, span = { start, end } }
        | '\\' -> { kind = Backslash, span = { start, end } }
        | '(' -> { kind = LParen, span = { start, end } }
        | ')' -> { kind = RParen, span = { start, end } }
        | '[' -> { kind = LBrack, span = { start, end } }
        | ']' -> { kind = RBrack, span = { start, end } }
        | '{' -> { kind = LBrace, span = { start, end } }
        | '}' -> { kind = RBrace, span = { start, end } }
        | ',' -> { kind = Comma, span = { start, end } }
        | '.' -> { kind = Period, span = { start, end } }
        | ':' -> { kind = Colon, span = { start, end } }
        | ';' -> { kind = Semicolon, span = { start, end } }
        | _ -> error "unimplemented"
end

