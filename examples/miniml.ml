-- Bootstrapped compiler
-- Lexer
type Token = {
  kind : TokenKind,
  span : Span,
}

type TokenKind
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

type Span = {
  start : Int,
  end : Int,
}

type Lexer = {
  src : String,
  pos : Int,
}

let initLexer src = {
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
      { kind = Whitespace, span = { start = start, end = lexer'.pos } }
    else
      let start = lexer.pos
      let end = lexer.pos + 1
      let lexer' = { lexer with pos = lexer.pos + 1 }
      match c with
      | '+' -> { kind = Plus, span = { start = start, end = end } }
      | '-' -> { kind = Minus, span = { start = start, end = end } }
      | '*' -> { kind = Star, span = { start = start, end = end } }
      | '/' -> { kind = Slash, span = { start = start, end = end } }
      | '%' -> { kind = Percent, span = { start = start, end = end } }
      | '^' -> { kind = Caret, span = { start = start, end = end } }
      | '\\' -> { kind = Backslash, span = { start = start, end = end } }
      | '(' -> { kind = LParen, span = { start = start, end = end } }
      | ')' -> { kind = RParen, span = { start = start, end = end } }
      | '[' -> { kind = LBrack, span = { start = start, end = end } }
      | ']' -> { kind = RBrack, span = { start = start, end = end } }
      | '{' -> { kind = LBrace, span = { start = start, end = end } }
      | '}' -> { kind = RBrace, span = { start = start, end = end } }
      | ',' -> { kind = Comma, span = { start = start, end = end } }
      | '.' -> { kind = Period, span = { start = start, end = end } }
      | ':' -> { kind = Colon, span = { start = start, end = end } }
      | ';' -> { kind = Semicolon, span = { start = start, end = end } }
      | _ -> error "unimplemented"

