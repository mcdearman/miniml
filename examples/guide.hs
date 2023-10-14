-- let bindings
let x = 1

-- MiniML is a scripting language but there are no top level expressions.
-- Every MiniML script must have a main function that serves as the entry point.
let main = 
  -- You can use `;` to chain expressions together.
  -- You can think of this as a binary op that always 
  -- discards the left hand side.
  -- It's equivalent to `let _ = ... in ...`
  println "Hello World!";

  -- You can use `let` to bind values to names.
  let x = 1 in println x;

  -- Arithmetic
  1 + 1;
  21 - 2;
  2 * 2;
  4 / 2;
  5 % 2;
  6^2;

  -- elif
  if x = y then 1 
  elif x = z 
  then 2 else 3;

  -- lambda expression
  fn a b -> a + b;

  -- lambda application
  (fn a b -> a + b) 1 2;

  -- maps
  { x = 1, y = 2 };

  -- struct literals
  Point { x = 1, y = 2 };

  -- struct access
  Point { x = 1, y = 2 }.x;

  -- lists
  [1 2 3];

  -- sets
  { 1 2 3 }; 

  -- multidimensional lists
  [1 2, 3 4];

  -- Symbols are identifiers whose value is their name.
  :foo;

  -- main function must return unit or int
  ()
  
-- The only declarations that can appear outside of functions 
-- are type definitions and global variables.

-- Data constructors for a type `'a` are just functions of the
-- same name that take arguments and return a value of type `'a`.
data Point = Point Num Num

-- You can define records with `data`.
data Point = Point { x: Num, y: Num }

-- Booleans are just sum types.
data Bool = True | False

-- global function definition
let add x y = x + y

-- recursion
let gcd a b =
  if b = 0 then a 
  else gcd b (a % b)

-- lambdas can be assigned to variables
let gcd = fn a b ->
  if b = 0 then a 
  else gcd b (a % b)

-- You can define functions by pattern matching
let fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

-- or by using the `match` expression
let fib n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let map f [] = []
  | map f [x::xs] = f x :: map f xs

-- partial application with wildcard
let add1 = add 1 _

-- wildcard can be in any position
let add1 = add _ 1

-- Partial application is just an operator that returns
-- a lambda. The line above is equivalent to:
let add1 = fn x -> add x 1

-- list pattern
let [a b c] = [1 2 3]

-- pair pattern
let [head::tail] = [1 2 3]

-- list pair pattern
let [first, head::tail] = [1 2 3]

-- record pattern
let Point { x, y } = Point { x = 1, y = 2 }

-- map pattern
let { x, y } = { x = 1, y = 2 }

-- You can use `begin` to group items together.
-- The value of a `begin` expression is the value of the last expression.
begin
  let y = 1
  let z = 2
  y + z
-- miniml> 3

-- Classes
class Ord <: Eq + PartialOrd = 
  let cmp self other = raise :NotImplementedError

class List (I = Nat) V <: Map I V =
  let Nil = Nil
  let Pair (x: Nat) (xs: Nat) = Pair x xs

  let map f Nil = Nil
    | map f (Pair x xs) = Pair (f x) (map f xs)

data List (I = Nat) V = Nil | Pair I (List I V)

-- Metaprogramming
-- You can use the `quote` function to get the AST of an expression.
-- Quoting can also be done with the `:` operator. Symbols are just quoted
-- identifiers.
let ast = quote (1 + 1)
let ast = :(1 + 1)

-- You can use the `unquote` function to splice an AST into an expression.
-- Unquoting can also be done with the `$` operator.
quote (1 + unquote (quote 1))
-- miniml> :(1 + 1)
:(1 + $(:(1)))
-- miniml> :(1 + 1)

-- You can use the `eval` function to evaluate an AST.
eval :(1 + 1)
-- miniml> 2

-- You can use the `compile` function to compile an AST to a function.
let add = compile :(fn a b -> a + b)

-- You can use the `parse` function to parse a string into an AST.
let ast = parse "1 + 1"

-- You can use the `show_sexpr` function to display an AST as an s-expression.
show_sexpr :(1 + 1)
-- miniml> '(+ 1 1)

show_sexpr :(let x = 1 in x + 1)
-- miniml> '(let ((x 1)) (+ x 1))

show_sexpr :(let fib 0 = 0 
               | fib 1 = 1 
               | fib n = fib (n - 1) + fib (n - 2))
-- miniml> '(let 


-- Macros
-- You can define macros with the `macro` keyword.
macro when cond body... =
  :(if $(cond) then $(body) else ())

macro unless cond body =
  :(if $(cond) then () else $(body))

macro begin body... =
  :(let _ = $(body) in ())

-- (macro (while condition . body)
--   `(let loop ()
--      (cond (,condition
-- 	    (begin . ,body)
-- 	    (loop)))))

macro while cond body... =
  :(let loop () = 
      (if $(cond) then (begin $(body); loop) else ()))

macro while cond body =
  :(if $(cond) then $(body); while $(cond) $(body) else ())

