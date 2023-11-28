-- print "Hello World!" 
println "Hello World!"

-- Arithmetic
1 + 1
21 - 2
2 * 2
4 / 2
5 % 2
6^2

-- if/else
if x == y 
  1
else if x == z 
  2
else 3

-- lambda expression
\a b -> a + b

-- lambda application
(\a b -> a + b) 1 2

-- maps
{ x: 1, y: 2 }

-- lists
[1, 2, 3]

-- sets
{1, 2, 3}

-- Symbols are identifiers whose value is their name.
:foo

-- definition
x = 1

-- you can define functions
add x y = x + y

-- recursion
gcd a b =
  if b == 0 then a 
  else gcd b (a % b)

-- lambdas can be assigned to variables
gcd = \a b ->
  if b = 0 then a 
  else gcd b (a % b)

-- You can define functions by pattern matching
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- or by using the `match` expression
fib n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

map f xs = 
  match xs with
  | [] -> []
  | [x::xs] -> f x :: map f xs

class Node = Node Int (Maybe Node)

binaryListToDecimal (Node dataValue rest) = 
  match rest with
  | Nothing -> 0
  | Just rest -> binaryListToDecimal rest * 2 + dataValue

let y = 2 in (\x -> x + y) 1

-- partial application with wildcard
add1 = add 1 _

-- wildcard can be in any position
add1 = add _ 1

-- struct literals
Point { x = 1, y = 2 }

-- struct access
Point { x = 1, y = 2 }.x

-- Partial application is just an operator that returns
-- a lambda. The line above is equivalent to:
add1 = \x -> add x 1

-- list pattern
[a, b, c] = [1, 2, 3]

-- pair pattern
[head::tail] = [1, 2, 3]

-- list pair pattern
[first, head::tail] = [1, 2, 3]

-- record pattern
Point { x, y } = Point { x = 1, y = 2 }

-- map pattern
{ x, y } = { x = 1, y = 2 }

-- Data constructors for a type `'a` are just functions of the
-- same name that take arguments and return a value of type `'a`.
class Point = Point Num Num

-- You can define records with `data`.
class Point = Point { x: Num, y: Num }

-- Booleans are just sum types.
class Bool = True | False

-- You can use `let` to bind values to names.
let x = 1
    y = 2
in  x + y

-- You can use `begin` to group items together.
-- The value of a `begin` expression is the value of the last expression.
begin
  y = 1
  z = 2
  y + z
-- miniml> 3

-- Classes
class Ord <: Eq + PartialOrd = 
  cmp : [self, self] -> Ordering

class Map K V = 
  empty : Map K V
  find : K -> V
  insert : [K, V] -> Map K V
  delete : K -> Map K V

data List (I = Nat) V <: Map I V 
  = Empty 
  | Pair I V (List I V)

class List (I = Nat) V <: Map I V
  = Empty
  | Pair I V (List I V)

  empty : List I V
  find : I -> V
  insert : [I, V] -> List I V
  delete : I -> List I V

class Point = 
  x : Num
  y : Num

-- Metaprogramming
-- You can use the `quote` function to get the AST of an expression.
-- Quoting can also be done with the `:` operator. Symbols are just quoted
-- identifiers.
ast = quote (1 + 1)
ast = :(1 + 1)

-- You can use the `unquote` function to splice an AST into an expression.
-- Unquoting can also be done with the `$` operator.
a = 1
quote (1 + unquote 1)
-- miniml> :(1 + 1)
:(1 + $(a))
-- miniml> :(1 + 1)

-- Sometimes you might want to quote the unquote operator without
-- interpolating. You can do this with the `~` operator.
quote (1 + ~1)
-- miniml> :(1 + $(1))

-- You can use the `eval` function to evaluate an AST.
eval :(1 + a)
-- miniml> 2

-- You can use the `compile` function to compile an AST to a function.
add = compile :(\a b -> a + b)

-- You can use the `parse` function to parse a string into an AST.
ast = parse "1 + 1"

-- You can use the `show_sexpr` function to display an AST as an s-expression.
show_sexpr :(1 + 1)
-- miniml> '(+ 1 1)

show_sexpr :(let x = 1 in x + 1)
-- miniml> '(let ((x 1)) (+ x 1))

show_sexpr 
  :(fib 0 = 0 
      | 1 = 1 
      | n = fib (n - 1) + fib (n - 2))
-- miniml> '(let ((fib (lambda (n) (match n (0 0) (1 1) (n (+ (fib (- n 1)) (fib (- n 2))))))))

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

macro match expr with... = 
  