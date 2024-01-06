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
fib : Nat -> Nat
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- or by using the `match` expression
fib n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

map : (a -> b) -> [a] -> [b]
map f [] = []
map f [x::xs] = f x :: map f xs

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

class Main = 
  main args = println "Hello World!"

-- Classes
class Ord <: Eq PartialOrd = 
  cmp : (self, self) -> Ordering

class Collection = 
  empty : Collection
  insert : (self, a) -> Collection
  delete : (self, a) -> Collection
  contains : (self, a) -> Bool

class Tree a <: Collection = 
  empty : Tree a
  insert : a -> Tree a
  delete : a -> Tree a
  contains : a -> Bool

class Map K V = 
  empty : Map K V
  find : K -> V
  insert : (K, V) -> Map K V
  delete : K -> Map K V

class List V <: Map Nat V =
  push : V -> List V
  pop : () -> List V 
  get : Nat -> V

class LinkedList (I = Nat) V <: List I V
  = Empty
  | Pair I V (List I V)

  empty : List I V
  find : I -> V
  insert : (I, V) -> List I V
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
  @(if $(cond) then $(body) else ())

macro unless cond body =
  @(if $(cond) then () else $(body))

macro begin body... =
  @(let _ = $(body) in ())

-- (macro (while condition . body)
--   `(let loop ()
--      (cond (,condition
-- 	    (begin . ,body)
-- 	    (loop)))))

macro while cond body... =
  @(let loop () = 
      (if $(cond) then (begin $(body) loop) else ()))

while i < 10
  println i
  i = i + 1

-- expanded
let loop () = 
  if i < 10 then
    begin
      println i
      loop ()
  else
    ()

-- Main entry point
class Main =
  main args: () = begin
    let _ = println "Hello World!" in
    let _ = loop 0 in
    ()

  loop i =
    if i > 30 then ()
    else let _ = print (fib i) in loop (i + 1)

  fib n = 
    if n <= 1 then n 
    else fib (n - 1) + fib (n - 2)

-- arithmetic operators
1 + 2
1 - 2
1 * 2
1 / 2
1 % 2

-- comparison operators
1 == 2
1 != 2
1 < 2
1 <= 2

-- strings
"hello world"

-- lambda
\a b -> a + b

-- function application
f x

-- lambda application
(\a b -> a + b) 1 2

-- value definition
x = 1

-- function definition
f x = x + 1

-- negation
-x

-- not
!x

-- let expression
let x = 1 in x + 1

-- let expression function definition
let f x = x + 1 in f 1

-- if expression
if x then y else z

-- match expression
match x with
| y -> z

-- type hints
gcd (a : Int) (b : Int) : Int = 
  if b == 0 then a 
  else gcd b (a % b)
  
-- lists
[1, 2, 3]

-- ranges
1..10

-- product types
class Point = 
  x : Int
  y : Int

-- sum types
class Option T = 
  None
  Some T

-- class with methods
class Point = 
  x : Int
  y : Int

  move dx dy: () = begin
    x <- x + dx
    y <- y + dy
  end


-- generic classes
class Stack T (var xs : List T) =
  Stack x = begin 
    xs <- x :: xs

  push (x : T) = begin 
    xs <- x :: xs

  pop () = 
    match xs with
    | [] -> None
    | [x::xs] -> Some x (Stack xs)

class List T =
  Empty
  Pair T (List T)

-- examples
gcd a b = 
  if b == 0 then a 
  else gcd b (a % b)

gcd a b =
  if b == 0 then
    a
  else
    gcd b (a % b)

fib n = 
  if n <= 1 then n 
  else fib (n - 1) + fib (n - 2)

ack m n = 
  if m == 0 then n + 1 
  else if n == 0 then ack (m - 1) 1 
  else ack (m - 1) (ack m (n - 1))

loop i j = 
  if i > 5 then () 
  else if j > 5 then loop (i + 1) 0 
  else let _ = print (ack i j) in loop i (j + 1)

loop i = 
  if i > 30 then () 
  else let _ = print (fib i) in loop (i + 1)

gcd a b = 
  match b with 
  | 0 -> a 
  | _ -> gcd b (a % b)

map f xs = 
  match xs with 
  | [] -> [] 
  | [x::xs] -> f x :: map f xs

class Point (var x : Int = 0) (var y : Int = 0) <: ToString = 
  move dx dy: () = begin
    x <- x + dx
    y <- y + dy
  end

  impl ToString =
    toString (): String = f"Point(x: {x}, y: {y})"

class Node T =
  inner : T
  span : Int

enum List T = Nil
  | Pair { head: T, tail: List T }

-- [======================== Object Orientation ======================]
-- MiniML is a pure Object Oriented language. This means that everything is an object.
-- And the only way to interact with objects is through messages. Messages are sent
-- to objects by calling methods. Methods are defined in classes.


-- [======================== Constructors ======================]
-- Constructors in MiniML are different from ordinary constructors.
-- They are akin to Rust struct initializers. However, they share syntax
-- with ordinary functions. A constructor is a function with the name of
-- the type it constructs. They are not methods as they don't take `self`.
-- Nor can they modify the state of the object. Instead, they use initialization
-- syntax to initialize the object. All fields must be initialized. If a field 
-- is intended to sometimes be empty, it must be wrapped in an `Option`.
-- By default, all fields are immutable. To make a field mutable, it must be
-- declared with the `mut` keyword. The `mut` keyword is also used to declared
-- mutable variables in functions. A private initializer with all fields as 
-- arguments will be created by default. A public initializer with all
-- public fields as arguments will be created by default as well. You can also
-- create your own initializers (constructors).
class Rectangle <: ToString = 
  -- private fields
  mut width : Int
  mut height : Int
  mut area: Int = width * height

  -- public initializer
  -- default private initializer used in the public initializer
  Rectangle (size : Int) = Rectangle size size (size * size)

  updateDimensions self newWidth newHeight: () = begin
    self.width = newWidth
    self.height = newHeight
    self.area = width * height
  end

  impl ToString =
    toString (): String = f"Rectangle(width: {width}, height: {height}, area: {area})"

