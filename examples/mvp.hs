-- Files are composed of declarations at the top level. In the notebook 
-- or repl, you can use expressions at the top level. You can think of 
-- this as a shorthand for `let main args = <expr>`. In files, the main 
-- function must be defined explicitly:
let main args = 1

-- Bindings are immutable by default.
let x = 1

-- You can use `var` to declare mutable bindings.
var x = 1

-- You can use `pub` to declare public bindings.
pub let x = 1

-- strings
"hello, world!"

-- characters
'c'

-- ints
1

-- reals
1.0

-- rationals
1/2

-- -- complex
-- 1.0 + 2.0i

-- -- bigints
-- 1n

-- -- bigrationals
-- 1/2n

-- booleans
true

-- lists
[1, 2, 3]

-- tuples
(1, 2)

-- ranges
1..10

-- inclusive ranges
1..=10

-- range with step
1..2..10

-- list comprehension
[x * 2 | x <- [1, 2, 3]]

-- Binary operators
1 + 2 * 3^2 - 4 / 5 % 10

-- Unary operators
-1
-- => -1

not true
-- => false

-- Boolean operators
true and false or true
-- => true

-- Comparison
1 < 2 and 2 <= 3 and 3 > 2 and 3 >= 2 and 2 = 2 and 2 != 3

-- compare characters
'a' < 'b'
'0' < '1'

-- if-then-else
if 1 < 2 then 1 else 2

-- let-in
let x = 1 in x + 1

-- function definition
let f x = x + 1

-- this is equivalent to
let f = \x -> x + 1

-- function expression
let f x = x + 1 in f 1

-- function application
f 1

-- closure
\x -> x + 1

-- closure application
(\x -> x + 1) 1

-- nested let
let x = 1 in let y = 2 in x + y

-- recursion
let fib n = 
  if n < 2 then 1 
  else fib (n - 1) + fib (n - 2) 
in fib 10

-- tail recursion
let fib n = 
  let loop n a b = 
    if n = 0 then a 
    else loop (n - 1) b (a + b) 
  in loop n 0 1 

-- pattern matching
let map f xs = match xs with
  | [] -> []
  | [y::ys] -> f y :: map f ys

let map_iter f xs = 
  let loop xs acc = match xs with
    | [] -> acc
    | x::xs -> loop xs (f x :: acc)

let gcd a b = if b = 0 then a else gcd b (a % b)

-- record
type Point = Point { x : Num, y : Num }
let p = Point { x = 1, y = 2 }
p.x

-- product type
type Point = Point Num Num

-- sum type
type Shape 
  = Circle Num 
  | Rectangle Num Num

-- Modules are automatically defined by the file name.
-- You can also define modules explicitly:
module Math = 
  let add x y = x + y
  let sub x y = x - y
end

Math.add 1 2

-- You can bring names from a module into scope with `use`:
use Math.add
add 1 2

-- You can also use `use` to bring all names into scope:
use Math
add 1 2
sub 1 2