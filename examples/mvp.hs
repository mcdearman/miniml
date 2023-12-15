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

-- classes 
class Point (x : Int = 0) (y : Int = 0) <: ToString = 
  move dx dy = Point (x + dx) (y + dy)

  override toString () = f"Point(x: {x}, y: {y})"

-- sexpr
-- (class Point ((x Int 0) (y Int 0)) (<: ToString) 
--   (

-- generic classes
class Stack T (xs : List T)

impl Stack =
  Stack x = Stack [x]

  push (x : T) = Stack x :: xs

  pop () = 
    match this.xs with
    | [] -> None
    | [x::xs] -> Some x (Stack xs)

class List T =
  Empty
  Pair T (List T)

-- examples
gcd a b = 
  if b == 0 then a 
  else gcd b (a % b)

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

-- multiline
gcd a b = 
  match b with 
  | 0 -> a 
  | _ -> gcd b (a % b)

fib n = 
  if n <= 1 then n 
  else fib (n - 1) + fib (n - 2)





