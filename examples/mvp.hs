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

-- lists
[1, 2, 3]
1..10

-- classes 
class Point =
  x : Int
  y : Int

  impl Point =
    move dx dy = Point (x + dx) (y + dy)

  impl ToString for Point =
    toString = f"Point(x: {x}, y: {y})"
    

-- examples
gcd a b = if b == 0 then a else gcd b (a % b)

fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

ack m n = if m == 0 then n + 1 else if n == 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n-1))

loop i j = if i > 5 then () else if j > 5 then loop (i+1) 0 else let _ = print (ack i j) in loop i (j + 1)
loop i = if i > 30 then () else let _ = print (fib i) in loop (i + 1)

gcd a b = match b with | 0 -> a | _ -> gcd b (a % b)

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





