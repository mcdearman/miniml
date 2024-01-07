-- only bindings are allowed at the top level
let x = 1

-- mutable binding
var x = 1

-- Binary operators
let a = 1 + 2 * 3^2 - 4 / 5 % 10

-- Unary operators
let n = -1
let not = !true

-- Boolean
let b = true && false || true

-- Comparison
let c = 1 < 2 && 2 <= 3 && 3 > 2 && 3 >= 2 && 2 == 2 && 2 != 3

-- if-then-else
let i = if 1 < 2 then 1 else 2

-- let-in
let x = 1 in x + 1

-- function definition
let f x = x + 1

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
let fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in fib 10

-- tail recursion
let fib n = 
  let loop n a b = 
    if n = 0 then a 
    else loop (n - 1) b (a + b) 
  in loop n 0 1 

-- here's all of the mvp ideas in one program
let foo a b =
  let bar = \c ->
    if (a == b && b < 0 || b <= 1) || (a != b && b > 0 || b >= 1)
      then -a + (b - c) * 2/3 / 3 % 4 ^ 5
    else foo (a - 1) + foo (a - 2)
  in bar 1
