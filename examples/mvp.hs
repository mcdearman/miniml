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

-- examples
gcd a b = if b == 0 then a else gcd b (a % b)
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
ack m n = if m == 0 then n + 1 else if n == 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n-1))






