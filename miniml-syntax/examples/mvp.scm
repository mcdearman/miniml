(let x 10)

(let (gcd a b)
  (if (= b 0) 
      a
      (gcd b (mod a b))))

(let (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(let (((fib n) 
         (if (<= n 1)
             n
             (+ (fib (- n 1)) (fib (- n 2))))))
  (fib 10))

(let (map f xs)
  (if (empty? xs) nil
      (pair (f (head xs)) (map f (tail xs)))))

(let ((a 10)
      (b 5))
  (+ a b))

(if (= 1 2) 
    (println "1 is equal to 2")
    (println "1 is not equal to 2"))

'(1 2 3)
'(1 . 2)
'(1 2 . 3)
`(1 2 ,(+ 1 2))
`(1 2 ,@(list 3 4))
[1 2 3]
{ :a 1 :b 2 }
#[1 2 3]


