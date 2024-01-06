;; immutable binding
(def x 10)

;; mutable binding
(def! y 10)

;; functions are first class
(def gcd (lambda (a b)
  (if (= b 0) 
      a
      (gcd b (mod a b)))))

(def map (lambda (f xs)
  (if (empty? xs) nil
      (pair (f (head xs)) (map f (tail xs))))))

;; let expression
(let ((a 10)
      (b 5))
  (+ a b))

;; if expression
(if (= 1 2) 
    (println "1 is equal to 2")
    (println "1 is not equal to 2"))

;; macro
(macro (fn name args body)
  `(def ,name (lambda ,args) ,@body))

(macro (while test body)
  `(fn loop ()
     (if ,test
         (begin ,@body (loop))
         ()
         )))

(fn gcd (a b)
  (if (= b 0) 
      a
      (gcd b (mod a b))))

(fn fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn map (f xs)
  (if (empty? xs) nil
      (pair (f (head xs)) (map f (tail xs)))))

(fn fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fn ack (m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))

'(1 2 3)
`(1 2 ,(+ 1 2))
`(1 2 ,@(list 3 4))
[1 2 3]
#{ 1 2 3 }
{ :a 1 :b 2 }

;; records
(type Person (name age))
