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
  `(def (,name ,args) ,@body))

(fn gcd (a b)
  (if (= b 0) 
      a
      (gcd b (mod a b))))

'(1 2 3)
`(1 2 ,(+ 1 2))
`(1 2 ,@(list 3 4))
[1 2 3]
#{ 1 2 3 }
{ :a 1 :b 2 }