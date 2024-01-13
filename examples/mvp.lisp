; ;; let binding
; ;; bindings are immutable by default
; (let a 1)

; ;; mutable binding
; (let! a 1)

; ;; if expression
; ;; Every if expression must have a then and an else clause.
; ;; The types of the then and else clause must be the same.
; (if (= 1 2) 
;     (println "1 is equal to 2")
;     (println "1 is not equal to 2"))

; ;; let binding to function
; (let (fib n)
;   (let (loop n a b)
;     (if (= n 0)
;       a
;       (loop (- n 1) b (+ a b)))
;     (loop n 0 1)))

; ;; let binding to function with expression body
; (let (gcd a b)
;   (if (= b 0) 
;       a
;       (gcd b (mod a b)))
;   (gcd 10 5))

; ;; type hints
; (let (gcd (a : Int) (b : Int) : Int)
;   (if (= b 0) 
;       a
;       (gcd b (mod a b)))
;   (gcd 10 5))

; (let (map f xs)
;   (if (empty? xs) ()
;       (pair (f (head xs)) (map f (tail xs)))))

; (let (fact n)
;   (if (= n 0)
;       1
;       (* n (fact (- n 1)))))

; (let (ack m n)
;   (cond ((= m 0) (+ n 1))
;         ((= n 0) (ack (- m 1) 1))
;         (t (ack (- m 1) (ack m (- n 1))))))

; ;; lists
; '(1 2 3)

; ;; quasiquote/unquote
; `(1 2 ,(+ 1 2))

; ;; quasiquote/unquote-splicing
; `(1 2 ,@(list 3 4))

; ;; vectors
; [1 2 3]

; ;; sets
; #{ 1 2 3 }

; ;; maps
; { :a 1 :b 2 }

; ;; User defined types
; ;; product type
; (type (Point 
;   (x : Int) 
;   (y : Int)))

; ;; product type with type parameters
; (type (Pair T 
;   (head : T) 
;   (tail : (Pair T))))

; ;; sum type
; (type (Shape
;   (Circle (radius : Int))
;   (Rectangle (width : Int) (height : Int))
;   (Triangle (base : Int) (height : Int))))

; ;; sum type with type parameters
; (type (Option T (Some T) (None)))

; (type (Result T E 
;   (Ok T) 
;   (Err E)))

; ;; sum type with complex type parameters
; (type (List T
;   (Pair (head : T) (tail : (List T)))
;   (Empty)))

; ;; macros
; (macro (cond clauses)
;   (if (empty? clauses)
;       ()
;       (let (clause (head clauses))
;         (if (= (head clause) 'else)
;             (list 'begin (tail clause))
;             (list 'if (head clause) (list 'begin (tail clause)) (cond (tail clauses)))))))

; (macro (when test body)
;   `(if ,test (begin ,@body) ()))

; (macro (while test body)
;   `(let (loop)
;      (if ,test
;          (begin ,@body (loop))
;          ())))

; ;; example uses
; (while (< i 10)
;   (println i)
;   (set! i (+ i 1)))

; ;; typeclass
; (class (Eq T)
;   (eq (a : T) (b : T) : Bool)
;   (neq (a : T) (b : T) : Bool
;     (not (eq a b))))

;; let binding
;; bindings are immutable by default
(let a 1)

;; mutable binding
(let! a 1)

;; if expression
;; Every if expression must have a then and an else clause.
;; The types of the then and else clause must be the same.
(if (= 1 2) 
    (println "1 is equal to 2")
    (println "1 is not equal to 2"))

;; let binding to function
(let (fib n)
  (let (loop n a b)
    (if (= n 0)
      a
      (loop (- n 1) b (+ a b)))
    (loop n 0 1)))

;; let binding to function with expression body
(let (gcd a b)
  (if (= b 0) 
      a
      (gcd b (mod a b)))
  (gcd 10 5))

;; type hints
(let (gcd (a : Int) (b : Int) : Int)
  (if (= b 0) 
      a
      (gcd b (mod a b)))
  (gcd 10 5))

(let (map f xs)
  (if (empty? xs) ()
      (pair (f (head xs)) (map f (tail xs)))))

(let (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(let (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))

(let (main args)
  (println "Hello, World!"))

;; lists
'(1 2 3)
[1 2 3]

;; quasiquote/unquote
`(1 2 ,(+ 1 2))
[1 2 (+ 1 2)]

;; quasiquote/unquote-splicing
`(1 2 ,@(list 3 4))

;; vectors
#[1 2 3]

;; sets
#{ 1 2 3 }

;; maps
{ :a 1 :b 2 }

;; User defined types
;; product type
(type (Point 
  (x : Int) 
  (y : Int)))

;; product type with type parameters
(type (Pair T 
  (head : T) 
  (tail : (Pair T))))

;; sum type
(type (Shape
  (Circle (radius : Int))
  (Rectangle (width : Int) (height : Int))
  (Triangle (base : Int) (height : Int))))

;; sum type with type parameters
(type (Option T (Some T) (None)))

(type (Result T E 
  (Ok T) 
  (Err E)))

;; sum type with complex type parameters
(type (List T
  (Pair (head : T) (tail : (List T)))
  (Empty)))

;; modules
(module List
  (type List T
    (Pair (head : T) (tail : (List T)))
    (Empty))

  (let empty (Empty))

  (let (empty? xs)
    (match xs
      (Empty true)
      (Pair false)))

  (let (map f xs)
    (if (empty? xs) ()
        (pair (f (head xs)) (map f (tail xs)))))
  
  (let (foldl f acc xs) 
    (if (empty? xs)
        acc
        (foldl f (f acc (head xs)) (tail xs))))
  
  (let (foldr f acc xs)
    (if (empty? xs)
        acc
        (f (head xs) (foldr f acc (tail xs)))))

  (let (filter f xs)
    (if (empty? xs)
        ()
        (if (f (head xs))
            (pair (head xs) (filter f (tail xs)))
            (filter f (tail xs)))))

  (let (reverse xs)
    (foldl (lambda (acc x) (pair x acc)) () xs)))

;; module usage
(List.map (lambda (x) (* x x)) [1 2 3])

;; macros
(macro (cond clauses)
  (if (empty? clauses)
      ()
      (let (clause (head clauses))
        (if (= (head clause) 'else)
            (list 'begin (tail clause))
            (list 'if (head clause) (list 'begin (tail clause)) (cond (tail clauses)))))))

(macro (when test body)
  `(if ,test (begin ,@body) ()))

(macro (while test body)
  `(let (loop)
     (if ,test
         (begin ,@body (loop))
         ())))

;; example uses
(while (< i 10)
  (println i)
  (set! i (+ i 1)))

