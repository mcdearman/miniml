(def (fib n)
  (let ((\ (aux n a b))
    (if (= n 0)
      a
      (aux (- n 1) b (+ a b)))
    (aux n 0 1))))

;; let bindings
(let ((x 1) (y 2))
  (+ x y))

;; let bindings with function
(let ((x 1) (y 2))
  (def (add a b)
    (+ a b))
  (add x y))

(def (fib n)
  (let 
    (((aux n a b)
        (if (= n 0)
            a
            (aux (- n 1) b (+ a b)))))
    (aux n 0 1)))

;; def fib n =
;;   let aux n a b =
;;     if n = 0 then a
;;     else aux (n - 1) b (a + b)
;;   in aux n 0 1

(def (fib n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(let ((x 1) 
      (y 2))
  (+ x y))

(let ((x  10)
      ((fib n)
         (if (<= n 1)
             n
             (+ (fib (- n 1)) (fib (- n 2))))))
  (fib x))

;; let x = 10
;;     fib n =
;;       if n <= 1 then n
;;       else fib (n - 1) + fib (n - 2)
;; in fib x

(def (fib n)
  (while (<= n 1)
         n
         (+ (fib (- n 1)) (fib (- n 2)))))

(macro (while test body)
  `(let loop ()
     (if ,test
         (begin ,@body (loop)))))

;; ML view
;; macro while test body =
;;   `(let loop ()
;;      (if ,test
;;          (begin ,@body (loop)))))

(while (is-not-empty? (read-line))
       (println it))

;; expanded form
(let loop ()
  (if (is-not-empty? (read-line))
      (begin (println it) (loop))))

(macro (for init test update body)
  `(let loop ((,init))
     (if ,test
         (begin ,@body (loop ,update)))))

(for (i 0) (< i 10) (+ i 1)
    (println i))

;; expanded form
(let loop ((i 0))
  (if (< i 10)
      (begin (println i) (loop (+ i 1)))))

(macro (for-each var list body)
  `(for (,var ,list) (is-not-empty? ,var) (tail ,var)
        ,@body))

(for-each line (read-lines) (println line))

;; expanded form
(for (line (read-lines)) (is-not-empty? line) (tail line)
      (println line))

;; fully expanded form
(let loop ((line (read-lines)))
  (if (is-not-empty? line)
      (begin (println line) (loop (tail line)))))

;; iterative fib using for
(def (fib n)
  (if (<= n 1)
      n
      (for (i 2) (< i n) (+ i 1)
        (def a b)
        (set! a b)
        (set! b (+ a b))
        b)))

;; fully expanded form
(def (fib n)
  (if (<= n 1)
      n
      (let loop ((i 2) (a 0) (b 1))
        (if (< i n)
            (begin (def a b)
                   (set! a b)
                   (set! b (+ a b))
                   (loop (+ i 1) a b))
            b))))

;; iterative fib using while
(def (fib n)
  (if (<= n 1)
      n
      (let ((i 2) (a 0) (b 1))
        (while (< i n)
               (def a b)
               (set! a b)
               (set! b (+ a b))
               (inc! i))
        b)))