(def (fib n)
  (let (aux n a b) 
    (if (= n 0)
      a
      (aux (- n 1) b (+ a b)))
    (aux n 0 1)))

(def (fib n)
  (def (aux n a b)
    (if (= n 0)
        a
        (aux (- n 1) b (+ a b))))
    (aux n 0 1))

(def (fib n)
  (if (<= n 1)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(let (x 1)
  (let (y 2)
    (+ x y)))

(macro (while cond body)
  (if cond
    (begin body (while cond body))))