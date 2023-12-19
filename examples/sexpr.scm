;; Main class
(class (Main args)
  (def (main)
    (println "Hello World!")))

;; class def
(class (Point x y))

;; generic class
(class (Stack [T] (var xs))
  (def (push x) (set! xs (pair x xs)))
  (def (pop) T)
  (def (size) int)
  (impl ToString
    (def (toString)
      (f"Stack(" (join ", " (map (fn (x) (to-string x)) xs)) ")"))))

(val s (Stack [Int] '(1 2 3)))

;; sum class (enum)
(enum (Color Red Green Blue))

;; enum with inner types
(enum (Shape 
  (Circle rad) 
  (Square len) 
  (Rectangle w h)))

;; generic class with type hints
(class (Point <X Y> (var (x : X)) (var (y : Y))
  (def (move (dx : X) (dy : Y))
    (begin 
      (set! x (+ x dx))
      (set! y (+ y dy))))
  (impl ToString
    (def (toString)
      (f"Point {X} {Y} (x: {x}, y: {y})")))))

;; instantiate generic class
(val p (Point [Int Int] 1 2))

(enum (Option [T] (Some T) None))

(enum (List [T] (Pair T (List T)) Empty))