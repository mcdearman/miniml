;; Main class
(class (Main args)
  (def (main)
    (println "Hello World!")))

;; class def
(class (Point x y) (ToString)
  (def (move dx dy) (Point (+ self.x dx) (+ self.y dy)))
    
  (override def (toString self)
    (f"Point(" self.x "," self.y ")")))

;; generic class
(class (Stack (T) xs)
  (def (push x) (Stack T))
  (def (pop) T)
  (def (size) int)
  (impl ToString
    (def (toString)
      (f"Stack(" (join ", " (map (fn (x) (toString x)) xs)) ")"))))

(def s (Stack Int))

;; sum class (enum)
(enum (Color Red Green Blue))

;; enum with inner types
(enum (Shape 
  (Circle rad) 
  (Square len) 
  (Rectangle w h)))

;; generic enum
(enum (Option (T) (Some T) None))

(enum (List (T) (Pair T (List T)) Empty))

(class (Point x y)
  (var x Int 0)
  (var y Int 0)
  (def (move dx dy)
    (begin 
      (set! x (+ x dx))
      (set! y (+ y dy))))
  (impl ToString
    (def (toString)
      (f"Point(x: {x}, y: {y})"))))

(enum #:forall T (Option (Some T) None))

(enum #:forall T (List (Pair T (List T)) Empty))