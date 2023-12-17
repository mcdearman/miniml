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
(class (Stack (T) xs) (ToString)
  (def (push x) (Stack T))
  (def (pop) T)
  (def (size) int)
  (override def (toString self)
    (str "(" (join ", " (map (fn (x) (toString x)) self)) ")")))
;; (class (List (T)) (ToString)
;;   (def (add x) (List T))
;;   (def (get i) T)
;;   (def (size) int)
;;   (def (toString self)
;;     (str "(" (join ", " (map (fn (x) (toString x)) self)) ")")))

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

(class Point 
  (def x Int 0)
  (def y Int 0)
  (def (move dx dy)
    (Point (+ x dx) (+ y dy)))
  (override def (toString)
    (format "Point(x: {0}, y: {1})" x y)))

(class Node T (inner T) (span Int))