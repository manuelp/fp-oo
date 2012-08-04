(ns fp-oo.core)

;; This is a simple constructor, but instance variables are not
;; encapsulated:
;;
;;     (:x (Point 5 7)) ; -> 5
;;
;; However, we can load the resulting hash-map with all metadata we
;; need (such as: class name).
(def Point
  (fn [x y]
    {:x x
     :y y
     :__class_symbol__ 'Point}))

;; However, for now we can pretend that we already have data-hiding
;; and implement some *accessor methods*:
(def x :x)
(def y :y)
(def class :__class_symbol__)

(def shift
  (fn [this xinc yinc]
    (Point (+ (x this) xinc)
           (+ (y this) yinc))))

(def add
  (fn [this other]
    (shift this (x other) (y other))))

;; Support for creation a-la Java:
;;
;;     (new Point 3 5)
;;
;; But since `new` is reserverd for java interop, we'll use `a`:
(def a
  (fn [class & args]
    (apply class args)))

;; Let's define a `Triangle` class to test the `a` operator with more
;; than 2 arguments.
(def Triangle
  (fn [x y z]
    {:x x
     :y y
     :z z
     :__class_symbol__ 'Triangle}))

(def right-triangle (a Triangle (a Point 1 1) (a Point 1 2) (a Point 2 1)))
(def equal-right-triangle (a Triangle (a Point 1 1) (a Point 1 2) (a Point 2 1)))
(def different-triangle (a Triangle (a Point 3 3) (a Point 3 4) (a Point 4 3)))

;; Equality is very easy with this kind of object system. It's even
;; independent from classes!
(def equal-triangles? =)

(def valid-triangle?
  (fn [& points]
    (= 3 (count (distinct points)))))