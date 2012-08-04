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