(ns fp-oo.core)

;; This is a simple constructor, but instance variables are not
;; encapsulated:
;;
;;     (:x (Point 5 7)) ; -> 5
(def Point
  (fn [x y]
    {:x x
     :y y}))

;; However, for now we can pretend that we already have data-hiding
;; and implement some *accessor methods*:
(def x (fn [this]
         (:x this)))

(def y (fn [this]
         (:y this)))

;; A more concise definition would simply be:
;;
;;     (def x :x)

