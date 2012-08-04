(ns fp-oo.core)

;; Support for creation of instances a-la Java:
;;
;;     (new Point 3 5)
;;
;; But since `new` is reserverd for java interop, we'll use `a`:
(def a
  (fn [class & args]
    (apply class args)))

;; Function for message dispatching.
(def send-to
  (fn [object message & args]
    (apply (message (:__methods__ object)) object args)))

;; A class definition with instance variables (for now not
;; encapsulated) and methods.
;;
;; Equality in this type of object can be obtained for free using `=`.
(def Point
  (fn [x y]
    {:x x
     :y y
     :__class_symbol__ 'Point
     :__methods__ {
                   :class :__class_symbol__
                   :x :x
                   :y :y
                   :shift (fn [this xinc yinc]
                            (a Point
                               (+ (:x this) xinc)
                               (+ (:y this) yinc)))
                   :add (fn [this other]
                          (send-to this :shift
                                   (send-to other :x) (send-to other :y)))}}))

;; Let's define a `Triangle` class to test the `a` operator with more
;; than 2 arguments.
(def Triangle
  (fn [x y z]
    {:x x
     :y y
     :z z
     :__class_symbol__ 'Triangle
     :__methods__ {
                   :class :__class_symbol__
                   :x :x
                   :y :y
                   :z :z}}))

(def right-triangle (a Triangle (a Point 1 1) (a Point 1 2) (a Point 2 1)))
(def equal-right-triangle (a Triangle (a Point 1 1) (a Point 1 2) (a Point 2 1)))
(def different-triangle (a Triangle (a Point 3 3) (a Point 3 4) (a Point 4 3)))

;; Equality is very easy with this kind of object system. It's even
;; independent from classes!
(def equal-triangles? =)

(def valid-triangle?
  (fn [& points]
    (= 3 (count (distinct points)))))