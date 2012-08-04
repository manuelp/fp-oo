(ns fp-oo.core)

(defn- class-from-instance
  [instance]
  (eval (:__class_symbol__ instance)))

;; This class definition exposes an object as a map of instance
;; methods (or *handlers* for messages that can be sent to instances
;; of this class).
;; 
;; One special method is `add-instance-values`, and is used when
;; instantiating an object to populate its instance variables (like
;; Ruby's `initialize` method).
(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
   {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class-name :__class_symbol__
    :class #(class-from-instance %)
    :shift (fn [this xinc yinc]
             (a Point
                (+ (:x this) xinc)
                (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift
                    (send-to other :x) (send-to other :y)))
    }})

;; Applying a method defined in the class to an instance with some
;; arguments can be abstracted by a generic function.
(defn- apply-message-to
  [class instance message args]
  (let [method (message (:__instance_methods__ class))]
    (apply method instance args)))

;; Instance creation is done in three steps:
;;
;; 1. Allocate memory: `{}`
;; 2. Seed memory with metadata needed by the language runtime (in
;;    this case, the object system).
;; 3. A particular instance method is called to fill in the starting
;;    values of the new instance.
;;
;; The function that does the instantiation is generic and follows
;; this three steps. It's named `a` to differentiate from `new` that is
;; a function to instantiate Java's classes.
(def a
  (fn [class & args]
    (let [class-symbol (:__own_symbol__ class)
          seeded {:__class_symbol__ class-symbol}]
      (apply-message-to class seeded :add-instance-values args))))

;; The message dispatch function has to look up for the class
;; definition from an object instance of that class and call one of
;; his instance methods.
(def send-to
  (fn [instance message & args]
    (let [class (class-from-instance instance)]
      (apply-message-to class instance message args))))