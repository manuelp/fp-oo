(ns fp-oo.core)

(defn- class-from-instance
  "Returns the class from an instance."
  [instance]
  (eval (:__class_symbol__ instance)))

(defn- has-instance-variable?
  [instance variable]
  (not (nil? (variable instance))))

(defn- class-instance-methods
  "Extract a map with all instance methods of the given class."
  [class-symbol]
  (:__instance_methods__ (eval class-symbol)))

(defn- class-symbol-above
  "Returns the superclass symbol."
  [class-symbol]
  (:__superclass_symbol__ (eval class-symbol)))

(defn- lineage
  "Compiles a seq of symbols for the entire class hierarchy lineage, rooted in `Anything`."
  [class-symbol]
  (loop [line (seq [class-symbol])
         above-sym (class-symbol-above class-symbol)]
    (if (nil? above-sym)
      line
      (recur (cons above-sym line)
             (class-symbol-above above-sym)))))

;; Next, we can use that function to obtail a seq of maps for all the
;; instance methods of the class *lineage*. Then we can merge that
;; maps and obtain all methods available to that class.
(defn- method-cache
  [class]
  (let [class-symbol (:__own_symbol__ class)
        method-maps (map class-instance-methods
                         (lineage class-symbol))]
    (apply merge method-maps)))

;; Applying a method defined in the class to an instance with some
;; arguments can be abstracted by a generic function.
;;
;; If the class doesn't directly responds to the given message, before
;; throwing and exception, search for an instance variable with the
;; same name of the message: if it exists then returns the
;; corresponding value, fails otherwise. In other words, automatically
;; "generates" read accessors.
(defn- apply-message-to
  [class instance message args]
  (let [method (message (method-cache class))]
    (or (and (nil? method)
             (has-instance-variable? instance message)
             (message instance))
        (apply method instance args))))

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

(def Anything
  {
   :__own_symbol__ 'Anything
   :__instance_methods__
   {
    ;; Default constructor
    :add-instance-values identity
    
    :class-name :__class_symbol__
    :class #(class-from-instance %)}})

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
   :__superclass_symbol__ 'Anything
   :__instance_methods__
   {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :shift (fn [this xinc yinc]
             (a Point
                (+ (:x this) xinc)
                (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift
                    (send-to other :x) (send-to other :y)))
    }})