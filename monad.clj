;; just scratches
(m-bind
 
 1

 (fn [a]

   (m-bind

    (inc a)

    (fn [b]
      (* a b)))))

(domonad identity-m
  [a  1
   b  (inc a)]
  (* a b))


(defn f [x]
  (domonad maybe-m
    [a  x
     b  (inc a)]
    (* a b)))

(defn m-bind [value function]
  (if (nil? value)
      nil
      (function value)))

(defn m-bind-first-try [sequence function]
  (map function sequence))


(m-bind-first-try (range 5)  (fn [a]
(m-bind-first-try (range a)  (fn [b]
                               (* a b)))))

(defn m-bind-second-try [sequence function]
  (apply concat (map function sequence)))

(m-bind-second-try (range 5)  (fn [a]
(m-bind-second-try (range a)  (fn [b]
                                (* a b)))))


(m-bind-second-try (range 5)  (fn [a]
(m-bind-second-try (range a)  (fn [b]
                                (list (* a b))))))



(defn m-bind [sequence function]
  (apply concat (map function sequence)))

(defn m-result [value]
  (list value))


(= (m-bind (m-result value) function)
   (function value))

(= (domonad
     [x monadic-expression]
      x)
   monadic-expression)
