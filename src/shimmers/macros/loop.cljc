(ns shimmers.macros.loop)

(defmacro downto
  "[idx upper lower] => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from upper downto lower."
  [bindings & body]
  (let [i (first bindings)
        upper (second bindings)
        lower (nth bindings 2)]
    `(let [n# ~upper
           lower# ~lower]
       (loop [~i n#]
         (when (> ~i lower#)
           ~@body
           (recur (dec ~i)))))))

(defmacro upto
  "[idx lower upper] => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from lower upto upper."
  [bindings & body]
  (let [i (first bindings)
        lower (second bindings)
        upper (nth bindings 2)]
    `(let [n# ~lower
           upper# ~upper]
       (loop [~i n#]
         (when (< ~i upper#)
           ~@body
           (recur (inc ~i)))))))

(defmacro c-for
  "[sym init check step ...] & body

  Loops sym from initial value while check is true, and running step to update on
  each loop, executing body for side effects. If more then one symbol, init,
  check,step is defined, they are executed recursively, with each consecutive
  sym operating as an inner loop."
  [seq-exprs & body]
  (let [group (take-last 4 seq-exprs) ;; last group first
        next-group (drop-last 4 seq-exprs)
        do-loop (fn [[sym init check step] body]
                  `(loop [~sym ~init]
                     (when ~check
                       ~@body
                       (recur ~step))))]
    (if (seq next-group)
      `(c-for ~next-group ~(do-loop group body))
      (do-loop group body))))

;; TODO doesn't work here, needs to be required as loop
(comment (loop/downto [y (dec 5) 0] (println y))
         (loop/upto [x 0 4] (println x))
         (loop/c-for [x 0 (< x 4) (inc x)] (println x))
         (macroexpand-1 '(loop/c-for [x 2 (>= x 0) (dec x)] (println x)))
         (macroexpand '(loop/c-for [x 0 (< x 3) (inc x)
                                    y 1 (< y 3) (inc y)]
                         (+ x y)
                         (println [x y])))
         (loop/c-for [x 0 (< x 3) (inc x)
                      y 1 (< y 3) (inc y)]
           (+ x y)
           (println [x y]))
         (macroexpand '(loop/c-for [x 0 (< x 3) (inc x)
                                    y 1 (< y 3) (inc y)
                                    z 2 (> z 0) (dec z)]
                         (+ x y)
                         (println [x y z]))))

