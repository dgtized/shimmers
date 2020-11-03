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
  [[sym init check change] & body]
  `(loop [~sym ~init]
     (when ~check
       ~@body
       (recur ~change))))
