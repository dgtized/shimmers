(ns shimmers.math.wave)

;; https://en.wikipedia.org/wiki/List_of_periodic_functions
;; Consider implementing cycloid and pulse-wave as well

(defn square
  "Square wave function from -1 to 1 with frequency `f`"
  [f t]
  (+ (* 2 (- (* 2 (Math/floor (* f t)))
             (Math/floor (* 2 f t)))) 1))

(defn sawtooth
  "Sawtooth wave function from -1 to 1 over period `p`."
  [p t]
  (let [f (/ t p)]
    (* 2 (- f (Math/floor (+ 0.5 f))))))

(defn triangle
  "Linear wave function from -1 to 1 over period `p`."
  [p t]
  (let [f (Math/floor (+ (/ (* 2 t) p) 0.5))]
    (* (/ 4 p) (- t (* (/ p 2) f)) (Math/pow -1 f))))
