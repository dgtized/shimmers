(ns shimmers.math.polar)

(defn angles
  "Return sequence of angles from 0.0 to 2Pi subdivided by n."
  [n]
  (let [m (* 2 Math/PI)]
    (range 0 m (/ m n))))
