(ns shimmers.math.polar)

(defn angles
  "Return sequence of angles from 0.0 to 2Pi subdivided by n."
  [n]
  (take-while (fn [x] (<= x (* 2 Math/PI)))
              (iterate (partial + (/ (* 2 Math/PI) n)) 0.0)))
