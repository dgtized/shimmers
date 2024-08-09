(ns shimmers.scratch.timing)

(comment
  (map (fn [x] (mod (+ (int (/ x 24)) (int (/ x 23))) 7)) (filter (fn [x] (= (mod x 8) 0)) (range 512)))
  (map (fn [x] [x (mod (int (/ x 29)) 7)])
       (filter (fn [x] (= (mod x 16) (mod (int (/ x 64)) 3))) (range 512))))
