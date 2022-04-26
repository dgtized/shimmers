(ns shimmers.algorithm.wave-function-collapse
  (:require [clojure.string :as str]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn str->grid [s]
  (->> s
       str/split-lines
       (map str/trim)
       (mapv vec)))

(def rule-a
  (str->grid
   "AAA
    ABA
    AAA"))

(defn grid->amatrix [grid]
  (let [h (count grid)
        w (count (first grid))]
    (->>
     (for [j (range h)
           i (range w)]
       [(gv/vec2 i j) (nth (nth grid j) i)])
     (into {:size [w h]}))))

(defn valid-neighbor? [[i j] cols rows]
  (and (>= i 0) (< i cols)
       (>= j 0) (< j rows)))

(def cardinal-directions
  [(gv/vec2 1 0) (gv/vec2 -1 0) (gv/vec2 0 1) (gv/vec2 0 -1)])

(defn rules [amatrix directions]
  (let [{[w h] :size} amatrix]
    (for [j (range h)
          i (range w)
          dir directions
          :let [pos (gv/vec2 i j)
                neighbor (tm/+ pos dir)]
          :when (valid-neighbor? neighbor w h)]
      [(get amatrix pos) dir (get amatrix neighbor)])))

(comment (rules (grid->amatrix rule-a) cardinal-directions))
