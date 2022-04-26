(ns shimmers.algorithm.wave-function-collapse
  (:require
   [clojure.string :as str]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
;; https://isaackarth.com/papers/wfc_is_constraint_solving_in_the_wild.pdf

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

(defn dims [grid]
  [(count (first grid))
   (count grid)])

(defn grid->amatrix [grid]
  (let [[w h] (dims grid)]
    (->>
     (for [j (range h)
           i (range w)]
       [(gv/vec2 i j) (nth (nth grid j) i)])
     (into {:dims [w h]}))))

(defn valid-neighbor? [[cols rows] [i j]]
  (and (>= i 0) (< i cols)
       (>= j 0) (< j rows)))

(def cardinal-directions
  [(gv/vec2 1 0) (gv/vec2 -1 0) (gv/vec2 0 1) (gv/vec2 0 -1)])

(defn neighbors [dims pos]
  (filter (fn [p] (valid-neighbor? dims p))
          (map (partial tm/+ pos) cardinal-directions)))

(defn rules [amatrix directions]
  (let [{:keys [dims]} amatrix
        [w h] dims]
    (for [j (range h)
          i (range w)
          dir directions
          :let [pos (gv/vec2 i j)
                neighbor (tm/+ pos dir)]
          :when (valid-neighbor? dims neighbor)]
      [(get amatrix pos) dir (get amatrix neighbor)])))

(defn all-tiles [rules]
  (set (map first rules)))

(defn init-grid [[cols rows] cases]
  (into {:dims [cols rows]}
        (for [j (range rows)
              i (range cols)]
          [(gv/vec2 i j) cases])))

(defn tile-weights [rules]
  (frequencies (map first rules)))

(defn legal-rules
  "Calculate subset of legal rules given current `grid` and a `position`."
  [grid rules position]
  (let [{:keys [dims]} grid]
    (for [[value dir tile] rules
          :let [neighbor (tm/+ position dir)]
          :when (valid-neighbor? dims neighbor)
          :when (contains? (get grid neighbor) tile)]
      [value dir tile])))

(defn collapsed? [grid pos]
  (= 1 (count (get grid pos))))

(comment
  (let [rt (rules (grid->amatrix rule-a) cardinal-directions)
        grid (init-grid [3 3] (all-tiles rt))]
    [(legal-rules grid rt (gv/vec2 1 1))]))
