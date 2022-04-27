(ns shimmers.algorithm.wave-function-collapse
  (:require
   [clojure.string :as str]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   #?(:clj [clojure.data.priority-map :as priority]
      :cljs [tailrecursion.priority-map :as priority])))

;; https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
;; https://isaackarth.com/papers/wfc_is_constraint_solving_in_the_wild.pdf

(defn str->matrix [s]
  (->> s
       str/split-lines
       (map str/trim)
       (mapv vec)))

(defn dims [grid]
  [(count (first grid))
   (count grid)])

(defn matrix->grid [matrix]
  (let [[w h] (dims matrix)]
    (->>
     (for [j (range h)
           i (range w)]
       [(gv/vec2 i j) (nth (nth matrix j) i)])
     (into {:dims [w h]}))))

(defn grid->matrix [grid]
  (let [[w h] (:dims grid)]
    (vec (for [j (range h)]
           (vec (for [i (range w)]
                  (get grid (gv/vec2 i j))))))))

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
          :when (and (valid-neighbor? dims neighbor)
                     (contains? (get grid neighbor) tile))]
      [value dir tile])))

(comment (legal-rules {:dims [2 2]
                       (gv/vec2 0 0) #{:a :b :c} (gv/vec2 1 0) #{:a :c}
                       (gv/vec2 0 1) #{:a :b} (gv/vec2 1 1) #{:a :b :c}}
                      [[:a (gv/vec2 1 0) :b]
                       [:a (gv/vec2 0 1) :b]
                       [:b (gv/vec2 1 0) :c]
                       [:b (gv/vec2 0 1) :c]]
                      (gv/vec2 0 0)))

(defn collapsed? [grid pos]
  (= 1 (count (get grid pos))))

(defn fully-collapsed? [grid]
  (every? (fn [v] (= 1 (count v)))
          (vals (dissoc grid :dims))))

(defn entropy [grid weights position]
  (let [choices (select-keys weights (seq (get grid position)))
        all-options (reduce + 0.0 (vals choices))]
    (reduce (fn [shannon v]
              (let [p (/ v all-options)]
                (- shannon (* p (Math/log (/ 1 p))))))
            1.0
            (vals choices))))

(comment (entropy {(gv/vec2) #{:A :B :C}} {:A 3 :B 1 :C 1} (gv/vec2))
         (entropy {(gv/vec2) #{:A :B}} {:A 3 :B 1 :C 1} (gv/vec2))
         (entropy {(gv/vec2) #{:A}} {:A 3 :B 1 :C 1} (gv/vec2))
         (entropy {(gv/vec2) #{:B}} {:A 3 :B 1 :C 1} (gv/vec2)))

(defn propagate [grid rules position tile]
  (let [initial (neighbors (:dims grid) position)]
    (loop [visiting initial
           changes (set initial)
           grid (assoc grid position (set tile))]
      (if (empty? visiting)
        [changes grid]
        (let [[pos & remaining] visiting]
          (if (collapsed? grid pos)
            (recur remaining (disj changes pos) grid)
            (let [legal (legal-rules grid rules pos)
                  legal-tiles (set (map first legal))]
              (if (= legal-tiles (get grid pos))
                (recur remaining changes grid)
                (let [to-visit (neighbors (:dims grid) pos)]
                  (recur (into remaining to-visit)
                         (into changes to-visit)
                         (assoc grid position legal-tiles)))))))))))

(defn solve [grid rules]
  (let [weights (frequencies (map first rules))]
    (loop [positions (conj (priority/priority-map) [1 (first (keys (dissoc grid :dims)))])
           grid grid]
      (if (empty? positions)
        grid
        (let [pos (second (peek positions))]
          (if (collapsed? grid pos)
            (recur (pop positions) grid)
            (let [legal (legal-rules grid rules pos)
                  choice (dr/weighted (tile-weights legal))
                  [changes grid'] (propagate grid rules pos choice)]
              (recur (into (pop positions)
                           (map (fn [pos] [(+ (entropy grid weights pos)
                                             (* 0.001 (- (rand) 0.5)))
                                          (gv/vec2 pos)])
                                changes))
                     grid'))))))))

(def rule-a
  (str->matrix
   "AAAAA
    ABCBA
    AAAAA"))

(comment
  (let [rt (rules (matrix->grid rule-a) cardinal-directions)
        grid (init-grid [8 8] (all-tiles rt))]
    (grid->matrix (solve grid rt))))
