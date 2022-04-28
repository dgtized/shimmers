(ns shimmers.algorithm.wave-function-collapse
  (:require
   [clojure.string :as str]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   #?(:clj [clojure.data.priority-map :as priority]
      :cljs [tailrecursion.priority-map :as priority])
   [clojure.set :as set]))

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
                (+ shannon (* p (Math/log (/ 1 p))))))
            0.0
            (vals choices))))

(defn tiles-from-rules
  "List tiles that are legal according to rules for each surrounding direction."
  [rules]
  (->> rules
       (group-by second)
       vals
       (map (comp set (partial map first)))
       (reduce set/intersection)))

(defn propagate [grid rules position tile]
  (let [initial (neighbors (:dims grid) position)]
    (loop [visiting initial
           changes (set initial)
           grid (assoc grid position (set tile))]
      (if (empty? visiting)
        [changes grid]
        (let [[pos & remaining] visiting
              neighborhood (remove (partial collapsed? grid) (neighbors (:dims grid) pos))]
          (if (collapsed? grid pos)
            (recur remaining (disj changes pos) grid)
            (let [lr (legal-rules grid rules pos)
                  legal-tiles (tiles-from-rules lr)]
              (cond (empty? legal-tiles)
                    (throw [:no-legal-tiles pos lr])
                    (= legal-tiles (get grid pos))
                    (recur remaining changes grid)
                    :else
                    (recur (into remaining neighborhood)
                           (into changes neighborhood)
                           (assoc grid position legal-tiles))))))))))

(comment (propagate {:dims [2 2]
                     (gv/vec2 0 0) #{:a :b} (gv/vec2 1 0) #{:a :b}
                     (gv/vec2 0 1) #{:a :b} (gv/vec2 1 1) #{:a :b}}
                    [[:b (gv/vec2 1 0) :a]
                     [:b (gv/vec2 0 1) :a]
                     [:b (gv/vec2 -1 0) :a]
                     [:b (gv/vec2 0 -1) :a]
                     [:b (gv/vec2 1 0) :b]
                     [:b (gv/vec2 0 1) :b]
                     [:b (gv/vec2 -1 0) :b]
                     [:b (gv/vec2 0 -1) :b]
                     ]
                    (gv/vec2)
                    #{:a}))

(defn solve [grid rules]
  (let [weights (tile-weights rules)]
    (loop [positions (conj (priority/priority-map) [(gv/vec2) 0])
           grid grid]
      (if (empty? positions)
        grid
        (let [pos (first (peek positions))]
          (if (collapsed? grid pos)
            (recur (pop positions) grid)
            (let [legal (tiles-from-rules (legal-rules grid rules pos))
                  choice (dr/weighted (zipmap legal (map weights legal)))
                  [changes grid'] (propagate grid rules pos choice)]
              (println pos (entropy grid weights pos) choice changes)
              (recur (into (pop positions)
                           (map (fn [pos] [(gv/vec2 pos)
                                          (+ (entropy grid' weights pos)
                                             (* 0.0000001 (- (rand) 0.5)))])
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
