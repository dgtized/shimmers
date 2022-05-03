(ns shimmers.algorithm.wave-function-collapse
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [shimmers.common.ui.debug :as debug]
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

(defn matrix->grid [matrix directions]
  (let [[w h] (dims matrix)]
    (->>
     (for [j (range h)
           i (range w)]
       [(gv/vec2 i j) (nth (nth matrix j) i)])
     (into {:dims [w h]
            :directions directions}))))

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

(def diagonal-directions
  [(gv/vec2 1 -1) (gv/vec2 -1 -1) (gv/vec2 -1 1) (gv/vec2 1 1)])

(def directions-8
  (concat cardinal-directions diagonal-directions))

(def directions-4+4
  (concat cardinal-directions (mapv #(tm/* % 2) cardinal-directions)))

(def directions-8+4
  (concat directions-4+4 diagonal-directions))

(def directions-8+8
  (concat directions-4+4 diagonal-directions (mapv #(tm/* % 2) diagonal-directions)))

(defn neighbors [{:keys [dims directions]} pos]
  (filter (fn [p] (valid-neighbor? dims p))
          (map (partial tm/+ pos) directions)))

;; Rules are in form of [legal-tile dir other-tile]
;; ie given `other-tile` is present in `dir`, `legal-tile` is possible.
(defn rules [amatrix]
  (let [{:keys [dims directions]} amatrix
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

(defn init-grid [[cols rows] directions cases]
  (into {:dims [cols rows]
         :directions directions}
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

(defn entropy [grid weights position]
  (let [choices (select-keys weights (seq (get grid position)))
        all-options (reduce + 0.0 (vals choices))]
    (reduce (fn [shannon v]
              (let [p (/ v all-options)]
                (+ shannon (* p (Math/log (/ 1 p))))))
            0.0
            (vals choices))))

(defn tiles-from-rules
  "List tiles that are legal according to rules for each `check-dir` direction listed."
  [rules check-dirs]
  ;; Map is {dir #{tiles}}
  (let [surroundings
        (->> rules
             (group-by second)
             (reduce-kv (fn [m k v] (assoc m k (set (mapv first v))))
                        {}))]
    (reduce (fn [acc dir] (set/intersection acc (get surroundings dir #{})))
            (first (vals surroundings))
            check-dirs)))

(defn legal-at-location [grid rules pos]
  (let [legal-rules (legal-rules grid rules pos)]
    [(tiles-from-rules legal-rules (mapv (fn [n] (tm/- n pos)) (neighbors grid pos)))
     legal-rules]))

(defn propagate [initial-grid rules position tiles]
  (let [initial (neighbors initial-grid position)]
    (tap> [:init initial])
    (loop [visiting initial
           changes (set initial)
           grid (assoc initial-grid position tiles)]
      (if (empty? visiting)
        [changes grid]
        (let [[pos & remaining] visiting
              neighborhood (remove (partial collapsed? grid) (neighbors initial-grid pos))
              [legal-tiles legal-rules] (legal-at-location grid rules pos)]
          (tap> [:p pos])
          (cond (empty? legal-tiles)
                (throw [:no-legal-tiles pos legal-rules])
                (or (= legal-tiles (get grid pos))
                    (collapsed? grid pos))
                (recur remaining changes grid)
                :else
                (do (tap> [:change (get grid pos) legal-tiles])
                    (recur (into remaining (remove changes neighborhood))
                           (set/union changes (set neighborhood))
                           (assoc grid pos legal-tiles)))))))))

(comment
  (debug/with-tap-log
    (fn [] (propagate (init-grid [2 2] cardinal-directions #{:a :b})
                     [[:b (gv/vec2 1 0) :a]
                      [:b (gv/vec2 0 1) :a]
                      [:b (gv/vec2 -1 0) :a]
                      [:b (gv/vec2 0 -1) :a]
                      [:a (gv/vec2 1 0) :b]
                      [:a (gv/vec2 0 1) :b]
                      [:a (gv/vec2 -1 0) :b]
                      [:a (gv/vec2 0 -1) :b]]
                     (gv/vec2)
                     #{:a}))))

(defn cells-with-entropy [grid weights]
  (let [[cols rows] (:dims grid)]
    (for [j (range rows)
          i (range cols)
          :let [loc (gv/vec2 i j)]
          :when (not (collapsed? grid loc))]
      [loc (entropy grid weights loc)])))

(defn solve [grid rules]
  (let [weights (tile-weights rules)]
    (loop [positions (into (priority/priority-map) (cells-with-entropy grid weights))
           grid grid]
      (if (empty? positions)
        grid
        (let [pos (first (peek positions))]
          (if (collapsed? grid pos)
            (recur (pop positions) grid)
            (let [[legal-tiles legal-rules] (legal-at-location grid rules pos)
                  choice (dr/weighted (select-keys (tile-weights legal-rules) legal-tiles))
                  [changes grid'] (propagate grid rules pos (set [choice]))]
              (recur (into (pop positions)
                           (map (fn [pos] [(gv/vec2 pos) (entropy grid' weights pos)])
                                changes))
                     grid'))))))))

(defn solve-one [grid rules]
  (let [weights (tile-weights rules)]
    (loop [positions (into (priority/priority-map) (cells-with-entropy grid weights))
           grid grid]
      (if (empty? positions)
        [#{} grid]
        (let [pos (first (peek positions))]
          (if (collapsed? grid pos)
            (recur (pop positions) grid)
            (let [[legal-tiles legal-rules] (legal-at-location grid rules pos)
                  choice (dr/weighted (select-keys (tile-weights legal-rules) legal-tiles))
                  [changes grid'] (propagate grid rules pos (set [choice]))]
              [changes grid'])))))))

(def rule-a
  (str->matrix
   "AAAAA
    ABCBA
    AAAAA"))

(comment
  (let [dirs cardinal-directions
        rt (rules (matrix->grid rule-a dirs))
        grid (init-grid [8 8] dirs (all-tiles rt))]
    (grid->matrix (solve grid rt))))
