(ns shimmers.algorithm.wave-function-collapse
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [shimmers.common.sequence :as cs]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   #?(:clj [clojure.data.priority-map :as priority]
      :cljs [tailrecursion.priority-map :as priority])))

;; https://www.boristhebrave.com/2020/04/13/wave-function-collapse-explained/
;; https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
;; https://isaackarth.com/papers/wfc_is_constraint_solving_in_the_wild.pdf
;; https://adam-james-v.github.io/oatmilk/index.html#/notebooks/wave-collapse.org.md
;; https://github.com/maacl/wfclj
;; https://github.com/mxgmn/WaveFunctionCollapse

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

(def north (gv/vec2 0 -1))
(def east (gv/vec2 1 0))
(def south (gv/vec2 0 1))
(def west (gv/vec2 -1 0))

(def cardinal-directions
  [north east south west])

(def diagonal-directions
  [(tm/+ north east) (tm/+ south east)
   (tm/+ south west) (tm/+ north west)])

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
    (sort
     (for [j (range h)
           i (range w)
           dir directions
           :let [pos (gv/vec2 i j)
                 neighbor (tm/+ pos dir)]
           :when (valid-neighbor? dims neighbor)]
       [(get amatrix pos) dir (get amatrix neighbor)]))))

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

(defn legal-rules
  "Calculate subset of legal rules given current `grid` and a `position`."
  [grid rules position]
  (let [{:keys [dims directions]} grid
        ;; build the local neighborhoodcontaining immediate valid directions
        ;; mapped to the set of tiles at that location in the grid.
        locale (into {}
                     (for [dir directions
                           :let [neighbor (tm/+ position dir)]
                           :when (valid-neighbor? dims neighbor)]
                       {dir (get grid neighbor)}))]
    (filter (fn [[_ dir tile]]
              (contains? (get locale dir #{}) tile))
            rules)))

(defn tiles-from-rules
  "List tiles that are legal according to rules for each `check-dir` direction listed."
  [rules check-dirs]
  ;; Map is {dir #{tiles}}
  (let [surroundings
        (reduce (fn [m [a dir _]]
                  (update m dir (fnil conj #{}) a))
                {}
                rules)]
    (reduce (fn [acc dir]
              (set/intersection acc (get surroundings dir #{})))
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
                (throw (ex-info "No Legal Tiles"
                                {:pos pos
                                 :legal-tiles legal-tiles
                                 :legal-rules (count legal-rules)}))
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

;; TODO: backtracking.
;;
;; If propagate fails then we should first try a different random choice at the
;; current min-weight tile (after removing case just tired) OR try again from an
;; early branch in history.
;;
;; TODO: unify solve and solve one
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

(defn solve-one [grid {:keys [rules ranked-positions]}]
  (let [weights (tile-weights rules)]
    (loop [ranked-positions (or ranked-positions
                                (into (priority/priority-map) (cells-with-entropy grid weights)))
           grid grid]
      (if (empty? ranked-positions)
        [#{} grid ranked-positions]
        (let [pos (first (peek ranked-positions))]
          (if (collapsed? grid pos)
            (recur (pop ranked-positions) grid)
            (let [[legal-tiles legal-rules] (legal-at-location grid rules pos)
                  choice (dr/weighted (select-keys (tile-weights legal-rules) legal-tiles))
                  [changes grid'] (propagate grid rules pos (set [choice]))]
              [changes grid'
               (into (pop ranked-positions)
                     (map (fn [pos] [(gv/vec2 pos) (entropy grid' weights pos)])
                          changes))])))))))

(def rule-a
  (str->matrix
   "AAAAA
    ABCBA
    AAAAA"))

(comment
  (let [dirs cardinal-directions
        rt (rules (matrix->grid rule-a dirs))
        grid (init-grid [8 8] dirs (set (all-tiles rt)))]
    (grid->matrix (solve grid rt))))

(defn rules->tiles
  "Chop up a rules matrix into `n`x`n` tiles"
  [matrix n]
  (let [height (count matrix)
        width (count (first matrix))]
    (->> (for [x (range 0 (- width (dec n)) 1)
               y (range 0 (- height (dec n)) 1)]
           (let [tile
                 (for [j (range n)
                       :let [row (nth matrix (+ y j))]]
                   (apply str (for [i (range n)]
                                (nth row (+ x i)))))]
             {[x y] (vec tile)}))
         (into {}))))

(def rule-b
  (str->matrix
   "AAAA
    ABBA
    ABBA
    AAAA"))

(comment (rules->tiles rule-b 3))

(defn column [grid col]
  (str/join (map #(nth % col) grid)))

(defn row [grid row]
  (nth grid row))

(defn rotate-clockwise [grid]
  (vec (for [col (range (count grid))]
         (apply str (reverse (column grid col))))))

(defn rotations [tile]
  (vec (dedupe (take 4 (iterate rotate-clockwise tile)))))

(defn pattern->oriented-tiles [matrix n]
  (->> (rules->tiles matrix n)
       vals
       sort
       dedupe))

(defn pattern->rotated-tiles [matrix n]
  (->> (rules->tiles matrix n)
       vals
       (mapcat rotations)
       sort
       dedupe))

(defn clockwise-face
  "Generate the face of each tile in clockwise order.

  Directions are numbered north, south, east, west."
  [tile direction]
  (case direction
    0 (row tile 0)
    1 (column tile (dec (count (first tile))))
    2 (apply str (reverse (row tile (dec (count tile)))))
    3 (apply str (reverse (column tile 0)))))

(defn cardinal-face
  "Generate the face of a tile as a absolute cardinal ordering.

  Ie rows are always left to right, and columns are top to down."
  [tile direction]
  (case direction
    0 (row tile 0)
    1 (column tile (dec (count (first tile))))
    2 (row tile (dec (count tile)))
    3 (column tile 0)))

(defn tile-adjacencies [source adjacent]
  (filter (fn [[face opposite]] (= (cardinal-face source face)
                                  (cardinal-face adjacent opposite)))
          [[0 2]
           [1 3]
           [2 0]
           [3 1]]))

(defn all-adjacencies [tileset]
  (for [[a b] (cs/all-pairs tileset)
        :let [adj (tile-adjacencies a b)]
        :when (not-empty adj)]
    [[a b] adj]))

(defn indexed-all-adjancies
  "Given a `tileset`, generate an index of legal adjacencies.

  Index is tile -> direction -> legal tiles."
  [tileset]
  (reduce (fn [index [[a b] adj]]
            (reduce (fn [idx dir]
                      (update-in idx [a (first dir)] (fnil conj []) b))
                    index adj))
          {}
          (all-adjacencies tileset)))

(defn adjacency-rules [tileset]
  (->> tileset
       all-adjacencies
       (mapcat (fn [[[a b] adj]]
                 (mapcat (fn [dir]
                           [[a (nth cardinal-directions (first dir)) b]
                            [b (nth cardinal-directions (second dir)) a]])
                         adj)))
       (sort-by (juxt first second))
       (into [])))

(defn build-state [{:keys [dims directions pattern tiles rules]}]
  (let [tile-indices (range (count tiles))
        tile->index (zipmap tiles tile-indices)]
    {:pattern pattern
     :grid (init-grid dims directions (set tile-indices))
     :tiles tiles
     :rules (mapv (fn [[a dir b]] [(tile->index a) dir (tile->index b)]) rules)}))
