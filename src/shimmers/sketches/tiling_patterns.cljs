(ns shimmers.sketches.tiling-patterns
  (:require [shimmers.common.ui.controls :as ctrl]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.rect :as rect]
            [thi.ng.geom.svg.adapter :as adapt]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as gv]
            [thi.ng.math.core :as tm]))

(defn svg
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs & body]
  (into [:svg
         (svg/svg-attribs
          attribs
          {:xmlns "http://www.w3.org/2000/svg"})]
        body))

;; palette from https://htmlcolors.com/palette/288/paleta-6
(def paleta-6 ["rgb(7,31,65)" "rgb(0,75,90)" "rgb(246,199,111)", "rgb(237,69,52)" "rgb(188,52,44)"])

(defn seed-rect [rows cols palette]
  (for [i (range rows)
        j (range cols)]
    {:pos (gv/vec2 i j) :fill (rand-nth palette)}))

(defn translate [cells pos]
  (map #(update % :pos geom/translate pos) cells))

(defn column [cells col]
  (filter (fn [cell] (= col (get-in cell [:pos 0]))) cells))

(defn max-height [cells]
  (inc (apply max (map #(get-in % [:pos 1]) cells))))

(defn max-width [cells]
  (inc (apply max (map #(get-in % [:pos 0]) cells))))

(defn rotate-r [cells]
  (mapcat (fn [col]
            (map-indexed
             (fn [j cell] (assoc cell :pos (gv/vec2 j col)))
             (reverse (column cells col))))
          (range (max-width cells))))

(defn rotate-l [cells]
  (let [height (max-height cells)]
    (mapcat (fn [row]
              (map-indexed
               (fn [i cell] (assoc cell :pos (gv/vec2 i (- height row 1))))
               (column cells row)))
            (reverse (range (max-width cells))))))

(defn cells->svg-rect [cells size]
  (let [rect (rect/rect 0 0 size size)]
    (for [{:keys [pos fill]} cells
          :let [[i j] pos]]
      (-> rect
          (geom/translate (tm/* pos (gv/vec2 size size)))
          (with-meta {:fill fill :key (str "cell-" i "-" j)})))))

(defn clockwise [w h]
  [(gv/vec2 0 0) (gv/vec2 w 0) (gv/vec2 w h) (gv/vec2 0 h)])

(defn counter-clockwise [w h]
  [(gv/vec2 0 0) (gv/vec2 0 h) (gv/vec2 w h) (gv/vec2 w 0)])

(defn rotate-group-r [dir seed]
  (let [w (max-width seed)
        h (max-height seed)]
    (mapcat translate
            (iterate rotate-r seed)
            (dir w h))))

(defn rotate-group-l [dir seed]
  (let [w (max-width seed)
        h (max-height seed)]
    (mapcat translate
            (iterate rotate-l seed)
            (dir w h))))

(defn flip-x [seed]
  (let [w (max-width seed)]
    (map (fn [cell]
           (update-in cell [:pos 0] (fn [x] (- w x 1))))
         seed)))

(defn flip-y [seed]
  (let [h (max-height seed)]
    (map (fn [cell]
           (update-in cell [:pos 1] (fn [y] (- h y 1))))
         seed)))

(defn mirror-x-group [seed]
  (concat seed
          (-> (flip-x seed)
              (translate (gv/vec2 (max-width seed) 0)))))

(defn mirror-y-group [seed]
  (concat seed
          (-> (flip-y seed)
              (translate (gv/vec2 0 (max-height seed))))))

(defn mirror-xy-group [seed]
  ((comp mirror-x-group mirror-y-group) seed))

(defn random-operations [depth]
  (take depth (shuffle [(partial rotate-group-r clockwise)
                        (partial rotate-group-r counter-clockwise)
                        (partial rotate-group-l clockwise)
                        (partial rotate-group-l counter-clockwise)
                        mirror-xy-group
                        mirror-xy-group])))

;; FIXME: something is still off sometimes about the initial square
;; I think at least one operation is transposing or something instead of what it's supposed to do
;; and then the error compounds?
(defn scene [mode]
  (let [n 6
        square-size 9
        depth 4
        seed (seed-rect n n paleta-6)
        operations (case mode
                     :random (random-operations depth)
                     :mirror (repeat depth mirror-xy-group)
                     :rotate-l (repeat depth (partial rotate-group-l clockwise))
                     :rotate-r (repeat depth (partial rotate-group-r clockwise)))
        size (* square-size n (Math/pow 2 depth))]
    (svg {:width size :height size :stroke "black"}
         (cells->svg-rect ((apply comp operations) seed)
                          square-size))))

;; TODO: add dropdowns/sliders to control n,square,depth?
(defn page []
  (adapt/all-as-svg (scene :random)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
