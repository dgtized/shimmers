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

;; https://htmlcolors.com/palette/1345/cherry
(def cherry-5 ["#FFE5D7" "#E78B89" "#D2271F" "#B21D39" "#820539"])

;; https://htmlcolors.com/palette/1469/eae5e2
(def eae5e2-5 ["#7AF8D4" "#01585B" "#9FC3BB" "#B8E7C2" "#19CF54"])

;; https://lospec.com/palette-list/eulbink
(def eulbink-7 ["#ffffff" "#0ce6f2" "#0098db" "#1e579c"
                "#203562" "#252446" "#201533"])

;; https://lospec.com/palette-list/citrink
(def citrink-8 ["#ffffff" "#fcf660" "#b2d942" "#52c33f"
                "#166e7a" "#254d70" "#252446" "#201533"])

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

(defn mirror-yx-group [seed]
  ((comp mirror-y-group mirror-x-group) seed))

(def transformations
  {:rotate-rc (partial rotate-group-r clockwise)
   :rotate-rcc (partial rotate-group-r counter-clockwise)
   :rotate-lc (partial rotate-group-l clockwise)
   :rotate-lcc (partial rotate-group-l counter-clockwise)
   :mirror-xy mirror-xy-group
   :mirror-yx mirror-yx-group})

(defn random-operations [depth]
  (repeatedly depth #(rand-nth (keys transformations))))

;; FIXME: something is still off sometimes about the initial square
;; I think at least one operation is transposing or something instead of what it's supposed to do
;; and then the error compounds?
(defn scene []
  (let [screen-size 900
        n (rand-nth [3 4 5 6])
        depth (if (< n 6) 4 3)
        cell-size (/ screen-size (* n (Math/pow 2 depth)))
        palette (rand-nth [paleta-6 cherry-5 eae5e2-5 eulbink-7 citrink-8])
        seed (seed-rect n n palette)
        operations (random-operations depth)]
    (.log js/console {:n n :ops operations :colors palette})
    (time (svg {:width screen-size :height screen-size :stroke "black"}
               (cells->svg-rect ((apply comp (map transformations operations)) seed)
                                cell-size)))))

;; TODO: add dropdowns/sliders to control n,square,depth?
(defn page []
  (adapt/all-as-svg (scene)))

(defn ^:export run-sketch []
  ;; 20210409
  (ctrl/mount page "svg-host"))
