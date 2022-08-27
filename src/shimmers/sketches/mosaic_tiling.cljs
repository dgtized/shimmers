(ns shimmers.sketches.mosaic-tiling
  (:require
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.core]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

(def ui-settings (ctrl/state {:show-scene true}))

;; https://lospec.com/palette-list/eulbink
(def eulbink-7 ["#ffffff" "#0ce6f2" "#0098db" "#1e579c"
                "#203562" "#252446" "#201533"])

;; https://lospec.com/palette-list/citrink
(def citrink-8 ["#ffffff" "#fcf660" "#b2d942" "#52c33f"
                "#166e7a" "#254d70" "#252446" "#201533"])

(def palettes
  (->> [;; originally https://htmlcolors.com/palette/288/paleta-6
        "https://artsexperiments.withgoogle.com/artpalette/colors/071f41-004b5a-f6c76f-ed4534-bc342c"
        ;; https://htmlcolors.com/palette/1345/cherry
        "https://artsexperiments.withgoogle.com/artpalette/colors/ffe5d7-e78b89-d2271f-b21d39-820539"
        ;; https://htmlcolors.com/palette/1469/eae5e2
        "https://artsexperiments.withgoogle.com/artpalette/colors/7af8d4-01585b-9fc3bb-b8e7c2-19cf54"
        ;; https://htmlcolors.com/palette/1268/egg
        "https://artsexperiments.withgoogle.com/artpalette/colors/f1af3a-0e0c0a-e2e1dd-7e4e06-64746c"
        ;; https://htmlcolors.com/palette/1111/sunflowers
        "https://artsexperiments.withgoogle.com/artpalette/colors/d1ae68-2e363c-71493e-e7dfd5-b37a29"
        ;; orange blue
        "https://artsexperiments.withgoogle.com/artpalette/colors/db9003-332f2e-20778c-d8cdb9-ba3a29"
        "https://artsexperiments.withgoogle.com/artpalette/colors/204354-34a3bb-f34c1c-241f1e-c0bbb8"]
       palette/from-urls
       (concat [eulbink-7 citrink-8])))

(defn seed-rect [rows cols palette]
  (for [i (range rows)
        j (range cols)]
    {:pos (gv/vec2 i j) :fill (dr/rand-nth palette)}))

(defn translate [cells pos]
  (map #(update % :pos g/translate pos) cells))

(defn column [cells col]
  (filterv (fn [cell] (= col (get-in cell [:pos 0]))) cells))

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
  (repeatedly depth #(dr/rand-nth (keys transformations))))

(defn scene-options []
  (let [n (dr/rand-nth [2 3 4 5 6])
        palette (dr/rand-nth palettes)
        seed (seed-rect n n palette)
        depth (cond (< n 3) 5
                    (< n 6) 4
                    :else 3)
        operations (random-operations depth)]
    {:n n
     :palette palette
     :seed seed
     :depth depth
     :operations operations}))

(comment
  (let [seed [{:pos [0 0] :fill "a"}
              {:pos [1 0] :fill "b"}
              {:pos [1 1] :fill "c"}
              {:pos [0 1] :fill "d"}]]
    {:right (take 4 (iterate rotate-r seed))
     :left (take 4 (iterate rotate-l seed))}))

(defn svg-tile [size cell-size cells]
  (let [rect (rect/rect 0 0 cell-size cell-size)]
    (csvg/svg {:width size :height size :stroke "black"}
      (for [{:keys [pos fill]} cells]
        (-> rect
            (g/translate (tm/* pos (gv/vec2 cell-size cell-size)))
            (with-meta {:fill fill}))))))

;; FIXME: something is still off sometimes about the initial square
;; I think at least one operation is transposing or something instead of what it's supposed to do
;; and then the error compounds?
(defn scene [size {:keys [n depth seed operations]}]
  (binding [thi.ng.geom.svg.core/*ff* (f/float 1)]
    (svg-tile size
              (/ size (* n (Math/pow 2 depth)))
              ((apply comp (map transformations (take depth operations))) seed))))

(defn examples [seed]
  (binding [thi.ng.geom.svg.core/*ff* (f/float 1)]
    [:div {:style {:column-count 3}}
     (for [example [(rotate-group-l clockwise seed)
                    (rotate-group-l counter-clockwise seed)
                    (rotate-group-r clockwise seed)
                    (rotate-group-r counter-clockwise seed)
                    (mirror-xy-group seed)
                    (mirror-yx-group seed)]]
       [:div [svg-tile 256 16 example]])]))

;; TODO: add dropdowns/sliders to control n,square,depth?
(defn page []
  (let [{:keys [seed n palette operations] :as config} (scene-options)
        {:keys [show-scene]} @ui-settings]
    [:div
     (if show-scene
       [:div.canvas-frame (time (scene 1024 config))]
       [examples seed])
     [:div.flexcols
      [:p {:style {:margin-left "2em"}} (view-sketch/generate :mosaic-tiling)]
      [:div {:style {:margin-top "auto" :margin-bottom "auto"}}
       (ctrl/checkbox ui-settings "Show Scene" [:show-scene])]]
     [:div.explanation.readable-width
      [:p
       "Patterns are generated by applying a sequence of rotations and reflection
   operations on an initial seed pattern. For the pattern above the seed was"]
      [:div.center (svg-tile 128 (/ 128 n) seed)]
      [:p "The pattern was generated randomly from the following palette:"]
      [:div (palette/as-svg {:class "center"
                             :width (* 60 (count palette))
                             :height 30}
                            palette)]
      (ctrl/details "Operations applied to seed in sequence"
                    [:ol
                     (for [[i op] (map-indexed vector operations)]
                       [:li {:key i}
                        [:div op]
                        (scene 192 (assoc config :depth (inc i)))])])]]))

(sketch/definition mosaic-tiling
  {:created-at "2021-04-09"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount page "sketch-host"))
