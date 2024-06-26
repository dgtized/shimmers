(ns shimmers.sketches.mosaic-tiling
  (:require
   [clojure.math :as math]
   [shimmers.algorithm.mosaic :as mosaic]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
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

(def ui-settings
  (ctrl/state {:show-scene true
               :show-borders false}))

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

(def transformations
  {:rotate-rc (partial mosaic/rotate-group-r mosaic/clockwise)
   :rotate-rcc (partial mosaic/rotate-group-r mosaic/counter-clockwise)
   :rotate-lc (partial mosaic/rotate-group-l mosaic/clockwise)
   :rotate-lcc (partial mosaic/rotate-group-l mosaic/counter-clockwise)
   :mirror-ru (mosaic/mirror-group :right :up)
   :mirror-rd (mosaic/mirror-group :right :down)
   :mirror-lu (mosaic/mirror-group :left :up)
   :mirror-ld (mosaic/mirror-group :left :down)})

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

(defn svg-tile [size cell-size cells title]
  (let [rect (rect/rect 0 0 cell-size cell-size)]
    (csvg/svg {:width size :height size :stroke (if (:show-borders @ui-settings) "black" "none")}
      (into (for [{:keys [pos fill]} cells]
              (-> rect
                  (g/translate (tm/* pos (gv/vec2 cell-size cell-size)))
                  (with-meta {:fill fill})))
            (when title
              [[:title {:key "title"} title]])))))

;; FIXME: something is still off sometimes about the initial square
;; I think at least one operation is transposing or something instead of what it's supposed to do
;; and then the error compounds?
(defn scene [size {:keys [n depth seed operations]}]
  (binding [thi.ng.geom.svg.core/*ff* (f/float 1)]
    (let [scale (int (* size 0.85))]
      (csvg/timed
       (svg-tile scale
                 (/ scale (* n (math/pow 2 depth)))
                 ((apply comp (map transformations (take depth operations))) seed)
                 "Scene")))))

(defn examples [seed]
  (binding [thi.ng.geom.svg.core/*ff* (f/float 1)]
    [:div {:style {:column-count 2 :margin "2em"}}
     (for [[title transform] transformations]
       [:div {:key title}
        [svg-tile 200 (/ 200 (* 2 (math/sqrt (count seed))))
         (transform seed) title]])]))

;; TODO: add dropdowns/sliders to control n,square,depth?
(defn page []
  (let [{:keys [seed n palette operations] :as config} (scene-options)]
    (fn []
      (let [{:keys [show-scene]} @ui-settings]
        [:<>
         (if show-scene
           [:div.canvas-frame [scene 1024 config]]
           [examples seed])
         [:div.contained
          [:div.evencols
           [view-sketch/generate :mosaic-tiling]
           [:div
            (ctrl/checkbox ui-settings "Show Scene" [:show-scene])
            (ctrl/checkbox ui-settings "Show Borders" [:show-borders])]
           [palette/as-svg {} palette]]
          [:p]
          [:div.explanation
           [:div.flexcols
            [:p.readable-width
             "Patterns are generated by applying a sequence of rotations and
   reflection operations on an initial seed pattern. The initial seed is sampled
   from a palette, both of which can be seen to the right."]
            [svg-tile 128 (/ 128 n) seed "Seed Pattern"]]
           [:div ]
           (ctrl/details "Operations applied to seed in sequence"
                         [:ol
                          (doall
                           (for [[i op] (map-indexed vector operations)]
                             [:li {:key (str "step-" i)}
                              [:div op]
                              (scene 192 (assoc config :depth (inc i)))]))])]]]))))

(sketch/definition mosaic-tiling
  {:created-at "2021-04-09"
   :type :svg
   :tags #{:static :deterministic}}
  (ctrl/mount page))
