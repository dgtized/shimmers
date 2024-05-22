(ns shimmers.sketches.differential-harmonics
  (:require
   [clojure.math :as math]
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [O R]]
   [shimmers.model.harmonics :as harm]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn plot
  [center radius {[a b c] :freqs [a-osc b-osc c-osc] :freqs-osc} s]
  (-> (gv/vec2)
      (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.70 s))
      (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.25 s))
      (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
      (tm/* radius)
      (tm/+ center)))

(defn create-wobble [f1 p1 f2 p2]
  {:f1 f1
   :f2 f2
   :p1 p1
   :p2 p2})

(defn osc [{:keys [f1 f2 p1 p2]} s]
  (O f1 (+ p1 (O f2 p2 (- 1.0 s))) s))

(defn perturb [params v]
  (-> params
      (update-in [:freqs 0] + (* 0.6 v))
      (update-in [:freqs 1] + (* 0.25 v))
      (update-in [:freqs 2] + (* -0.15 v))))

(defn shapes [{:keys [n-points perturb-dist remove-freq] :as params}]
  (let [center (rv 0.5 0.5)
        radius (* 0.48 height)
        r (* 0.0033 height)]
    (for [s (tm/norm-range n-points)]
      (let [d1 (* 0.05 (osc perturb-dist s))
            d2 (* 0.05 (osc perturb-dist (- 1.0 s)))
            p (plot center radius (perturb params (- d1)) s)
            q (plot center radius (perturb params d2) s)
            dir (tm/normalize (tm/- p q) r)
            val (if (zero? remove-freq)
                  0
                  (Math/sin (+ (* remove-freq eq/TAU s)
                               (* 1.5 (Math/sin (* tm/PHI remove-freq s))))))]
        (cond (< -0.8 val 0.8)
              (csvg/group {}
                (gc/circle (tm/+ p dir) r)
                (gl/line2 p q)
                (gc/circle (tm/- q dir) r))
              (< val -0.8)
              (gc/circle p (* 0.2 r))
              (< 0.8 val)
              (gc/circle q (* 0.2 r)))))))

(defn scene [params]
  (csvg/svg-timed {:id "scene"
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.66}
    (shapes params)))

(defn parameters []
  (let [[a b c] (harm/abc)]
    {:n-points 2048
     :freqs [a b c]
     :freqs-osc [(* (dr/random-sign) (dr/random-int 5))
                 (* (dr/random-sign) (dr/random-int 7))
                 (* (dr/random-sign) (dr/random-int 13))]
     :perturb-dist (create-wobble (math/floor (tm/clamp (+ 0.5 (dr/gaussian 5.0 3)) 1 12))
                                  (- tm/QUARTER_PI)
                                  (dr/random-int 1 5)
                                  tm/QUARTER_PI)}))

(defn page []
  (let [ui-state (ctrl/state {:remove-freq 0})
        params (parameters)]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene (merge params @ui-state)]]
       [:div.flexcols
        [view-sketch/generate :differential-harmonics]
        [:div
         [kb/kb-action "alt-s" #(svg-export/download "scene" "differential-harmonics")]
         [:p.readable-width
          "Similar to helix but instead perturb oscillation frequency forward and backward from base harmonic."]
         [:div {:style {:width "20em"}}
          [ctrl/numeric ui-state "Remove Frequency" [:remove-freq] [-10 10 1]]]]]
       [:div.readable-width
        (debug/pre-edn params)]])))

(sketch/definition differential-harmonics
    {:created-at "2024-05-16"
     :tags #{}
     :type :svg}
  (ctrl/mount page))
