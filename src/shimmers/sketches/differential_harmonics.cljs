(ns shimmers.sketches.differential-harmonics
  (:require
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
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
      (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.65 s))
      (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.30 s))
      (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
      (tm/* radius)
      (tm/+ center)))

(defn perturb [params v]
  (-> params
      (update-in [:freqs 0] + (* 0.6 v))
      (update-in [:freqs 1] + (* 0.25 v))
      (update-in [:freqs 2] + (* -0.15 v))))

(defn shapes [{:keys [n-points d-freq] :as params}]
  (let [center (rv 0.5 0.5)
        radius (* 0.48 height)
        r (* 0.0033 height)]
    (for [s (tm/norm-range n-points)]
      (let [d1 (* 0.04 (O d-freq 0 s))
            d2 (* 0.04 (O d-freq tm/QUARTER_PI s))
            p (plot center radius (perturb params (- d1)) s)
            q (plot center radius (perturb params d2) s)
            dir (tm/normalize (tm/- p q) r)]
        (csvg/group {}
          (gc/circle (tm/+ p dir) r)
          (gc/circle (tm/- q dir) r)
          (gl/line2 p q))))))

(defn scene [params]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "white"
                   :stroke-width 0.5}
    (shapes params)))

(defn parameters []
  (let [[a b c] (harm/abc)]
    {:n-points 2048
     :freqs [a b c]
     :freqs-osc [(* (dr/random-sign) (dr/random-int 5))
                 (* (dr/random-sign) (dr/random-int 7))
                 (* (dr/random-sign) (dr/random-int 13))]
     :d-freq (dr/random-int 1 7)}))

(defn page []
  (let [params (parameters)]
    (fn []
      [sketch/with-explanation
       [:div.canvas-frame [scene params]]
       [:div.flexcols
        [view-sketch/generate :differential-harmonics]
        [:p "Similar to helix but instead perturb oscillation frequency forward and backward from base harmonic."]]
       [:div.readable-width
        (debug/pre-edn params)]])))

(sketch/definition differential-harmonics
    {:created-at "2024-05-16"
     :tags #{}
     :type :svg}
  (ctrl/mount page))
