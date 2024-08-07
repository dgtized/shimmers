(ns shimmers.sketches.differential-harmonics
  (:require
   [clojure.math :as math]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [O R]]
   [shimmers.model.harmonics :as harm]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn wob-osc [freq phase-freq]
  {:f freq :pf phase-freq})

(defn plot
  [center radius {[a b c] :freqs} s]
  (-> (gv/vec2)
      (tm/+ (R (:f a) (* 0.2 (O (:pf a) 0 s)) 0.70 s))
      (tm/+ (R (:f b) (* 0.6 (O (:pf b) 0 (- 1.0 s))) 0.25 s))
      (tm/+ (R (:f c) (* 0.8 (O (:pf c) 0 s)) 0.05 s))
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
  (let [weights (gv/vec3 0.6 0.25 -0.15)]
    (update params :freqs
            (fn [freqs]
              (map (fn [p w] (update p :f + (* w v)))
                   freqs weights)))))

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

(defn scene [ui-state {:keys [params scene-id]}]
  (csvg/svg-timed {:id scene-id
                   :width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 0.66}
    (shapes (merge params @ui-state))))

(defn parameters []
  (let [[a b c] (harm/abc)]
    {:n-points 1500
     :freqs [(wob-osc a (* (dr/random-sign) (dr/random-int 5)))
             (wob-osc b (* (dr/random-sign) (dr/random-int 7)))
             (wob-osc c (* (dr/random-sign) (dr/random-int 13)))]
     :perturb-dist (create-wobble (math/floor (tm/clamp (+ 0.5 (dr/gaussian 5.0 3)) 1 12))
                                  (- tm/SIXTH_PI)
                                  (dr/random-int 1 5)
                                  tm/SIXTH_PI)}))

(defn explanation [ui-state params]
  (fn []
    [:<>
     [:div.flexcols
      [:p.readable-width
       "Similar to helix but instead perturb oscillation frequency forward and backward from base harmonic."]
      [:div {:style {:width "10em"}}
       [ctrl/numeric ui-state "Remove Frequency" [:remove-freq] [-32 32 1]]]]
     [:div.readable-width
      (debug/pre-edn params)]]))

(sketch/definition differential-harmonics
  {:created-at "2024-05-16"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (let [ui-state (ctrl/state {:remove-freq 0})
         params (parameters)]
     (usvg/page (assoc sketch-args
                       :params params
                       :explanation (explanation ui-state params))
                (partial scene ui-state)))))
