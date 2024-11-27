(ns shimmers.sketches.remaining
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.wobble :as mw :refer [O R]]
   [shimmers.model.harmonics :as harm]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:screen-size "900x600"}))
(defonce defo (debug/state {}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (let [[a b c] (harm/abc)]
    {:params
     {:n-points 200
      :freqs
      [a b c]
      :freqs-osc
      [(* (dr/random-sign) (dr/random-int 5))
       (* (dr/random-sign) (dr/random-int 7))
       (* (dr/random-sign) (dr/random-int 13))]
      :phase 0
      :angle-osc (dr/random-int 6 32)
      :size-osc (dr/random-int 6 32)
      :radius-osc (dr/random-int 6 32)}}))

(defn harmonic-loop [center radius {:keys [n-points phase freqs freqs-osc]}]
  (let [[a b c] freqs
        [a-osc b-osc c-osc] freqs-osc]
    (for [s (tm/norm-range n-points)]
      (let [s (+ s phase)]
        (-> (gv/vec2)
            (tm/+ (R a (* 0.2 (O a-osc 0 s)) 0.65 s))
            (tm/+ (R b (* 0.6 (O b-osc 0 (- 1.0 s))) 0.30 s))
            (tm/+ (R c (* 0.8 (O c-osc 0 s)) 0.05 s))
            (tm/* radius)
            (tm/+ center))))))

(defn update-state [state]
  (update-in state [:params :phase] +
             (-> (dr/gaussian 2.0 0.5)
                 (tm/clamp 0.5 5.0)
                 (/ 30000.0))))

;; TODO: consider drawing each stream of points instead of all points per frame?
(defn draw-point
  [{:keys [angle-osc radius-osc size-osc phase]}
   radius p q s]
  (let [z (tm/- q p)
        r (+ radius (* 0.25 radius (O radius-osc (* 0.25 phase) s)))
        angle (* math/PI (O angle-osc (* math/PI (math/sin (+ (* s radius) phase))) s))
        delta  (tm/normalize (g/rotate z (+ angle (/ eq/TAU 4)))
                             (+ (* 2 tm/PHI r) (* 2 r (O size-osc (+ 2.0 (* 0.2 angle)) s))))
        line-delta (tm/normalize delta (- (tm/mag delta) r))]
    (cq/circle (tm/+ p delta) r)
    (cq/circle (tm/- p delta) r)
    (q/line (tm/+ p line-delta)
            (tm/- p line-delta))))

(defn draw-frame [{:keys [n-points] :as params}]
  (q/stroke-weight ((dr/sample-between #(dr/gaussian 0.225 0.15) 0.1 0.6)))
  (q/stroke 0.0 ((dr/sample-between #(dr/gaussian 0.1 0.1) 0.005 0.5)))
  (q/no-fill)
  (let [height (q/height)
        pts (harmonic-loop (cq/rel-vec 0.5 0.5) (* 0.45 height) params)
        radius (* 0.0075 height)]
    (doseq [[[p q] s] (mapv vector
                            (partition 2 1 pts)
                            (tm/norm-range n-points))]
      (draw-point params radius p q s))))

(defn draw [{:keys [params]}]
  (reset! defo params)
  (let [t (/ (q/millis) 1000.0)]
    (if (> t 2.0)
      (q/no-loop)
      (draw-frame params))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div.flexcols
    [ctrl/container
     [ctrl/dropdown ui-state "Screen Size" [:screen-size]
      (screen/sizes)
      {:on-change #(view-sketch/restart-sketch :remaining)}]]
    [:div [:p]
     [debug/display defo]]]])

(sketch/definition remaining
  {:created-at "2024-05-02"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
