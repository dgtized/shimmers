(ns shimmers.sketches.stretchy-curves
  (:require
   [clojure.math :as math]
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.screen :as screen]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry.ellipse :as ellipse]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]))

(defonce ui-state
  (ctrl/state {:screen-size "800x600"}))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 0.5)
  {:outline
   (dr/rand-nth [(gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5))
                 (g/as-polygon (gc/circle (cq/rel-vec 0.5 0.5) (cq/rel-h 0.5)) 6)
                 (ellipse/ellipse (cq/rel-vec 0.5 0.5)
                                  (cq/rel-w 0.49)
                                  (cq/rel-h 0.49))
                 (cq/screen-rect 0.9)])
   :weights (repeatedly 2 #(dr/random -0.1 0.1))
   :phase (repeatedly 2 dr/random-tau)
   :time (/ (q/millis) 1000.0)})

(defn update-state [{:keys [time] :as state}]
  (let [dt (- (/ (q/millis) 1000.0) time)]
    (-> state (update :time + dt))))

(defn draw [{[w0 w1] :weights [p0 p1] :phase :keys [time outline]}]
  (q/background 1.0)
  (let [N 256
        center (cq/rel-vec 0.5 0.5)
        t (+ (* 1.5 time) (* 1.5 (math/sin (+ (* 0.35 time) (* 2 (math/sin (* 0.4 time)))))))]
    (dotimes [i N]
      (let [pidx (/ (float i) N)
            a (mod (- pidx
                      (* 0.003 t)
                      (* 0.17 (math/sin (+ (* w0 i) (* 0.25 t) p0))))
                   1.0)
            b (- 1.0
                 (mod (+ pidx
                         (* 0.005 t)
                         (* 0.23 (math/sin (- (* w1 i) (* 0.35 t) p1))))
                      1.0))
            d1 (+ 0.05 (* 0.25 (eq/unit-sin (- (* 0.7 t) pidx))))
            d2 (+ 0.5 (* 0.4 (math/sin (+ (* 0.4 t)
                                          (math/sin (+ (* eq/TAU pidx) (* 0.3 t)))))))
            [px py] (g/point-at outline a)
            [qx qy] (g/point-at outline b)
            [rx ry] (tm/mix center (g/point-at outline (sm/mix-mod a b d1)) d2)
            [sx sy] (tm/mix center (g/point-at outline (sm/mix-mod a b (- 1.0 d1))) d2)]
        (q/bezier px py rx ry sx sy qx qy)))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size (screen/parse-size (:screen-size @ui-state))
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [ctrl/container
    [ctrl/dropdown ui-state "Screen Size" [:screen-size]
     (screen/sizes)
     {:on-change #(view-sketch/restart-sketch :stretchy-curves)}]]])

(sketch/definition stretchy-curves
  {:created-at "2024-06-01"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
