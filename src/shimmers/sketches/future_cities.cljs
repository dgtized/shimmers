(ns shimmers.sketches.future-cities
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 600)
(def height 600)

(defn v [x y]
  (gv/vec2 (int x) (int y)))

(defn road [p q]
  (with-meta (gl/line2 p q)
    {:stroke-width 3
     :type :road}))

(defn extend-road [path]
  (let [{[p q] :points} path
        [x0 y0] p
        [x1 y1] q
        dir (tm/* (gv/vec2 (tm/signum (- x1 x0))
                           (tm/signum (- y1 y0)))
                  5)]
    (if (dr/chance 0.5)
      (road (tm/- p dir) q)
      (road p (tm/+ q dir)))))

(defn building [[x y] w h]
  (rect/rect x y w h))

(defn city-start []
  {:turn 0
   :cash 100
   :shapes [(road (v 300 280) (v 300 320))
            (road (v 300 280) (v 330 280))
            (building (v 285 290) 10 20)
            (building (v 305 285) 20 20)]})

(defn move [state]
  (let [change (->> (:shapes state)
                    (map-indexed (fn [i e] {:path i :entity e}))
                    (filter (fn [change] (= (:type (meta (:entity change))) :road)))
                    dr/rand-nth)]
    (-> change
        (update :entity extend-road)
        (assoc :cost 1))))

(defn next-turn [state]
  (let [{:keys [path entity cost]} (move state)]
    (-> state
        (update :shapes assoc path entity)
        (update :cash - cost)
        (update :turn inc))))

(defn scene [state]
  (let [{:keys [shapes]} @state]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "black"
               :stroke-width 0.5}
              shapes)))

(defn ui-controls [state]
  [:div
   [:button.generate {:on-click #(swap! state next-turn)} "Next Turn"]
   (debug/display state)])

(sketch/definition future-cities
  {:created-at "2022-05-24"
   :type :svg
   :tags #{}}
  (let [state (ctrl/state (city-start))]
    (ctrl/mount (view-sketch/page-for
                 (partial scene state)
                 :future-cities
                 (partial ui-controls state))
                "sketch-host")))
