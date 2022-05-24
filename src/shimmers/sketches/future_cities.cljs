(ns shimmers.sketches.future-cities
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.line :as gl]))

(def width 600)
(def height 600)

(defn v [x y]
  (gv/vec2 (int x) (int y)))

(defn road [p q]
  (with-meta (gl/line2 p q) {:stroke-width 3}))

(defn building [[x y] w h]
  (rect/rect x y w h))

(defn city-start []
  {:year 0
   :cash 100
   :shapes [(road (v 300 280) (v 300 320))
            (road (v 300 280) (v 330 280))
            (building (v 285 290) 10 20)
            (building (v 305 285) 20 20)]})

(defn scene [state]
  (let [{:keys [shapes]} @state]
    (csvg/svg {:width width
               :height height
               :stroke "black"
               :fill "black"
               :stroke-width 0.5}
              shapes)))

(sketch/definition future-cities
  {:created-at "2022-05-24"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for
               (partial scene (ctrl/state (city-start)))
               :future-cities)
              "sketch-host"))
