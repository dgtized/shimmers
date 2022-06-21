(ns shimmers.sketches.radial-expansion
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]
   [thi.ng.geom.circle :as gc]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.math.core :as tm]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.core :as g]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn new-planet [p r]
  (assoc (gc/circle p r)
         :spokes (dr/rand-nth [3 5 6 7])))

(defn init []
  {:planets [(new-planet (rv 0.5 0.5) 64)]
   :spokes []})

(defn evolve [{:keys [planets spokes] :as state}]
  (let [expansions (->> planets
                        (filter #(> (:spokes %) 0))
                        (mapcat
                         (fn [{:keys [p r spokes] :as e}]
                           (let [distance (* r (dr/random 0.3 1.8))
                                 t0 (dr/random 0 1.0)]
                             (for [t (butlast (tm/norm-range spokes))
                                   :let [theta (+ (* eq/TAU t) t0)
                                         radius (* r (dr/random 0.3 0.8))]]
                               {:planet (new-planet (tm/+ p (v/polar (+ r distance radius) theta)) radius)
                                :spoke (g/translate (gl/line2 (v/polar r theta)
                                                              (v/polar (+ r distance) theta))
                                                    p)})))))]
    (assoc state
           :planets (concat (map (fn [p] (dissoc p :spokes)) planets)
                            (map :planet expansions))
           :spokes (concat spokes (map :spoke expansions)))))

(defn shapes [depth]
  (apply concat (vals (nth (iterate evolve (init)) depth))))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
            (shapes (dr/random-int 2 5))))

(sketch/definition radial-expansion
  {:created-at "2022-06-21"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :radial-expansion)
              "sketch-host"))
