(ns shimmers.sketches.radial-expansion
  (:require
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn new-planet [p r depth spoke-theta]
  (assoc (gc/circle p r)
         :depth depth
         :spoke-theta spoke-theta
         :spokes
         (dr/weighted {0 1
                       2 2
                       3 4
                       5 4
                       6 4
                       7 3
                       8 3
                       12 1})))

(defn spoke [{:keys [p r]} distance theta]
  (-> (gl/line2 (v/polar r theta)
                (v/polar (+ r distance) theta))
      (g/translate p)))

(defn init []
  [(-> (rv (dr/random 0.2 0.8)
           (dr/random 0.2 0.8))
       (new-planet (* (dr/random 0.08 0.12) height) 0 (dr/random eq/TAU))
       (update :spokes max 3))])

(defn evolve [bounds shapes]
  (let [expansions
        (->> shapes
             (filter #(> (:spokes %) 0))
             (mapcat
              (fn [{:keys [p r spokes depth spoke-theta] :as planet}]
                (let [distance (* r (tm/clamp (dr/gaussian tm/PHI 0.4) 0.8 2))
                      fan (and (pos? depth) (dr/chance 0.5))
                      width (* eq/TAU (if fan (/ 1 (dr/rand-nth [1.5 2 3 4 5 6 7 8])) 1))
                      arc-length (* 0.5 (+ r distance) (/ width spokes))
                      radius (min (* (tm/clamp (dr/gaussian 0.5 0.3) 0.1 0.9) arc-length)
                                  r)
                      fixed-radius (dr/chance 0.3)
                      t0 (+ spoke-theta
                            (cond fan
                                  (- (/ width 2))
                                  (even? spokes)
                                  (* eq/TAU (/ 1 (* 2 spokes)))
                                  :else 0))]
                  (for [t (if fan (tm/norm-range spokes)
                              (butlast (tm/norm-range spokes)))
                        :let [theta (+ (* width t) t0)
                              radius (if fixed-radius radius (* radius (dr/random 0.6 1.1)))
                              center (v/+polar p (+ r distance radius) theta)
                              new (new-planet center
                                              radius
                                              (inc depth)
                                              theta)]
                        :when (and (geometry/contains-circle? bounds new)
                                   (not (geometry/circles-overlap? planet new)))]
                    {:planet new
                     :spoke (spoke planet distance theta)})))))]
    (concat (map (fn [p] (dissoc p :spokes)) shapes)
            (mapcat (fn [{:keys [planet spoke]}] [planet spoke]) expansions))))

(defn shapes [bounds depth]
  (for [s (nth (iterate (partial evolve bounds) (init)) depth)
        :let [d (:depth s)]]
    (if (contains? #{0 3} d)
      (vary-meta s assoc :fill "black")
      s)))

(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.5}
    (shapes (rect/rect 0 0 width height)
            (dr/random-int 3 6))))

(sketch/definition radial-expansion
  {:created-at "2022-06-21"
   :type :svg
   :tags #{}}
  (ctrl/mount (view-sketch/page-for scene :radial-expansion)
              "sketch-host"))
