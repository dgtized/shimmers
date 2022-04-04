(ns shimmers.sketches.epicenter-of-impact
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.utils.intersect :as gisec]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn gen-points [bounds radius]
  (->> [(rp/inside-circle (gc/circle (g/unmap-point bounds (gv/vec2 0.5 0.5))  radius) dr/random)]
       (iterate (fn [points] (let [p (tm/+ (dr/rand-nth points)
                                          (v/polar (* radius (Math/sqrt (dr/random 0.7 0.9)))
                                                   (dr/random eq/TAU)))]
                              (if (and (g/contains-point? bounds p)
                                       (not (some (fn [o] (< (g/dist o p) (* 0.6 radius))) points)))
                                (conj points p)
                                points))))))

(defn make-start [epicenter]
  (for [t (take (dr/random-int 3 7) (dr/shuffle (butlast (tm/norm-range 12))))
        :let [direction (v/polar 1 (dr/gaussian (* eq/TAU t) 0.2))]]
    (gl/line2 epicenter (tm/+ epicenter direction))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [bounds (cq/screen-rect)
        points (nth (gen-points bounds (cq/rel-h 0.3)) (dr/random-int 1 3))]
    (println points)
    {:bounds bounds
     :lines (mapcat make-start points)}))

(defn intersects? [lines {[p q] :points}]
  (some (fn [{[a b] :points}]
          (let [{type :type isec :p} (gisec/intersect-line2-line2? p q a b)]
            ;; intersecting somewhere other than origin
            (when (and (= type :intersect)
                       (tm/delta= q isec 0.8)
                       (not (or (tm/delta= p isec 0.5)
                                (tm/delta= a isec 0.5))))
              isec))
          ) lines))

(defn growth [bounds lines line]
  (let [{[p q] :points} line
        dir (tm/- q p)
        len (tm/mag dir)
        angle (g/heading dir)
        q' (tm/+ p (v/polar (+ len 0.5) angle))
        line' (gl/line2 p q')]
    (if (:complete line)
      line
      (if (g/contains-point? bounds q')
        (if-let [isec (intersects? lines line')]
          (assoc (gl/line2 p isec) :complete true)
          line')
        (assoc line :complete true)))))

(defn spur [{[p q] :points} dir]
  (let [heading (g/heading (tm/- q p))
        angle (* dir (dr/gaussian 0.6 0.05))]
    (gl/line2 q (tm/+ q (v/polar 1.1 (+ heading angle))))))

(defn update-state [{:keys [bounds lines] :as state}]
  (let [lines' (map (partial growth bounds lines) lines)]
    (assoc state :lines (if (dr/chance 0.09)
                          (let [double (dr/rand-nth lines')]
                            (if-not (:complete double)
                              (into (remove #{double} lines')
                                    [(assoc double :complete true)
                                     (spur double 1)
                                     (spur double -1)])
                              lines'))
                          lines'))))

(defn draw [{:keys [lines]}]
  (q/background 1.0)
  (q/stroke-weight 1)
  (doseq [{[p q] :points} lines]
    (q/line p q)))

(sketch/defquil epicenter-of-impact
  :created-at "2022-04-03"
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
