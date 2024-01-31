(ns shimmers.sketches.display-tree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn generate-box [{p :p [width height] :size} angle]
  (let [[w h] (dr/weighted {(gv/vec2 4 3) 2
                            (gv/vec2 5 4) 2
                            (gv/vec2 16 9) 1})
        side (dr/gaussian (/ height 5) (/ height 100))
        ratio (/ (float w) (float h))
        ;; _ (println side ratio)
        box (gv/vec2 (* side ratio) (* side (/ 1.0 ratio)))
        [x y] p
        a (gv/vec2 (dr/random x (- width (:x box)))
                   (dr/random y (- height (:y box))))]
    (geometry/rotate-around-centroid
     (rect/rect a (tm/+ a box))
     (if (dr/chance 0.25)
       (dr/random (- angle) angle)
       0.0))))

(defn place-boxes [bounds angle]
  (loop [boxes [] attempts 0]
    (cond (> (count boxes) 6)
          boxes
          (> attempts 512)
          (recur [] 0)
          :else
          (let [candidate (generate-box bounds angle)]
            (if (some (fn [b] (collide/overlaps? (g/scale-size b 1.15) candidate))
                      boxes)
              (recur boxes (inc attempts))
              (recur (conj boxes candidate) (inc attempts)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:displays (place-boxes (cq/screen-rect 0.8)
                          (* 0.025 eq/TAU))
   :t 0.0})

(defn update-state [state]
  (update state :t + 0.01))

(defn draw [{:keys [displays t]}]
  (q/background 1.0)
  (doseq [[i screen] (map-indexed vector displays)
          :let [centroid (g/centroid screen)
                [x y] centroid
                f (eq/unit-sin (+ i
                                  (* 0.02 x t)
                                  (* 0.1 t)
                                  (* 2 (Math/sin (+ i (* 0.01 y t))))))]]
    (q/fill f)
    (qdg/draw screen)
    (q/fill (- 1.0 f))
    (let [s (str i "\n" (int x) "," (int y))]
      (q/text s
              (- x (* 0.5 (q/text-width s)))
              (- y (* 0.5 (q/text-ascent)))))))

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])])

(sketch/definition display-tree
  {:created-at "2024-01-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
