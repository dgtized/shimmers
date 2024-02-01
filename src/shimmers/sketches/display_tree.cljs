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

(defonce ui-state (ctrl/state {:debug false}))

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
                   (dr/random y (- height (:y box))))
        angle (if (dr/chance 0.25)
                (dr/random (- angle) angle)
                0.0)
        rect (rect/rect a (tm/+ a box))]
    {:box rect
     :centroid (g/centroid rect)
     :rotation angle}))

(defn rotated-box [{:keys [box rotation]}]
  (geometry/rotate-around box (rect/bottom-left box) rotation))

(defn place-boxes [bounds angle]
  (loop [boxes [] attempts 0]
    (cond (> (count boxes) 6)
          boxes
          (> attempts 512)
          (recur [] 0)
          :else
          (let [candidate (generate-box bounds angle)
                rbox (rotated-box candidate)]
            (if (some (fn [b]
                        (collide/overlaps? (g/scale-size (rotated-box b) 1.15)
                                           rbox))
                      boxes)
              (recur boxes (inc attempts))
              (recur (conj boxes candidate) (inc attempts)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:displays (place-boxes (cq/screen-rect 0.8)
                          (* 0.025 eq/TAU))
   :t 0.0})

(defn split [{p :p [w h] :size}]
  (if (> w h)
    [(rect/rect p (tm/+ p (gv/vec2 (* 0.5 w) h)))
     (rect/rect (tm/+ p (gv/vec2 (* 0.5 w) 0))
                (tm/+ p (gv/vec2 w h)))]
    [(rect/rect p (tm/+ p (gv/vec2 w (* 0.5 h))))
     (rect/rect (tm/+ p (gv/vec2 w (* 0.5 h)))
                (tm/+ p (gv/vec2 w h)))]))

(defn subdivide [{:keys [box divisions] :as screen}]
  (if divisions
    screen
    (assoc screen
           :divisions
           (split box))))

(defn combine [{:keys [divisions] :as screen}]
  (if divisions
    (dissoc screen :divisions)
    screen))

(defn update-displays [displays _t]
  (map (fn [s]
         (case (dr/weighted {:divide 1
                             :combine 1
                             :nothing 120})
           :divide (subdivide s)
           :combine (combine s)
           :nothing s))
       displays))

(defn update-state [{:keys [t] :as state}]
  (-> state
      (update :displays update-displays t)
      (update :t + 0.01)))

(defn fader [i x y t]
  (eq/unit-sin (+ (* 0.005 x t)
                  (* 0.2 t)
                  (* 2 (eq/cube (Math/sin (+ i (* 0.005 y t))))))))

(defn draw [{:keys [displays t]}]
  (q/background 1.0)
  (doseq [[i screen] (map-indexed vector displays)
          :let [{[x y] :centroid :keys [box rotation divisions]} screen
                rbox (rotated-box screen)
                fade (fader i x y t)]]
    (if (seq divisions)
      (doseq [d divisions
              :let [div (geometry/rotate-around d (rect/bottom-left box) rotation)
                    [dx dy] (g/centroid d)]]
        (q/fill (fader i dx dy t))
        (qdg/draw div))
      (do
        (q/fill fade)
        (qdg/draw rbox)))
    (when (:debug @ui-state)
      (q/fill (- 1.0 fade))
      (let [s (str i "\n" (int x) "," (int y))]
        (q/with-translation [(- x (* 0.5 (q/text-width s)))
                             (- y (* 0.5 (q/text-ascent)))]
          (q/with-rotation [rotation]
            (q/text s 0 0)))))))

(defn page []
  [sketch/with-explanation
   (sketch/component
     :size [800 600]
     :setup setup
     :update update-state
     :draw draw
     :middleware [m/fun-mode framerate/mode])
   [:div
    [ctrl/checkbox-after ui-state "Debug" [:debug]]]])

(sketch/definition display-tree
  {:created-at "2024-01-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
