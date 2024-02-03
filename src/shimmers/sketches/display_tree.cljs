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
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:debug false}))

(defn generate-screen [{p :p [width height] :size} angle]
  (let [[w h] (dr/weighted {(gv/vec2 4 3) 4
                            (gv/vec2 5 4) 4
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
        display (rect/rect a (tm/+ a box))]
    {:display display
     :centroid (g/centroid display)
     :rotation angle}))

(defn rotated-box [{:keys [display rotation]}]
  (geometry/rotate-around display (rect/bottom-left display)
                          rotation))

(defn place-boxes [bounds angle]
  (loop [boxes [] attempts 0]
    (cond (> (count boxes) 6)
          boxes
          (> attempts 512)
          (recur [] 0)
          :else
          (let [candidate (generate-screen bounds angle)
                rbox (rotated-box candidate)]
            (if (some (fn [screen]
                        (-> screen
                            rotated-box
                            (g/scale-size 1.15)
                            (collide/overlaps? rbox)))
                      boxes)
              (recur boxes (inc attempts))
              (recur (conj boxes candidate) (inc attempts)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [displays (place-boxes (cq/screen-rect 0.8)
                              (* 0.025 eq/TAU))]
    {:displays displays
     :center (tm/* (reduce (fn [a screen] (tm/+ a (g/centroid (rotated-box screen))))
                           (gv/vec2)
                           displays)
                   (/ 1 (count displays)))
     :t 0.0}))

(defn create-node [display]
  {:display display
   ;; :children []
   })

(defn split [{p :p [w h] :size}]
  (let [m (dr/gaussian 0.5 0.05)]
    (if (> w h)
      [(rect/rect p (tm/+ p (gv/vec2 (* m w) h)))
       (rect/rect (tm/+ p (gv/vec2 (* m w) 0))
                  (tm/+ p (gv/vec2 w h)))]
      [(rect/rect p (tm/+ p (gv/vec2 w (* m h))))
       (rect/rect (tm/+ p (gv/vec2 0 (* m h)))
                  (tm/+ p (gv/vec2 w h)))])))

(defn subdiv [{:keys [display children] :as node} depth]
  (cond (seq children)
        (let [i (dr/random-int (count children))]
          (update-in node [:children i] subdiv (inc depth)))
        (< depth 8)
        (assoc node :children (mapv create-node (split display)))
        :else node))

(defn subdivide [{:keys [display children] :as screen}]
  (if (seq children)
    (let [i (dr/random-int (count children))]
      (update-in screen [:children i] subdiv 0))
    (assoc screen
           :children
           (mapv create-node (split display)))))

(defn combine [{:keys [children] :as node}]
  (if (seq children)
    (if-let [index (->> children
                        (map-indexed vector)
                        (keep (fn [[i n]] (when (seq (:children n)) i)))
                        seq)]
      (let [i (dr/rand-nth index)]
        (update-in node [:children i] combine))
      (dissoc node :children))
    node))

(defn collapse [{:keys [children] :as screen}]
  (if (seq children)
    (dissoc screen :children)
    screen))

;; TODO: fix rotation of bounds
(defn make-triangle [bounds]
  (let [centroid (g/centroid bounds)
        triangle
        (triangle/inscribed-equilateral (gv/vec2)
                                        (* 0.3 (min (g/width bounds)
                                                    (g/height bounds)))
                                        (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 4)))))]
    (fn [t] (-> triangle
               (g/rotate t)
               (g/translate centroid)
               qdg/draw))))

(defn add-symbol [{:keys [children display] :as screen}]
  (cond (and (seq children) (dr/chance 0.75))
        (let [i (dr/random-int (count children))]
          (update-in screen [:children i] add-symbol))
        (:symbol screen)
        screen
        :else
        (assoc screen :symbol (make-triangle display))))

(defn update-displays [displays _t]
  (let [i (dr/random-int (count displays))]
    (update displays i
            (fn [s]
              (case (dr/weighted {:divide 64
                                  :add-symbol 16
                                  :combine 8
                                  :collapse 2
                                  :nothing 2048})
                :divide (subdivide s)
                :combine (combine s)
                :collapse (collapse s)
                :add-symbol (add-symbol s)
                :nothing s)))))

(defn update-state [{:keys [t] :as state}]
  (-> state
      (update :displays update-displays t)
      (update :t + 0.01)))

(defn fader [i x y t]
  (let [theta (g/heading (gv/vec2 x y))
        ;; r (tm/mag (gv/vec2 x y))
        ]
    (mod (+ (eq/unit-sin (+ (* 0.01 x)
                            (* t tm/PHI)
                            (* 2 (eq/cube (Math/sin (+ i (* 0.01 y) (/ t tm/PHI)))))))
            ;; (eq/unit-sin (+ r (* 2 t)))
            (eq/unit-sin (- theta (* 1.2 t))))
         1.0)))

(defn rdraw
  [{:keys [display children symbol]}
   {:keys [depth p rotation i t center] :as dstate}]
  (if (seq children)
    (doseq [d children]
      (rdraw d (update dstate :depth inc)))
    (let [div (geometry/rotate-around display p rotation)
          [dx dy] (tm/- (g/centroid display) center)
          f (fader i dx dy (* t (/ 1 (Math/pow 1.33 depth))))]
      (q/fill f)
      (qdg/draw div)
      (when symbol
        (q/fill (- 1.0 f))
        (symbol t)))))

(defn draw [{:keys [displays center t]}]
  (q/background 1.0)
  (doseq [[i screen] (map-indexed vector displays)
          :let [{[x y] :centroid :keys [display rotation]} screen
                fade (fader i x y t)]]
    (rdraw screen
           {:depth 0
            :p (rect/bottom-left display)
            :center center
            :rotation rotation
            :i i
            :t t})
    (when (:debug @ui-state)
      (q/fill 0.0)
      (cq/circle center 10.0)
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
