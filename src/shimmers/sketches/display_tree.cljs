(ns shimmers.sketches.display-tree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
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

(defn box-ratio [[w h] side]
  (let [ratio (/ (float w) (float h))]
    (gv/vec2 (* side ratio) (* side (/ 1.0 ratio)))))

(defn generate-display
  [{p :p [width height] :size}
   {:keys [dims size] :as info}
   angle-mag]
  (let [inch (/ (q/height) 70)
        side (* inch size)
        box (box-ratio dims side)
        a (tm/+ p (gv/vec2 (dr/random 0 (- width (:x box)))
                           (dr/random 0 (- height (:y box)))))
        angle (if (dr/chance 0.25)
                (dr/random (- angle-mag) angle-mag)
                0.0)
        display (rect/rect a (tm/+ a box))]
    {:display display
     :centroid (g/centroid display)
     :info info
     :rotation angle}))

(defn generate-screen [bounds angle-mag]
  (let [[w h] (dr/weighted {(gv/vec2 4 3) 4
                            (gv/vec2 5 4) 4
                            (gv/vec2 16 9) 1})]
    (generate-display bounds
                      {:dims [w h]
                       :size (dr/rand-nth [19 20 21 23])}
                      angle-mag)))

(defn rotated-box [{:keys [display rotation]}]
  (geometry/rotate-around display (rect/bottom-left display)
                          rotation))

(def display-dims
  [{:dims [5 4] :size 19}
   {:dims [5 4] :size 19}
   {:dims [16 9] :size 21}
   {:dims [16 9] :size 21}
   {:dims [16 10] :size 23}
   {:dims [16 10] :size 23}
   {:dims [16 10] :size 14}])

(defn place-boxes [bounds angle]
  (loop [displays (dr/shuffle display-dims)
         boxes []
         attempts 0]
    (cond (empty? displays)
          boxes
          (> attempts 512)
          (place-boxes bounds angle)
          :else
          (let [candidate (generate-display bounds (first displays) angle)
                rbox (rotated-box candidate)]
            (if (some (fn [screen]
                        (-> screen
                            rotated-box
                            (g/scale-size 1.15)
                            (collide/overlaps? rbox)))
                      boxes)
              (recur displays boxes (inc attempts))
              (recur (rest displays) (conj boxes candidate) (inc attempts)))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (q/text-align :center :center)
  (let [displays (place-boxes (cq/screen-rect 0.9)
                              (* 0.025 eq/TAU))]
    {:displays displays
     :center (tm/* (reduce (fn [a screen] (tm/+ a (g/centroid (rotated-box screen))))
                           (gv/vec2)
                           displays)
                   (/ 1 (count displays)))
     :mode :divisions
     :t 0.0}))

(defn create-node [display]
  {:display display
   ;; :children []
   })

(defn cut-split
  ([quad] (cut-split quad (dr/gaussian 0.5 0.05)))
  ([{p :p [w h] :size} m]
   (if (> w h)
     [(rect/rect p (tm/+ p (gv/vec2 (* m w) h)))
      (rect/rect (tm/+ p (gv/vec2 (* m w) 0))
                 (tm/+ p (gv/vec2 w h)))]
     [(rect/rect p (tm/+ p (gv/vec2 w (* m h))))
      (rect/rect (tm/+ p (gv/vec2 0 (* m h)))
                 (tm/+ p (gv/vec2 w h)))])))

(defn point-quad [{p :p [w h] :size :as quad} point]
  (if (g/contains-point? quad point)
    (let [[x y] (tm/- point p)]
      (map (fn [r] (g/translate r p))
           [(rect/rect 0 0 x y)
            (rect/rect x 0 (- w x) y)
            (rect/rect 0 y x (- h y))
            (rect/rect x y (- w x) (- h y))]))
    quad))

(comment
  (point-quad (rect/rect 10 10 10 10) (gv/vec2 15 15))
  (point-quad (rect/rect 10 10 10 10) (gv/vec2 25 15)))

(defn split [quad]
  (case (dr/weighted {:cut-split 4
                      :point-quad 1})
    :cut-split (cut-split quad)
    :point-quad (point-quad quad
                            (rp/sample-point-at quad 0.5 0.5))))

(defn subdiv [{:keys [display children] :as node} depth]
  (cond (seq children)
        (let [i (dr/random-int (count children))]
          (update-in node [:children i] subdiv (inc depth)))
        (< depth 6)
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

(defn make-triangle [bounds]
  (let [circle (dr/chance 0.5)
        limit (min (g/width bounds) (g/height bounds))
        s (if circle (dr/random 0.2 0.8) 0)
        d (* s (* 0.3 limit))
        triangle
        (triangle/inscribed-equilateral (gv/vec2)
                                        (* (- 1.0 s) 0.3 limit)
                                        (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 4)))))
        rate (* (dr/rand-nth [1 -1])
                (dr/gaussian 1.0 0.1))
        rate2 (* (dr/rand-nth [1 -1])
                 (dr/gaussian 1.25 0.2))]
    (fn [p rotation t]
      (let [box (geometry/rotate-around bounds p rotation)
            centroid (g/centroid box)]
        (-> triangle
            (g/rotate (* rate t))
            (g/translate (gv/vec2 d 0))
            (g/rotate (* rate2 t))
            (g/translate centroid)
            qdg/draw)))))

(defn make-letter [bounds]
  (let [size (g/height bounds)
        letter (char (+ 65 (dr/rand-nth (range 26))))]
    (fn [p rotation _t]
      (let [box (geometry/rotate-around bounds p rotation)
            [x y] (g/centroid box)]
        (q/text-size (int (* size (/ 2 3))))
        (q/stroke 0.0 0.5 0.5 1.0)
        (q/with-translation [x y]
          (q/with-rotation [rotation]
            (q/text-char letter 0 0)))
        (q/stroke 0.0)))))

(defn add-symbol
  [{:keys [children display] :as screen}]
  (cond (and (seq children) (dr/chance 0.75))
        (let [i (dr/random-int (count children))]
          (update-in screen [:children i] add-symbol))
        (:symbol screen)
        screen
        :else
        (let [mk-sym (dr/weighted [[make-triangle 1]
                                   [make-letter 1]])]
          (assoc screen :symbol (mk-sym display)))))

(defn update-displays [displays _t]
  (let [i (dr/random-int (count displays))]
    (update displays i
            (fn [s]
              (case (dr/weighted {:divide 32
                                  :add-symbol 32
                                  :combine 8
                                  :collapse 2
                                  :nothing 4096})
                :divide (subdivide s)
                :combine (combine s)
                :collapse (collapse s)
                :add-symbol (add-symbol s)
                :nothing s)))))

(defn update-state [{:keys [mode t] :as state}]
  (let [df ({:divisions update-displays} mode)]
    (-> state
        (update :displays df t)
        (update :t + 0.01))))

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
        (symbol p rotation t)))))

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
      (q/text-size 12)
      (q/fill 0.0)
      (cq/circle center 10.0)
      (q/fill (- 1.0 fade))
      (let [s (str i "\n" (int x) "," (int y) "\n" (str (:info screen)))]
        (q/with-translation [x y]
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
