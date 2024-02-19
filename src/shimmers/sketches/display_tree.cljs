(ns shimmers.sketches.display-tree
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.random-points :as rp]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.quil-draws-geom :as qdg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.geometry :as geometry]
   [shimmers.math.geometry.collisions :as collide]
   [shimmers.math.geometry.triangle :as triangle]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))
(defonce ui-state (ctrl/state {:debug false}))

(defn ratio [[w h] side]
  (let [ratio (/ (float h) (float w))]
    (gv/vec2 side (int (* side ratio)))))

(defn generate-display
  [{p :p [width height] :size}
   {:keys [dims size] :as info}
   angle-mag]
  (let [inch (/ (q/height) 70)
        side (* inch size)
        box (ratio dims side)
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
        (q/with-translation [x (+ y (* 0.025 (q/text-ascent)))]
          (q/with-rotation [rotation]
            (q/text-char letter 0 0)))))))

(defn make-rect-growth [bounds]
  (let [period (dr/rand-nth [2.0 4.0 6.0 8.0])
        scale (dr/weighted [[(fn [t] (mod (/ t period) 1.0)) 1]
                            [(fn [t] (mod (/ (- t) period) 1.0)) 1]
                            [(fn [t] (eq/unit-sin (/ t period))) 1]])]
    (fn [p rotation t]
      (-> bounds
          (geometry/rotate-around p rotation)
          (g/scale-size (scale t))
          (qdg/draw)))))

(defn R [f p a s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn make-spinner [bounds]
  (let [radius (min (g/width bounds) (g/height bounds))
        direction (* (dr/rand-nth [1 -1]) (dr/gaussian 0.75 0.05))
        [a b c] (repeatedly 3 #(dr/random-int -4 4))]
    (fn [p rotation t f]
      (q/no-fill)
      (q/stroke (- 1.0 f))
      (let [center (geometry/rotate-around (g/centroid bounds) p rotation)
            t (* direction t)
            path (for [s (tm/norm-range 128)]
                   (->
                    (gv/vec2)
                    (tm/+ (R (+ a (Math/sin (* 0.2 t))) (Math/cos (* 0.5 t)) 1.0 s))
                    (tm/+ (R (+ b (Math/cos (* 0.2 t))) (Math/sin (* 0.5 t)) 1.0 s))
                    (tm/+ (R (+ c (Math/cos (* 0.5 t))) (Math/sin (* 0.7 t)) 0.5 s))
                    (tm/* (* 0.15 radius))
                    (tm/+ center)))]
        (cq/draw-path path))
      (q/no-stroke))))

(defn make-static [bounds]
  (let [{size :size} bounds
        [cols rows] (ratio size (dr/rand-nth [12 16 20]))
        center (g/centroid bounds)
        mode (dr/weighted {:static 1.0
                           :sweep 2.0
                           :radial 2.0})
        divisions
        (for [div (g/subdivide bounds {:cols cols :rows rows})
              :let [c (g/centroid div)
                    r (g/dist c center)
                    theta (g/heading (tm/- c center))]]
          (assoc div :value
                 (case mode
                   :static
                   (let [v (dr/random)]
                     (fn [t _f] (mod (+ t v) 1.0)))
                   :radial
                   (fn [t f] (eq/unit-sin (+ r (* 2 t) f)))
                   :sweep
                   (fn [t _f] (eq/unit-sin (+ theta t))))))]
    (fn [p rotation t f]
      (doseq [{:keys [value] :as div} divisions]
        (q/fill (value t f))
        (-> div
            (geometry/rotate-around p rotation)
            qdg/draw)))))

(defn add-animation
  [{:keys [children display] :as screen} t]
  (cond (seq children)
        (let [i (dr/random-int (count children))]
          (update-in screen [:children i] add-animation t))
        (:animation screen)
        screen
        :else
        (let [mk-anim (dr/weighted [[make-triangle 0.5]
                                    [make-letter 0.1]
                                    [make-rect-growth 1]
                                    [make-spinner 1]
                                    [make-static 0.5]])]
          (assoc screen :animation (mk-anim display t)))))

(defn all-displays [displays]
  (->> displays
       (mapcat (fn [display]
                 (tree-seq (fn [{:keys [children]}] (not-empty children))
                           (fn [{:keys [children]}] children)
                           display)))
       (map (fn [n] (update n :children count)))))

(comment
  (for [n (range 200)]
    [n (* 64 (Math/exp (* -0.04 n)))]))

(defn update-displays [displays t]
  (let [i (dr/random-int (count displays))
        ramp (Math/pow 2 (* 6 (tm/smoothstep* 0.92 1.0 (mod (/ t 50.0) 1.0))))
        tree (all-displays displays)
        display-f
        (dr/weighted [[subdivide (* 64 (Math/exp (* -0.04 (count tree))))]
                      [add-animation 48]
                      [combine (* 8 ramp)]
                      [collapse (* 2 ramp)]
                      [identity 4096]])]
    (swap! defo assoc :tree tree)
    (update displays i display-f t)))

(defn update-state [{:keys [mode t] :as state}]
  (let [df ({:divisions update-displays} mode)]
    (-> state
        (update :displays df t)
        (update :t + 0.01))))

(defn fader [i x y t]
  (let [theta (g/heading (gv/vec2 x y))
        ;; r (tm/mag (gv/vec2 x y))
        wobble
        (eq/unit-sin (+ (* 0.01 x)
                        (* 0.5 t tm/PHI)
                        (* 2 (eq/cube (Math/sin (+ i (* 0.01 y) (/ t tm/PHI)))))))
        orientation (eq/unit-sin (- theta (* 1.2 t)))]
    (mod (+ (* 0.75 wobble)
            ;; (eq/unit-sin (+ (* tm/PHI r) t))
            (* 0.5 orientation))
         1.0)))

(defn rdraw
  [{:keys [display children animation]}
   {:keys [depth p rotation i t center] :as dstate}]
  (if (seq children)
    (doseq [d children]
      (rdraw d (update dstate :depth inc)))
    (let [div (geometry/rotate-around display p rotation)
          [dx dy] (tm/- (g/centroid display) center)
          f (fader i dx dy (* t (/ 1 (Math/pow 1.33 depth))))]
      (q/fill f)
      (qdg/draw div)
      (when animation
        (q/fill (- 1.0 f))
        (animation p rotation t f)))))

(defn draw [{:keys [displays center t]}]
  (q/background 1.0)
  (q/no-stroke)
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
    [ctrl/checkbox-after ui-state "Debug" [:debug]]
    (when (:debug @ui-state)
      [debug/display defo])]])

(sketch/definition display-tree
  {:created-at "2024-01-30"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
