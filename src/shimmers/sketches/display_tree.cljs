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
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce defo (debug/state))
(defonce ui-state (ctrl/state {:debug false}))

(defn ratio [[w h] side]
  (let [ratio (/ (float h) (float w))]
    (gv/vec2 (int side) (int (* side ratio)))))

(defn generate-display
  [{p :p [width height] :size}
   {:keys [dims size] :as info}
   angle-mag]
  (let [inch (/ (q/height) 60)
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

(defn rotated-centroid [bounds pos rotation]
  (g/centroid (geometry/rotate-around bounds pos rotation)))

(def display-dims
  [{:dims [5 4] :size 19} ;; 1280x1024
   {:dims [5 4] :size 19} ;; 1280x1024
   {:dims [16 10] :size 21} ;; 1440x900
   {:dims [16 10] :size 21} ;; 1440x900
   {:dims [16 9] :size 23} ;; 1920x1080
   {:dims [16 9] :size 23} ;; 1920x1080
   ;; {:dims [16 10] :size 14} ;; 2560x1440 broken laptop
   ;; {:dims [16 9] :size 27} ;; 3840x2160
   ])

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
                            (g/scale-size 1.1)
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
     :heat 0.0
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

(defn segmented-path [path]
  (doseq [[a b] (partition 2 2 path)]
    (apply q/point a)
    (apply q/point b)
    (q/line (tm/mix a b 0.25) (tm/mix a b 0.75))))

(defn dotted-path [path]
  (doseq [p path]
    (cq/circle p 1.0)))

(defn choose-path-draw []
  (dr/rand-nth [segmented-path cq/draw-path dotted-path]))

(defn cube-wobble [r c]
  (fn [t p]
    (eq/cube (Math/sin (+ (* r t) p c)))))

(defn make-triangle [bounds]
  (let [limit (min (g/width bounds) (g/height bounds))
        s (if (dr/chance 0.75) (dr/random 0.15 0.66) 0)
        radius (* s (* 0.65 limit))
        n-triangles (dr/weighted {1 1 3 2 5 3 7 2})
        angle (* eq/TAU (dr/rand-nth (butlast (tm/norm-range 4))))
        spin (* (dr/rand-nth [1 -1])
                (dr/gaussian 1.5 0.25))
        orbit (* (dr/rand-nth [1 -1])
                 (dr/gaussian 1.1 0.25))
        dir (dr/rand-nth [1 -1])]
    (fn [p rotation t f]
      (let [centroid (rotated-centroid bounds p rotation)
            t (* dir t)]
        (q/fill (- 1.0 f))
        (dotimes [i n-triangles]
          (let [spacing (/ i n-triangles)]
            (-> (gv/vec2)
                (triangle/inscribed-equilateral
                 (* (- 1.0 s) 0.25 limit) angle)
                (g/rotate (* spin t))
                (g/translate (gv/vec2 radius 0))
                (g/rotate (+ (* eq/TAU spacing) (* orbit t)))
                (g/translate centroid)
                qdg/draw)))))))

(defn make-letter [bounds]
  (let [size (g/height bounds)
        letter (cond (dr/chance 0.1)
                     (char (+ 65 (dr/rand-nth (range 26))))
                     (dr/chance 0.2)
                     (char (+ 48 (dr/random-int 10)))
                     :else
                     (char (+ 48 (dr/random-int 2))))]
    (fn [p rotation t f]
      (q/fill (- 1.0 f))
      (let [[x y] (rotated-centroid bounds p rotation)]
        (q/text-size (+ (* size (/ 2.0 3)) (* 9.0 (Math/sin (* tm/PHI t)))))
        (q/with-translation [x (+ y (* 0.025 (q/text-ascent)))]
          (q/with-rotation [rotation]
            (q/text-char letter 0 0)))))))

(defn make-rect-growth [bounds]
  (let [period (dr/rand-nth [3.0 4.0 6.0 8.0])
        scale (dr/weighted [[(fn [t] (mod (/ t period) 1.0)) 1]
                            [(fn [t] (mod (/ (- t) period) 1.0)) 1]
                            [(fn [t] (eq/unit-sin (/ t period))) 1]])]
    (fn [p rotation t f]
      (q/fill (- 1.0 f))
      (-> bounds
          (geometry/rotate-around p rotation)
          (g/scale-size (scale t))
          (qdg/draw)))))

(defn R [f p a s]
  (v/polar a (* eq/TAU (+ (* s f) p))))

(defn make-spinner [bounds]
  (let [radius (min (g/width bounds) (g/height bounds))
        direction (* (dr/rand-nth [1 -1]) (dr/gaussian 0.7 0.06))
        [a b c] (repeatedly 3 #(dr/random-int -4 4))]
    (fn [p rotation t f]
      (q/no-fill)
      (q/stroke-weight (+ 0.75 (* 1.25 (eq/unit-sin (+ (* 0.3 b t) (Math/sin (* 0.7 a t) c))))))
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
      (q/stroke-weight 1.0)
      (q/no-stroke))))

(defn O [f p v d s]
  (+ v (* d (Math/sin (* eq/TAU (+ (* s f) p))))))

(defn make-loop-spinner [bounds]
  (let [radius (min (g/width bounds) (g/height bounds))
        direction (* (dr/rand-nth [1 -1]) (dr/gaussian 0.7 0.06))
        major (dr/random-int 1 10)
        minor (* (dr/weighted {-1 5 1 1})
                 (dr/random-int 1 10))
        osc1 (dr/random-int 1 10)
        val1 (dr/weighted {0.0 4 0.1 1 0.15 1})
        amp1 (dr/weighted {0.05 1 0.1 4 0.15 1 0.2 1})
        osc2 (* (dr/weighted {-1 3 1 1})
                (dr/random-int 1 10))
        val2 (dr/weighted {0.0 4 0.1 1 0.15 1})
        amp2 (dr/weighted {0.05 1 0.1 4 0.15 1 0.2 1})
        draw (choose-path-draw)]
    (fn [p rotation t f]
      (q/no-fill)
      (q/stroke-weight (+ 0.75 (* 1.0 (Math/sin (+ (* (/ 1.0 major) t) (Math/sin (* t (/ 1.0 minor))))))))
      (q/stroke (- 1.0 f))
      (let [center (geometry/rotate-around (g/centroid bounds) p rotation)
            t (* direction t)
            path (for [s (tm/norm-range 192)]
                   (->
                    (gv/vec2)
                    (tm/+ (R major (O osc1 t val1 amp1 s) 1.0 s))
                    (tm/+ (R minor (O osc2 s val2 amp2 s) 1.0 s))
                    (tm/* (* 0.15 radius))
                    (tm/+ center)))]
        (draw path))
      (q/stroke-weight 1.0)
      (q/no-stroke))))

;; these aren't easing in from fade because they calculate their own fade
(defn make-static [bounds]
  (let [{size :size} bounds
        [cols rows] (ratio size (dr/rand-nth [12 16 20]))
        center (g/centroid bounds)
        mode (dr/weighted {:static 1.0
                           :sweep 2.0
                           :radial 3.0})
        dir (dr/rand-nth [-1 1])
        divisions
        (for [div (g/subdivide bounds {:cols cols :rows rows})
              :let [c (g/centroid div)
                    r (g/dist c center)
                    theta (g/heading (tm/- c center))]]
          (assoc div :value
                 (case mode
                   :static
                   (let [v (dr/random)]
                     (fn [t _f] (mod (+ (* dir t) v) 1.0)))
                   :radial
                   (let [factor (Math/pow 2.0 (dr/rand-nth [0 0 6]))]
                     (fn [t _f] (eq/unit-sin (+ (/ r factor) (* 1.5 (* dir t))))))
                   :sweep
                   (let [blades (dr/random-int 4)]
                     (fn [t _f] (eq/unit-sin (+ (+ blades theta) (* dir t))))))))]
    (fn [p rotation t f]
      (doseq [{:keys [value] :as div} divisions]
        (q/fill (value t f))
        (-> div
            (geometry/rotate-around p rotation)
            qdg/draw)))))

(defn make-wobble [bounds]
  (let [{ul :p [w h] :size} bounds
        wobble (cube-wobble 2 (dr/random-tau))
        po0 (dr/random-tau)
        pw1 (dr/random-tau)
        dir (* (dr/rand-nth [-1 1]) (dr/gaussian 0.95 0.1))
        fxw0 (dr/gaussian 0.0 0.33)
        fxw1 (dr/gaussian 0.0 0.33)
        fxo (if (dr/chance 0.5)
              (dr/gaussian 1.0 0.5)
              (dr/gaussian 24.0 4.0))
        draw (choose-path-draw)]
    ;; self overlap example
    ;; 4.5993827884932115 6.249439750487722 4.443832286713721 -1 -0.2878850416584414 -0.20994315524082172 21.157634493128658
    (println bounds po0 pw1 dir fxw0 fxw1 fxo draw)
    (fn [p rotation t f]
      (q/no-fill)
      (q/stroke-weight (if (> fxo 4.0)
                         0.8
                         (+ 0.75 (* 0.75 (eq/unit-sin (+ (* fxw0 t) (Math/sin (* fxw1 t) pw1)))))))
      (q/stroke (- 1.0 f))
      (let [t (* dir t)
            path (for [s (tm/norm-range 128)
                       :let [x (* 1.5 eq/TAU s)
                             wob0 (wobble t (* x fxw0))
                             wob1 (Math/sin (+ (* x fxw1) (* 1 t) pw1))
                             v (Math/sin (+ (* x fxo) (* 4 t) po0
                                            (* (/ 4 3) wob0) (* (/ 1 3) wob1)))]]
                   (-> (gv/vec2 (* w s) (+ (* 0.5 h) (* 0.4 h v)))
                       (g/rotate rotation)
                       (tm/+ (g/rotate (tm/- ul p) rotation))
                       (tm/+ p)))]
        (draw path))
      (q/stroke-weight 1.0)
      (q/no-stroke))))

;; (gc/circle (v/+polar (g/point-at (gc/circle (gv/vec2) 10) 0) 9.0 Math/PI) 9.0)

(defn make-spiral [bounds]
  (let [size (min (g/width bounds) (g/height bounds))
        dr-rate (dr/random 0.45 1.2)
        dr-rate' (dr/gaussian 1.0 0.2)
        dr-phase (dr/random-tau)
        dr-phase' (dr/random-tau)
        t-phase (dr/random-tau)
        t-phase' (dr/random-tau)]
    (fn [pos rotation t f]
      (let [center (rotated-centroid bounds pos rotation)
            dr (- 0.975 (* 0.15 (eq/unit-sin (+ (* dr-rate t) dr-phase
                                                (Math/sin (+ (* dr-rate' t)
                                                             dr-phase'))))))
            dt (+ 0.05 (* (/ eq/TAU 12) (eq/unit-cos (+ (/ t tm/PHI) size f))))
            r (* 0.46 size)
            theta (* 4 eq/TAU (Math/sin (+ (* 0.06 t) (eq/cube (Math/sin (+ (* 0.13 t) t-phase'))) t-phase)))
            circles (->> {:circle (gc/circle (gv/vec2) r)
                          :theta theta}
                         (iterate
                          (fn [{:keys [circle theta]}]
                            (let [{:keys [p r]} circle
                                  r' (* dr r)]
                              {:circle
                               (gc/circle (v/+polar p (- r' r) theta)
                                          r')
                               :theta (+ theta dt)})))
                         (take-while (fn [{:keys [circle]}] (> (:r circle) 3.0))))]
        (q/no-fill)
        (q/stroke-weight 0.6)
        (q/stroke (- 1.0 f))
        (doseq [c (map :circle circles)]
          (qdg/draw (g/translate c center)))
        (q/stroke-weight 1.0)
        (q/no-stroke)))))

(defn make-helix [bounds]
  (let [{ul :p [w h] :size} bounds
        dir (* (dr/rand-nth [-1 1]) (dr/gaussian 0.85 0.06))
        r (dr/gaussian 2.0 0.1)
        p (dr/random-tau)
        wobble0 (cube-wobble (dr/gaussian 1.5 0.2) (dr/random-tau))
        wobble1 (cube-wobble (dr/gaussian 2.5 0.5) (dr/random-tau))
        helices (dr/rand-nth [1 2 3])
        width-w (if (dr/chance 0.5) 0.0 (dr/random 1.5 4.0))]
    (fn [pos rotation t f]
      (let [t (* dir t)
            spots 50.0]
        (q/no-stroke)
        (q/fill (- 1.0 f))
        (dotimes [j helices]
          (dotimes [i spots]
            (let [y (* (+ 0.025 (* 0.95 (/ (+ i 0.5) (float spots)))) h)
                  width (* (inc (* width-w (eq/unit-sin (* 0.2 t)))) (inc j))
                  w0 (wobble0 t (+ (* 0.03 y) width))
                  w1 (wobble1 t (+ (* 0.05 y) (* 2 width)))
                  v (Math/sin (+ (/ y 10.0) (* r t) p w0 (* 0.3 w1)))]
              (-> (gv/vec2 (* w (+ 0.5 (* 0.425 v)))
                           y)
                  (g/rotate rotation)
                  (tm/+ (g/rotate (tm/- ul pos) rotation))
                  (tm/+ pos)
                  (cq/circle (+ 0.75 (* 0.5 (eq/unit-sin (+ w0 w1 (* 0.1 t))))))))))))))

;; (map (fn [t] [t (mapv (fn [y] (mod (+ (/ y 3.0) t) 1.0)) (range 3))]) (range 0 1.0 0.05))

(defn make-tunnel [bounds]
  (let [n 4
        dir (dr/rand-nth [1 -1])
        time-f (if (dr/chance 0.5)
                 (fn [t] (* dir t))
                 (fn [t] (* 10 (Math/sin (* 0.15 t)))))]
    (fn [pos rotation t f]
      (q/no-fill)
      (q/stroke-weight 4.0)
      (q/stroke (- 1.0 f))
      (dotimes [i n]
        ;; (q/fill (if (even? i) (- 1.0 f) f))
        (let [v (/ (+ (time-f t) i) (float n))]
          (-> bounds
              (g/scale-size (mod v 1.0))
              (geometry/rotate-around pos rotation)
              (qdg/draw))))
      (q/stroke-weight 1.0))))

(defn make-circle-blob [bounds]
  (let [radius (* 0.4 (min (g/width bounds) (g/height bounds)))
        samples 40
        blades (+ (dr/random-int 2 7) (dr/gaussian 0.0 0.1))
        phase (dr/random-tau)
        rf (fn [theta t]
             (let [w (Math/sin (+ (* blades theta) (* 1.5 t)))]
               (eq/unit-sin (+ (* 0.66 blades theta)
                               t
                               (* 2 (eq/cube w))
                               phase))))]
    (fn [pos rotation t f]
      (let [center (rotated-centroid bounds pos rotation)]
        (q/no-fill)
        (q/stroke-weight 1.0)
        (q/stroke (- 1.0 f))
        (q/begin-shape)
        (doseq [s (tm/norm-range samples)]
          (let [theta (* eq/TAU 1.1 s)
                r (rf theta t)]
            (apply q/curve-vertex
                   (v/+polar center
                             (* radius (+ 0.75 (* 0.25 r)))
                             (+ theta phase)))))
        (q/end-shape)
        (q/stroke-weight 1.0)))))

(defn make-hex [bounds]
  (let [scale (* 0.435 (Math/pow 0.98 (dr/random-int 5)))
        radius (* scale (min (g/width bounds) (g/height bounds)))
        spin-phase (dr/random-tau)
        o-phase (dr/random-tau)]
    (fn [pos rotation t f]
      (let [center (rotated-centroid bounds pos rotation)
            outline (Math/sin (+ (* 0.4 t) o-phase (* 1.5 (Math/sin (* 0.6 t)))))
            r radius
            spin (* eq/TAU (Math/sin (+ (* 0.13 t) spin-phase (Math/sin (* 0.17 t)))))]
        (if (pos? outline)
          (do (q/no-stroke)
              (q/fill (* outline (- 1.0 f))))
          (do (q/stroke (* (- outline) (- 1.0 f)))
              (q/no-fill)
              (q/stroke-weight (+ 0.5 (* 5.0 (- outline))))))
        (cq/draw-shape
         (for [s (butlast (tm/norm-range 6))]
           (v/+polar center r (+ spin (* s eq/TAU)))))
        (q/stroke-weight 1.0)))))

(defn make-bars [bounds]
  (let [n (dr/random-int 2 13)
        dir (dr/rand-nth [1 -1])
        F (dr/rand-nth [:xy :yx])
        w (/ 1.0 (inc n))]
    (fn [pos rotation t f]
      (q/no-stroke)
      (let [t (* dir 0.15 t)
            cuts (->> n
                      tm/norm-range
                      (map (fn [x] (mod (+ x t) 1.0)))
                      (map-indexed vector)
                      (sort-by second))
            [li lc] (last cuts)]
        (when (> (+ lc w) 1.0)
          (q/fill (if (even? li) (- 1.0 f) f))
          (-> (rect/rect (g/unmap-point bounds (F (gv/vec2 0 0)))
                         (g/unmap-point bounds (F (gv/vec2 (- (+ lc w) 1.0) 1.0))))
              (g/translate (tm/- pos))
              (g/rotate rotation)
              (g/translate pos)))
        (doseq [[i a] cuts
                :let [a' (gv/vec2 a 0)
                      b' (gv/vec2 (min 1.0 (+ a w)) 1.0)]
                :when (< a 1.0)]
          (q/fill (if (even? i) (- 1.0 f) f))
          (cq/draw-polygon
           (-> (rect/rect (g/unmap-point bounds (F a'))
                          (g/unmap-point bounds (F b')))
               (g/translate (tm/- pos))
               (g/rotate rotation)
               (g/translate pos))))))))

(defn add-animation
  [{:keys [children display] :as screen} t]
  (cond (seq children)
        (let [i (dr/random-int (count children))]
          (update-in screen [:children i] add-animation t))
        (:animation screen)
        (dissoc screen :animation)
        :else
        (let [mk-anim (dr/weighted [[make-triangle 1.0]
                                    [make-letter 0.75]
                                    [make-rect-growth 2.0]
                                    [make-spinner 2.5]
                                    [make-loop-spinner 2.5]
                                    [make-wobble 2.25]
                                    [make-spiral 1.75]
                                    [make-static 1.0]
                                    [make-helix 2.0]
                                    [make-tunnel 1.0]
                                    ;; [make-circle-blob 1.0]
                                    [make-hex 1.0]
                                    [make-bars 1.0]
                                    ])]
          (assoc screen
                 :animation {:animate (mk-anim display t)
                             :rate (tm/clamp (dr/gaussian 0.5 0.15) 0.05 10.0)
                             :t0 t}))))

(defn all-displays [displays]
  (->> displays
       (mapcat (fn [display]
                 (tree-seq (fn [{:keys [children]}] (not-empty children))
                           (fn [{:keys [children]}] children)
                           display)))))

(comment
  (for [n (range 200)]
    [n (* 128 (Math/exp (* -0.12 n)))]))

(defn update-displays [displays t _heat]
  (let [i (dr/random-int (count displays))
        tree (all-displays displays)
        leaves (filter (comp empty? :children) tree)
        ramp (Math/exp (* 7 (tm/smoothstep* 0.85 0.95 (mod (/ t 50.0) 1.0))))
        n (count leaves)
        animations (count (filter :animation leaves))
        display-f
        (dr/weighted [[subdivide (* 64 (Math/exp (* -0.06 (+ n (dec ramp)))))]
                      [add-animation (* 32 (Math/exp (* -0.125 (+ animations (dec ramp)))))]
                      [combine (* 8 ramp)]
                      [collapse (* 2 ramp)]])]
    (swap! defo assoc
           :displays n
           ;; :leaves leaves
           :ramp ramp
           :animations animations)
    (update displays i display-f t)))

(defn update-state [{:keys [t heat] :as state}]
  (swap! defo assoc :heat heat)
  (let [event (> (+ heat (dr/gaussian 0.4 0.1)) 0.99)]
    (cond-> state
      event (update :displays update-displays t heat)
      event (update :heat - (dr/random 0.5))
      true (update :heat + (tm/clamp (dr/gaussian (/ 1 300.0) 0.01) 0.0001 0.01))
      true (update :t + 0.01))))

;; Why is this sometimes being overridden?
(defn fader [i x y t]
  (let [theta (g/heading (gv/vec2 x y))
        ;; r (tm/mag (gv/vec2 x y))
        wobble
        (eq/unit-sin (+ (* 0.02 x)
                        (* 0.15 t tm/PHI)
                        (* 2 (eq/cube (Math/sin (+ i (* 0.02 y) (/ t (* 1.5 tm/PHI))))))))
        orientation (eq/unit-sin (- theta (* 0.8 t)))
        unison-blink (eq/unit-sin t)
        ramp-mode (tm/smoothstep* 0.8 0.9 (eq/unit-cos (+ (* 0.05 t) (Math/sin (* 0.17 t)))))]
    (swap! defo assoc :ramp-mode ramp-mode :unison-blink unison-blink)
    (tm/mix*
     (tm/smoothstep* 0.15 0.85
                     (+ (* 0.8 wobble)
                        (* 0.4 orientation)))
     (tm/smoothstep* 0.2 0.8 unison-blink)
     ramp-mode)))

(defn rdraw
  [{:keys [display children animation]}
   {:keys [depth p rotation i t center] :as dstate}]
  (let [div (geometry/rotate-around display p rotation)
        [dx dy] (tm/- (g/centroid display) center)
        f (fader i dx dy (* t (/ 1 (Math/pow 1.33 depth))))]
    (if (= depth 0)
      (q/stroke 0.0)
      (q/no-stroke))
    (q/fill f)
    (qdg/draw div)
    (q/no-stroke)
    (if (seq children)
      (doseq [d children]
        (rdraw d (update dstate :depth inc)))
      (when-let [{:keys [animate t0 rate]} animation]
        (q/fill f)
        (let [ease-in-f (tm/mix* (- 1.0 f) f (tm/smoothstep* 0.0 1.0 (* rate (- t t0))))]
          (animate p rotation t ease-in-f))))))

(defn draw [{:keys [displays center t]}]
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 1.0)
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
