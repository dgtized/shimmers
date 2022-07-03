(ns shimmers.sketches.clothoids
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.circle :as gc]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:mode :animation
                               :A 12
                               :L 60
                               :phi0 0.0
                               :N 40
                               :clockwise false
                               :from false
                               :scale 10}))
(defonce defo (debug/state))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0.0})

(defn update-state [state]
  (update state :t + 0.005))

(defn plot [r points]
  (q/begin-shape)
  (doseq [[x y] points]
    (cq/circle x y r)
    (q/vertex x y))
  (q/end-shape))

(defn pen-color [n]
  (q/stroke (mod (* n tm/PHI) 1.0) 0.7 0.4))

(defn clothoid->points [params]
  (let [{:keys [A L phi0 N clockwise from scale]} params]
    (->> ((if from eq/clothoid-from eq/clothoid)
          A L N
          (if clockwise -1 1) phi0
          (gv/vec2))
         (mapv #(tm/* % scale)))))

(defn clothoid-circle-at-end [points]
  (let [{:keys [A L clockwise from scale]} @ui-state
        tangent-point (if from (first points) (last points))
        ;; not working, maybe precision errors as it looks right L<20 ish?
        ;; tangent-angle (mod (eq/clothoid-tangent A (if clockwise 1 -1) L phi0) eq/TAU)
        [fp0 fp1] (if from (take 2 points) (reverse (take-last 2 points)))
        tangent-angle (g/heading (tm/- fp1 fp0))
        R (* scale (/ (* A A) L))]
    (swap! defo assoc
           :accuracy-limit (/ L (* 2 R)) ;; >3 is a problem
           :tangent-angle tangent-angle)
    [(v/-polar tangent-point R
               (+ tangent-angle (* (tm/sign L) (if clockwise -1 1) 0.5 Math/PI)))
     R]))

(defn draw-animation [{:keys [t]}]
  (let [length (+ 40 (* 20 (Math/sin t)))
        scaled (fn scaled [xs] (mapv #(tm/* % 11) xs))
        r 1.0
        A3 (+ 15 (* 5 (Math/cos t)))
        A4 (+ 8 (* 2 (Math/cos t)))]
    (q/stroke-weight 0.5)
    (pen-color 0)
    (plot r (scaled (eq/clothoid 18 length 20 1 0.0 (gv/vec2))))
    (pen-color 1)
    (plot r (scaled (eq/clothoid 12 length 50 1 Math/PI (gv/vec2))))
    (pen-color 2)
    (plot r (scaled (eq/clothoid-from A3 50 30 1 0 (gv/vec2 0.0 0.0))))
    (pen-color 3)
    (plot r (scaled (eq/clothoid-from A4 30 30 1 Math/PI (gv/vec2 0.0 0.0))))))

(defn draw-sandbox [from]
  (let [points (clothoid->points @ui-state)
        lp (if from (first points) (last points))
        [p r] (clothoid-circle-at-end points)]
    (q/translate 0 0)
    (q/stroke-weight 0.66)
    (q/stroke 0)
    (plot 1.0 points)
    (q/stroke 0.6 0.5 0.5)
    (cq/circle (first points) 2.5)
    (q/stroke 0.0 0.5 0.5)
    (cq/circle (last points) 2.5)
    (q/stroke-weight 0.5)
    (cq/circle p r)
    (q/line p lp)
    (swap! defo assoc
           :points points)))

(defn draw-circle-circle [_]
  (let [c1 (gc/circle (gv/vec2 4 3) 2.0)
        c2 (gc/circle (gv/vec2 -3 -2) 1.0)
        scaled-c1 (g/scale c1 30)
        scaled-c2 (g/scale c2 30)
        c1-angle (* 0.25 eq/TAU)
        c2-angle (* 0.75 eq/TAU)
        lambda-c1 1
        lambda-c2 1
        phi0-c1 (* 0.0 eq/TAU)
        phi0-c2 (* 0.5 eq/TAU)
        f1 (v/+polar (:p c1) (:r c1) 0)
        f2 (v/+polar (:p c2) (:r c2) Math/PI)
        scaled-f1 (g/scale f1 30)
        scaled-f2 (g/scale f2 30)]
    ;; TODO: solve for circle/circle connection clothoids between c1,c2
    (cq/circle scaled-c1)
    (cq/circle scaled-c2)
    (cq/circle scaled-f1 1.0)
    (cq/circle scaled-f2 1.0)
    (let [R (:r c1)
          L (eq/clothoid-length R (eq/clothoid-tau lambda-c1 c1-angle phi0-c1))
          A (Math/sqrt (* R L))]
      (swap! defo assoc :c1 {:circle c1 :phi0 phi0-c1 :R R :L L :A A})
      (plot 1.0 (mapv #(tm/* % 30) (eq/clothoid-from A L 30 lambda-c1 phi0-c1 f1))))
    (let [R (:r c2)
          L (eq/clothoid-length R (eq/clothoid-tau lambda-c2 c2-angle phi0-c2))
          A (Math/sqrt (* R L))]
      (swap! defo assoc :c2 {:circle c2 :phi0 phi0-c2 :R R :L L :A A})
      (plot 1.0 (mapv #(tm/* % 30) (eq/clothoid-from A L 30 lambda-c2 phi0-c2 f2))))))

(defn draw [state]
  (reset! defo {})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/translate (cq/rel-vec 0.5 0.5))
  (q/scale 1 -1) ;; flip y over x-axis
  (q/stroke 0.5)
  (q/line (cq/rel-vec -0.5 0) (cq/rel-vec 0.5 0))
  (q/line (cq/rel-vec 0 -0.5) (cq/rel-vec 0 0.5))
  (q/stroke 0)

  (let [{:keys [mode from]} @ui-state]
    (case mode
      :animation (draw-animation state)
      :sandbox (draw-sandbox from)
      :circle-circle (draw-circle-circle state))))

(defn sandbox-ctrls []
  [:div
   (ctrl/numeric ui-state "A" [:A] [0.0 30.0 0.01])
   (ctrl/numeric ui-state "Length" [:L] [-100.0 100.0 0.1])
   (ctrl/numeric ui-state "𝛷₀" [:phi0] [0.0 eq/TAU 0.01])
   (ctrl/numeric ui-state "N Points" [:N] [8 512 1])
   (ctrl/checkbox ui-state "Clockwise" [:clockwise])
   (ctrl/checkbox ui-state "From/To" [:from])
   (ctrl/numeric ui-state "Scale" [:scale] [1.0 50.0 0.1])])

(defn sandbox-view []
  [:div {:style {:font-size "0.8em"}}
   (let [points (clothoid->points @ui-state)
         [p r] (clothoid-circle-at-end points)]
     [:pre>code
      (interpose "\n" [(str (first points))
                       (str (last points))
                       (str "circle " p " " r)])])
   (debug/display defo)])

(defn ui-controls []
  (let [mode (:mode @ui-state)]
    [:div.flexcols
     (ctrl/container ;; TODO: params
      (ctrl/change-mode ui-state [:animation :sandbox :circle-circle])
      (case mode
        :animation nil
        :sandbox [sandbox-ctrls]
        :circle-circle nil))
     (case mode
       :animation nil
       :sandbox [sandbox-view]
       :circle-circle (debug/display defo))]))

(sketch/defquil clothoids
  :created-at "2021-11-23"
  :tags #{:demo}
  :size [800 600]
  :on-mount #(ctrl/mount ui-controls)
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
