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

;; TODO: use a Cornu or clothoid spiral
;; https://pwayblog.com/2016/07/03/the-clothoid/ origin is the midpoint where r
;; is infinity and L is 0, and each side of the curve is one of the two circles.
;; https://math.stackexchange.com/questions/1785816/calculating-coordinates-along-a-clothoid-betwen-2-curves
;; https://etrr.springeropen.com/articles/10.1007/s12544-013-0119-8
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
          (if clockwise 1 -1) phi0
          (gv/vec2))
         (mapv #(tm/* % scale)))))

(defn clothoid-circle-at-end [points]
  (let [{:keys [A L phi0 clockwise from scale]} @ui-state
        tangent-point (if from (first points) (last points))
        ;; not working, maybe precision errors as it looks right L<20 ish?
        ;; tangent-angle (mod (eq/clothoid-tangent A (if clockwise 1 -1) L phi0) eq/TAU)
        [fp0 fp1] (if from (take 2 points) (reverse (take-last 2 points)))
        tangent-angle (g/heading (tm/- fp1 fp0))
        R (* scale (/ (* A A) L))]
    (swap! defo assoc
           :accuracy-limit (/ L (* 2 R)) ;; >3 is a problem
           :tangent-angle tangent-angle)
    [(tm/- tangent-point (v/polar R (+ tangent-angle (* (tm/sign L) (if clockwise 1 -1) 0.5 Math/PI))))
     R]))

(defn draw-animation [{:keys [t]}]
  (let [length (+ 40 (* 20 (Math/sin t)))
        scaled (fn scaled [xs] (mapv #(tm/* % 11) xs))
        r 1.0
        A3 (+ 15 (* 5 (Math/cos t)))
        A4 (+ 8 (* 2 (Math/cos t)))]
    (q/stroke-weight 0.5)
    (pen-color 0)
    (plot r (scaled (eq/clothoid 18 length 20 -1 0.0 (gv/vec2))))
    (pen-color 1)
    (plot r (scaled (eq/clothoid 12 length 50 -1 Math/PI (gv/vec2))))
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

(defn draw [state]
  (reset! defo {})
  (q/background 1.0)
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke 0)
  (q/translate (cq/rel-vec 0.5 0.5))

  (let [{:keys [mode from]} @ui-state]
    (case mode
      :animation (draw-animation state)
      :sandbox (draw-sandbox from))))

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
                       (str "circle " p " " r)])])])

(defn ui-controls []
  (let [mode (:mode @ui-state)]
    [:div.flexcols
     (ctrl/container ;; TODO: params
      (ctrl/change-mode ui-state [:animation :sandbox])
      (case mode
        :animation nil
        :sandbox [sandbox-ctrls]))
     (case mode
       :animation nil
       :sandbox [sandbox-view])]))

(sketch/defquil clothoids
  :created-at "2021-11-23"
  :tags #{:demo}
  :size [800 600]
  :on-mount #(do (debug/mount defo)
                 (ctrl/mount ui-controls))
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
