(ns shimmers.sketches.fluid
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.macros.loop :as loop :include-macros true]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.ndarray.core :as nd]))

;; other reading:
;; https://shahriyarshahrabi.medium.com/gentle-introduction-to-fluid-simulation-for-programmers-and-technical-artists-7c0045c40bac

;; From https://en.wikipedia.org/wiki/Lattice_Boltzmann_methods &
;; https://www.math.nyu.edu/~billbao/report930.pdf
(def directions
  [(gv/vec2 0 0) ;; 0 staying
   (gv/vec2 1 0) ;; 1 east
   (gv/vec2 0 1) ;; 2 north
   (gv/vec2 -1 0) ;; 3 west
   (gv/vec2 0 -1) ;; 4 south
   (gv/vec2 1 1) ;; 5 north-east
   (gv/vec2 -1 1) ;; 6 north-west
   (gv/vec2 -1 -1) ;; 7 south-west
   (gv/vec2 1 -1) ;; 8 south-east
   ])

(defn make-lattice [width height]
  (let [zeros (repeatedly (* width height) (fn [] (float 0)))]
    (-> (fn [_] (nd/ndarray :float32 zeros [width height]))
        (map (range 9))
        array)))

(def lattice (make-lattice 10 10))

;; denoted as c
(def lattice-speed 0.1)

(defn get-flow-density
  [lattice [x y] direction]
  (nd/get-at (aget lattice direction) x y))

(defn set-flow-density
  [lattice [x y] direction value]
  (nd/set-at (aget lattice direction)
             x y
             value))

(defn flow-forces [lattice position]
  (for [direction (range 9)]
    (get-flow-density lattice position direction)))

(defn density [lattice position]
  (reduce + (flow-forces lattice position)))

(defn velocity [lattice position density]
  (let [c lattice-speed]
    (/ (* c (reduce + (map g/scale (flow-forces lattice position) directions)))
       density)))

(defn weights [direction]
  (case direction
    0 (/ 4 9)
    (1 2 3 4) (/ 1 9)
    (5 6 7 8) (/ 1 36)))

(defn distribution
  "Denoted as
  s_i(u) = w_i [3*(e_i * u) / c + 9/2*(e_i * u)^2 / c^2 - 3/2 (u * u) / c^2 ]
  "
  [velocity direction]
  (let [c lattice-speed
        c2 (* c c)
        eiu (tm/dot (directions direction) velocity)]
    (* (weights direction)
       (+
        (/ (* 3 eiu) c)
        (/ (* (/ 9 2) (* eiu eiu)) c2)
        (- (/ (* (/ 3 2) (tm/dot velocity velocity)) c2))))))

#_:clj-kondo/ignore
(defn equilibrium-distribution
  "Denoted as f_i^eq(x,t) = w_ip + p*s_i(u(x,t))"
  [lattice position direction density velocity]
  (+ (* (weights direction) density)
     (* density (distribution velocity direction))))

;; Borrowing from
;; https://physics.weber.edu/schroeder/fluids/LatticeBoltzmannDemo.java.txt,
;; from https://physics.weber.edu/schroeder/fluids/
(defn stream [lattice]
  (let [pE (aget lattice 1)
        pN (aget lattice 2)
        pW (aget lattice 3)
        pS (aget lattice 4)
        pNE (aget lattice 5)
        pNW (aget lattice 6)
        pSW (aget lattice 7)
        pSE (aget lattice 8)
        [xdim-1 ydim-1] (map dec (nd/shape pN))]
    (loop/c-for [x 0 (< x xdim-1) (inc x)
                 y ydim-1 (> y 0) (dec y)]
      (nd/set-at pN  x y (nd/get-at pN  x (dec y)))
      (nd/set-at pNW x y (nd/get-at pNW (inc x) (dec y))))
    (loop/c-for [x xdim-1 (> x 0) (dec x)
                 y ydim-1 (> y 0) (dec y)]
      (nd/set-at pE  x y (nd/get-at pE  (dec x) y))
      (nd/set-at pNE x y (nd/get-at pNE (dec x) (dec y))))
    (loop/c-for [x xdim-1 (> x 0) (dec x)
                 y 0 (< y ydim-1) (inc y)]
      (nd/set-at pS  x y (nd/get-at pS  x (inc y)))
      (nd/set-at pSE x y (nd/get-at pSE (dec x) (inc y))))
    (loop/c-for [x 0 (< x xdim-1) (inc x)
                 y 0 (< y ydim-1) (inc y)]
      (nd/set-at pW  x y (nd/get-at pW  (inc x) y))
      (nd/set-at pSW x y (nd/get-at pSW (inc x) (inc y))))))

(defn setup []
  (q/background "black")
  {})

(defn update-state [state]
  state)

(defn draw [_]
  )

(defn page []
  (sketch/component
   :size [100 100]
   :setup setup
   :update update-state
   :draw draw
   :middleware [m/fun-mode framerate/mode]))

(sketch/definition fluid
  {:created-at "2020-10-29"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
