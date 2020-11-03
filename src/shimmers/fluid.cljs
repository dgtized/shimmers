(ns shimmers.fluid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]
            [shimmers.macros.loop :as loop :include-macros true]
            [thi.ng.ndarray.core :as nd]))

;; From https://en.wikipedia.org/wiki/Lattice_Boltzmann_methods &
;; https://www.math.nyu.edu/~billbao/report930.pdf
(def directions
  [(v/vec2 0 0) ;; 0 staying
   (v/vec2 1 0) ;; 1 east
   (v/vec2 0 1) ;; 2 north
   (v/vec2 -1 0) ;; 3 west
   (v/vec2 0 -1) ;; 4 south
   (v/vec2 1 1) ;; 5 north-east
   (v/vec2 -1 1) ;; 6 north-west
   (v/vec2 -1 -1) ;; 7 south-west
   (v/vec2 1 -1) ;; 8 south-east
   ])

(defn make-lattice [width height]
  (let [zeros (repeatedly (* width height) (fn [] (float 0)))]
    (-> #(nd/ndarray :float32 zeros [width height])
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
    (/ (* c (reduce + (map v/scale (flow-forces lattice position) directions)))
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
        eiu (v/dot (directions direction) velocity)]
    (* (weights direction)
       (+
        (/ (* 3 eiu) c)
        (/ (* (/ 9 2) (* eiu eiu)) c2)
        (- (/ (* (/ 3 2) (v/dot velocity velocity)) c2))))))

(defn equilibrium-distribution
  "Denoted as f_i^eq(x,t) = w_ip + p*s_i(u(x,t))"
  [lattice position direction density velocity]
  (+ (* (weights direction) density)
     (* density (distribution velocity direction))))

;; Borrowing from
;; https://physics.weber.edu/schroeder/fluids/LatticeBoltzmannDemo.java.txt,
;; from https://physics.weber.edu/schroeder/fluids/
(defn stream [lattice]
  (let [pN (aget lattice 1)
        pE (aget lattice 2)
        pNE (aget lattice 5)
        pNW (aget lattice 6)
        [xdim ydim] (nd/shape pN)]
    (loop/c-for [x 0 (< x xdim) (inc x)]
                (loop/c-for [y (dec ydim) (> y 0) (dec y)]
                            (nd/set-at pN  x y (nd/get-at pN  x (dec y)))
                            (nd/set-at pNW x y (nd/get-at pNW (inc x) (dec y)))))
    (loop/c-for [x (dec xdim) (> x 0) (dec x)]
                (loop/c-for [y (dec ydim) (> y 0) (dec y)]
                            (nd/set-at pE  x y (nd/get-at pE  (dec x) y))
                            (nd/set-at pNE x y (nd/get-at pNE (dec x) (dec y)))))))

(comment (loop/downto [y (dec 5) 0] (println y))
         (loop/upto [x 0 4] (println x))
         (loop/c-for [x 0 (< x 4) (inc x)] (println x))
         (macroexpand-1 '(loop/c-for [x 2 (>= x 0) (dec x)] (println x)))
         (macroexpand '(loop/c-for [x 0 (< x 3) (inc x)
                                    y 1 (< y 3) (inc y)]
                                   (+ x y)
                                   (println [x y])))
         (loop/c-for [x 0 (< x 3) (inc x)
                      y 1 (< y 3) (inc y)]
                     (+ x y)
                     (println [x y])))

(defn setup []
  (q/background "black")
  {})

(defn update-state [state]
  state)

(defn draw [state]
  (framerate/display (q/current-frame-rate)))

(defn ^:export run-sketch []
  (q/defsketch fluid
    :host "quil-host"
    :size [100 100]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
