(ns shimmers.fluid
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [shimmers.framerate :as framerate]
            [shimmers.math.vector :as v]))

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
  (let [directions (make-array float 9 (* width height))]
    {:width width
     :height height
     :directions directions}))

(def lattice (make-lattice 10 10))

;; denoted as c
(def lattice-speed 0.1)

(defn idx [[x y] width]
  (+ (* y width) x))

(defn get-flow-density
  [{:keys [directions width]} position direction]
  (aget (aget directions direction) (idx position width)))

(defn set-flow-density
  [{:keys [directions width]} position direction value]
  (aset (aget directions direction)
        (idx position width)
        value))

(defn flow-forces [lattice position]
  (for [direction (range 0 9)]
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
