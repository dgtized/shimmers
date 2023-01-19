(ns shimmers.sketches.barcodes
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as tm]
   [thi.ng.geom.vector :as gv]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.common.quil :as cq]
   [thi.ng.geom.rect :as rect]
   [shimmers.common.quil-draws-geom :as qdg]))

(defn code [y w h]
  (rect/rect (q/width) y w h))

(defn update-barcode [codes new-code speed chance dt]
  (let [codes' (mapv (fn [s] (g/translate s (tm/* speed dt))) codes)]
    (->> (if (and (< (+ (if-let [c (peek codes')] (rect/right c) 0) 0.1)
                     (q/width))
                  (dr/chance chance))
           (conj codes' (new-code))
           codes')
         (filter (fn [s] (> (rect/right s) 0))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :barcodes [[] [] []]})

(defn update-state [{:keys [t] :as state}]
  (let [dt 0.15
        n (count (:barcodes state))
        height (/ 1.0 (inc n))
        chaos (tm/smoothstep* -0.2 0.6 (Math/sin (* t 0.01)))
        new-code (fn [i]
                   (partial code
                            (cq/rel-h (+ (+ (* 0.5 height) (* i height))
                                         (* chaos (* 0.01 (dr/random)))))
                            (cq/rel-w (Math/abs (dr/gaussian 0.005 0.002)))
                            (cq/rel-h (* (if (dr/chance 0.08) 0.7 0.8) height))))
        differential (+ 1.66 (* 0.3 chaos))]
    (-> state
        (update :t + dt)
        (update :barcodes
                (partial map-indexed
                         (fn [i codes]
                           (update-barcode codes
                                           (new-code i)
                                           (gv/vec2 (- (Math/pow differential (+ i 2))) 0.0)
                                           (* 0.05 (Math/pow differential i))
                                           dt)))))))

(defn draw [{:keys [barcodes]}]
  (q/background 1.0)
  (q/fill 0.0)
  (q/no-stroke)
  (doseq [codes barcodes
          r codes]
    (qdg/draw r)))

(sketch/defquil barcodes
  :created-at "2023-01-19"
  :tags #{}
  :size [800 600]
  :setup setup
  :update update-state
  :draw draw
  :middleware [m/fun-mode framerate/mode])
