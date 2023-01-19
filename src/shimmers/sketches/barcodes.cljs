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
    (->> (if (and (< (+ (if-let [c (peek codes')] (rect/right c) 0) 1.0)
                     (q/width))
                  (dr/chance chance))
           (conj codes' (new-code))
           codes')
         (filter (fn [s] (> (rect/right s) 0))))))

(defn setup []
  (q/color-mode :hsl 1.0)
  {:t 0
   :barcodes [[] [] []]})

(defn update-state [state]
  (let [dt 0.1
        n (count (:barcodes state))
        height (/ 1.0 (inc n))
        new-code (fn [i]
                   (partial code
                            (cq/rel-h (+ (* 0.5 height) (* i height)))
                            (cq/rel-w (dr/random 0.001 0.01))
                            (cq/rel-h (* 0.8 height))))]
    (-> state
        (update :t + dt)
        (update :barcodes
                (partial map-indexed
                         (fn [i codes]
                           (update-barcode codes
                                           (new-code i)
                                           (gv/vec2 (- (Math/pow 1.5 (+ i 2))) 0.0)
                                           (* 0.03 (Math/pow 1.5 i))
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
