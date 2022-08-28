(ns shimmers.algorithm.mosaic
  (:require [thi.ng.geom.vector :as gv]
            [thi.ng.geom.core :as g]))

(defn translate [cells pos]
  (map #(update % :pos g/translate pos) cells))

(defn max-height [cells]
  (inc (apply max (map #(get-in % [:pos 1]) cells))))

(defn max-width [cells]
  (inc (apply max (map #(get-in % [:pos 0]) cells))))

(defn flip-x [seed]
  (let [w (max-width seed)]
    (map (fn [cell]
           (update-in cell [:pos 0] (fn [x] (- w x 1))))
         seed)))

(defn flip-y [seed]
  (let [h (max-height seed)]
    (map (fn [cell]
           (update-in cell [:pos 1] (fn [y] (- h y 1))))
         seed)))

(defn mirror [dir seed]
  (case dir
    :left
    (concat (flip-x seed) (translate seed (gv/vec2 (max-width seed) 0)))
    :right
    (concat seed (translate (flip-x seed) (gv/vec2 (max-width seed) 0)))
    :up
    (concat (flip-y seed) (translate seed (gv/vec2 0 (max-height seed))))
    :down
    (concat seed (translate (flip-y seed) (gv/vec2 0 (max-height seed))))))

(defn mirror-group [a b]
  (fn [seed]
    (->> seed
         (mirror a)
         (mirror b))))
