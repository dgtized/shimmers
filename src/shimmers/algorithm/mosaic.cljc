(ns shimmers.algorithm.mosaic
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]))

(defn translate [cells pos]
  (map #(update % :pos g/translate pos) cells))

(defn max-height [cells]
  (inc (apply max (map #(get-in % [:pos 1]) cells))))

;; FIXME: fix for test crash in cljc unit tests
(defn max-width [cells]
  (inc (apply max (map #(get-in % [:pos 0]) cells))))

(defn column [cells col]
  (filterv (fn [cell] (= col (int (get-in cell [:pos 0])))) cells))

(defn rotate-r [cells]
  (mapcat (fn [col]
            (map-indexed
             (fn [j cell] (assoc cell :pos (gv/vec2 j col)))
             (reverse (column cells col))))
          (range (max-width cells))))

(defn rotate-l [cells]
  (let [height (max-height cells)]
    (mapcat (fn [row]
              (map-indexed
               (fn [i cell] (assoc cell :pos (gv/vec2 i (- height row 1))))
               (column cells row)))
            (reverse (range (max-width cells))))))

(defn clockwise [w h]
  [(gv/vec2 0 0) (gv/vec2 w 0) (gv/vec2 w h) (gv/vec2 0 h)])

(defn counter-clockwise [w h]
  [(gv/vec2 0 0) (gv/vec2 0 h) (gv/vec2 w h) (gv/vec2 w 0)])

(defn rotate-group-r [dir seed]
  (let [w (max-width seed)
        h (max-height seed)]
    (mapcat translate
            (iterate rotate-r seed)
            (dir w h))))

(defn rotate-group-l [dir seed]
  (let [w (max-width seed)
        h (max-height seed)]
    (mapcat translate
            (iterate rotate-l seed)
            (dir w h))))

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
