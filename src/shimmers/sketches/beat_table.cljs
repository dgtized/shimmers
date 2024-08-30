(ns shimmers.sketches.beat-table
  (:require
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.sketch :as sketch :include-macros true]))

;; search for time change locations on base 64 or 48
(comment (filter (fn [x] (or (= 0 (mod x 64))
                            (= 0 (mod x 48))))  (range 2049)))

(defn scene [_]
  (let [divisions [1 4 6 8 12 16 24 32]]
    [:table
     (into [:tbody]
           (for [division divisions]
             (into [:tr]
                   (cons [:th (if (= division 1)
                                "beat"
                                (str "1/" division))]
                         (for [beat (range 65)
                               :when (some (fn [div] (= 0 (mod beat div)))
                                           [4 6 8 12 16 24 32])]
                           [:td {:style {:width "1.8em" :text-align "right"}}
                            (if (= division 1) beat
                                (str (mod beat division)))])))))]))

(sketch/definition beat-table
  {:created-at "2024-08-30"
   :tags #{}
   :type :svg}
  (ctrl/mount (usvg/page sketch-args scene)))
