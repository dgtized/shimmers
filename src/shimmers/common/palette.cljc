(ns shimmers.common.palette
  (:require
   [clojure.string :as str]
   [shimmers.common.svg :as csvg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn url->colors [url]
  (-> url
      (str/split #"/")
      last
      (str/split #"-")))

(defn from-urls [urls]
  (->> urls
       (map url->colors)
       (map (partial map (partial str "#")))))

(defn as-svg
  [{:keys [width height class]
    :or {width 400 height 30
         class "palette"}}
   palette]
  (let [cell (/ width (count palette))
        rect (rect/rect 0 0 cell height)]
    (csvg/svg {:class class :width width :height height}
      (for [[idx color] (map-indexed vector palette)]
        (-> rect
            (g/translate (tm/* (gv/vec2 idx 0) (gv/vec2 cell 0)))
            (with-meta {:fill (str color)}))))))
