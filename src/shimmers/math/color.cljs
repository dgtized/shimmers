(ns shimmers.math.color
  "Useful tools for generating or working with colors

  Gradients borrowed from https://github.com/thi-ng/color/blob/master/src/gradients.org"
  (:require [clojure.string :as str]
            [quil.core :as q :include-macros true]
            [thi.ng.color.core :as col]
            [thi.ng.color.gradients :as grad]
            [thi.ng.dstruct.streams :as streams]))

(defn random []
  (let [colors [[128 192 128 32]
                [128 0 0 32]
                [0 128 0 32]
                [0 0 128 32]
                [0 0 192 32]]]
    (rand-nth colors)))

(defn random-lerp [from to]
  (let [color (q/lerp-color from to (rand))]
    [(q/red color) (q/green color) (q/blue color) 32]))

(defn as-vector [color]
  (map (fn [x] (float (* x 255)))
       [(col/red color) (col/green color) (col/blue color) (col/alpha color)]))

;; FIXME: slow, can we cache the gradient instead of rebuilding for each particle?
(defn random-gradient [scheme]
  (let [color (->> scheme
                   grad/cosine-schemes
                   (grad/cosine-gradient 32)
                   rand-nth)]
    (as-vector (col/adjust-alpha color -0.95))))

(defn hex->hsla
  ([hex] (hex->hsla hex 0))
  ([hex theta]
   (-> hex
       col/hex->int
       col/as-hsla
       (col/rotate-hue theta)
       streams/get-float-buffer)))

(defn url->colors [url]
  (-> url
      (str/split #"/")
      last
      (str/split #"-")))

(defn url->hex-colors
  "Converts a palette url into a hex color string like `#abcdef`."
  [url]
  (mapv (partial str "#") (url->colors url)))

(defn url->palette
  "Converts a palette url from like
  https://artsexperiments.withgoogle.com/artpalette/colors/7f2e14-5d503f-e4c111-806d4e
  to hsla color vectors."
  [url]
  (->> url
       url->colors
       (map hex->hsla)))
