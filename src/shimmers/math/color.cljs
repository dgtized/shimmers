(ns shimmers.math.color
  "Useful tools for generating or working with colors

  Gradients borrowed from https://github.com/thi-ng/color/blob/master/src/gradients.org"
  (:require
   [quil.core :as q :include-macros true]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [thi.ng.color.core :as col]
   [thi.ng.color.gradients :as grad]
   [thi.ng.dstruct.streams :as streams]
   [thi.ng.math.core :as tm]))

;; TODO: https://hugodaniel.com/posts/minimal-color-swatches/

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

(defn mixer
  "Mix two color vectors with some random displacement by `t`"
  [[c1 & v1] [c2 & v2] t]
  (into [(-> (sm/mix-mod c1 c2 t)
             (+ (* (dr/gaussian 0 1) 0.05))
             (mod 1.0))]
        (mapv (fn [a b]
                (-> (tm/mix* a b t)
                    (+ (* (dr/gaussian 0 1) 0.05))
                    (tm/clamp 0 1)))
              v1 v2)))

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

(defn hex-palette->hsla
  [palette]
  (mapv (comp vec hex->hsla) palette))

(defn cube [n] (* n n n))

;; https://observablehq.com/@fil/oklab-color-space
(defn gamma [x]
  (if (>= x 0.0031308)
    (- (* 1.055 (Math/pow x (/ 1.0 2.4))) 0.055)
    (* 12.92 x)))

;; https://bottosson.github.io/posts/oklab/
(defn oklab->rgb [L a b]
  (let [l (cube (+ L (* 0.3963377774 a) (* 0.2158037573 b)))
        m (cube (- L (* 0.1055613458 a) (* 0.0638541728 b)))
        s (cube (- L (* 0.0894841775 a) (* 1.2914855480 b)))]
    [(gamma (+ (*  4.0767416621 l) (* -3.3077115913 m) (*  0.2309699292 s)))
     (gamma (+ (* -1.2684380046 l) (*  2.6097574011 m) (* -0.3413193965 s)))
     (gamma (+ (* -0.0041960863 l) (* -0.7034186147 m) (*  1.7076147010 s)))]))

(defn oklab-lch [L C h]
  (oklab->rgb L (* C (Math/cos h)) (* C (Math/sin h))))

(comment (oklab->rgb 0 0.1 0.1)
         (oklab->rgb 0.1 0.1 0.1)
         (oklab-lch 1.0 0.5 0.5))
