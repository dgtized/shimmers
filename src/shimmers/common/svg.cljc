(ns shimmers.common.svg
  (:require
   [clojure.string :as str]
   [shimmers.common.string :as scs]
   [shimmers.math.vector :as v]
   [thi.ng.color.core :as col]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

(defn screen [width height]
  (rect/rect 0 0 width height))

(defn svg-elem
  "Replaces svg/svg, and removes warnings about xlink & react keys"
  [attribs]
  [:svg
   (svg/svg-attribs
    attribs
    {:xmlns "http://www.w3.org/2000/svg"})])

;; still not quite right, (svg {} [el el ...]) works, but
;; and (svg {} [el el] [el]) work,
;; but (svg {} el) and (svg {} el el) do not.
(defn svg
  {:style/indent [:defn]}
  [attribs & body]
  (-> (svg-elem attribs)
      (into (apply concat body))
      adapt/all-as-svg
      adapt/inject-element-attribs))

(defn group
  {:style/indent [:defn]}
  [attribs & body]
  (into [:g (svg/svg-attribs attribs nil)] body))

(defn rotate
  ([angle]
   (scs/cl-format "rotate(~0,3f)" (tm/degrees angle)))
  ([angle [x y]]
   (scs/cl-format "rotate(~0,3f,~0,3f,~0,3f)"
                  (tm/degrees angle) x y)))

(defn transform [& operations]
  (str/join " " operations))

(defn translate
  ([[x y]] (translate x y))
  ([x y] (scs/cl-format "translate(~0,3f,~0,3f)" x y)))

(defn matrix [a b c d e f]
  (scs/cl-format "matrix(~0,3f,~0,3f,~0,3f,~0,3f,~0,3f,~0,3f)"
                 a b c d e f))

(defn scale [scale-x scale-y]
  (scs/cl-format "scale(~0,3f,~0,3f)" scale-x scale-y))

(defn skew-x [angle]
  (scs/cl-format "skewX(~0,3f)" (tm/degrees angle)))

(defn skew-y [angle]
  (scs/cl-format "skewY(~0,3f)" (tm/degrees angle)))

(comment (transform (translate 0.5 1.2) (skew-y 2)))

;; Comment is cribbed from https://github.com/thi-ng/geom/pull/79/files
(defn path
  "Extend `svg/path` to include all segment types.

  Uppercase are absolute coordinates, lowercase are relative coordinates.

  | Command | Name                     | Arguments                                             |
  |---------+--------------------------+-------------------------------------------------------|
  | M/m     | moveto                   | [pt]                                                  |
  | L/l     | lineto                   | [pt]                                                  |
  | H/h     | horizontal lineto        | [x]                                                   |
  | V/v     | vertical lineto          | [y]                                                   |
  | C/c     | cubic curveto            | [pt1 pt2 endpt]                                       |
  | S/s     | smooth cubic curveto     | [pt2 endpt]                                           |
  | Q/q     | quadratic curveto        | [pt1 endpt]                                           |
  | T/t     | smooth quadratic curveto | [endpt]                                               |
  | A/a     | elliptical arc           | [rx ry x-axis-rotation large-arc-flag sweep-flag x y] |
  | Z/z     | closepath                | []                                                    |
  "
  ([segments] (path segments nil))
  ([segments attribs]
   (with-redefs [thi.ng.geom.svg.core/path-segment-formats
                 (assoc svg/path-segment-formats
                        :Q ["Q" svg/*fmt-vec* " " svg/*fmt-vec* " "]
                        :q ["q" svg/*fmt-vec* " " svg/*fmt-vec* " "]
                        :S ["S" svg/*fmt-vec* " " svg/*fmt-vec* " "]
                        :s ["s" svg/*fmt-vec* " " svg/*fmt-vec* " "]
                        :T ["T" svg/*fmt-vec* " "]
                        :t ["t" svg/*fmt-vec* " "])]
     (svg/path segments attribs))))

(defn hsl
  ([h s l] (hsl h s l 1.0))
  ([h s l a]
   (col/as-css (col/hsla h s l a))))

(comment (hsl 0.5 0.8 0.4 0.6))

;; svg tricks to revisit:
;; https://www.smashingmagazine.com/2019/03/svg-circle-decomposition-paths/
;; https://www.smashingmagazine.com/2022/05/magical-svg-techniques/
;; https://zverok.space/blog/2021-12-28-grok-shan-shui.html

(defn arc-segment [t0 t1 r0 r1 attribs]
  (let [lower (v/polar r0 t0)
        upper (v/polar r1 t1)]
    (svg/path [[:M lower]
               [:L (v/polar r1 t0)]
               [:A [r1 r1] 0.0 0 1 upper]
               [:L (v/polar r0 t1)]
               [:A [r0 r0] 0.0 0 0 lower]
               [:Z]]
              attribs)))

(comment
  (f/format (:A svg/path-segment-formats) (gv/vec2 0.5 0.1) 1.0 1.0 1.0 (gv/vec2 1.0 0.5))
  (f/format [(f/float 2)] 0.21)
  (arc-segment 0.5 1 1 2 {}))
