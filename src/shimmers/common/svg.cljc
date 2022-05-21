(ns shimmers.common.svg
  (:require
   [clojure.string :as str]
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.math.core :as tm]
   [thi.ng.strf.core :as f]))

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
  [attribs & body]
  (-> (svg-elem attribs)
      (into (apply concat body))
      adapt/all-as-svg
      adapt/inject-element-attribs))

(defn rotate [heading [x y]]
  (as-> [(tm/degrees heading) x y] o
    (str/join "," o)
    (str "rotate(" o ")")))

(defn translate [[x y]]
  (f/format ["translate(" (f/float 2) "," (f/float 2) ")"] x y))

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
