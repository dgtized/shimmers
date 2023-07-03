(ns shimmers.sketches.radial-breaks
  (:require
   [shimmers.common.palette :as palette]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.sketches.radial-mosaic :as radial-mosaic]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.vector :as gv]))

(def width 800)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn minimum-spacing [gap offsets]
  (->> offsets
       (partition 2 1)
       (filter (fn [[a b]] (> (- b a) gap)))
       (map second)
       (into [0])))

(defn transforms [jitter]
  (let [jitter (max 0 jitter)]
    (csvg/transform
     (csvg/translate (dr/jitter (if (< jitter 1.0)
                                  0.0
                                  jitter)))
     (csvg/rotate (if (dr/chance 0.05)
                    (dr/gaussian 0.0 0.025)
                    0.0)))))

(defn segment [t0 t1 r0 r1 attribs]
  (if (and (= "none" (:fill attribs))
           (dr/chance 0.08))
    (csvg/group {}
      (repeatedly (dr/random-int 2 4)
                  (fn []
                    (let [transformed
                          (assoc attribs
                                 :transform (transforms (dr/gaussian 1.2 0.33))
                                 :stroke-width (dr/gaussian 1.0 0.02))]
                      (csvg/arc-segment t0 t1 r0 r1 transformed)))))
    (csvg/arc-segment t0 t1 r0 r1 attribs)))

;; FIXME how to gather together slices and not just the breaks outward from center
(defn shapes [palette]
  (let [radius (* 0.45 height)
        radial (->> (dr/gaussian-range 0.02 0.065 true)
                    (map (partial * eq/TAU))
                    (minimum-spacing 0.025))
        breaks (->> (dr/gaussian-range 0.01 0.03 true)
                    (minimum-spacing 0.02)
                    (map (partial * radius))
                    rest)]
    (for [[t0 t1] (partition 2 1 radial)
          [r0 r1] (partition 2 1 (dr/random-sample 0.4 breaks))]
      (->> {:fill (dr/rand-nth palette)
            :stroke-width (if (dr/chance 0.15) 2.5 1.0)
            :transform (transforms (dr/gaussian 1.5 0.5))}
           (segment t0 t1 r0 r1)))))

(defn scene [palette]
  (csvg/svg-timed {:width width
                   :height height
                   :stroke "black"
                   :fill "none"
                   :stroke-width 1.0}
    (csvg/group {:transform (csvg/translate (rv 0.5 0.5))}
      (shapes (into (repeat 10 "none") palette)))))

(defn page []
  (let [palette (:colors (dr/rand-nth radial-mosaic/palettes))]
    (fn []
      [:<>
       [:div.canvas-frame [scene palette]]
       [:div.contained
        [:div.flexcols {:style {:justify-content :space-evenly :align-items :center}}
         [view-sketch/generate :radial-breaks]
         [palette/as-svg {} palette]]]])))

(sketch/definition radial-breaks
  {:created-at "2023-05-08"
   :type :svg
   :tags #{}}
  (ctrl/mount (page)))
