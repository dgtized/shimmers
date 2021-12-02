(ns shimmers.sketches.morse-patterns
  (:require
   [shimmers.algorithm.line-clipping :as clip]
   [shimmers.common.svg :as csvg]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.sketch :as sketch :include-macros true]
   [shimmers.view.sketch :as view-sketch]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [thi.ng.geom.rect :as rect]
   [thi.ng.math.core :as tm]))

(def width 800)
(def height 600)

(defn perturb [pattern]
  (let [sorted (sort pattern)
        lower (take 3 sorted)
        upper (take-last 3 sorted)]
    (dr/shuffle (concat lower
                        (map (partial * tm/PHI) lower)
                        (dr/random-sample 0.8 sorted)
                        (map (partial * (/ 1 tm/PHI)) upper)
                        upper))))

(defn poly-divide [gap-cycle len-cycle line]
  (let [{[p q] :points width :width} line
        dist (g/dist p q)
        perp (g/rotate (tm/normalize (tm/- p q) (/ width 2)) (* 0.25 eq/TAU))
        u0 (tm/+ p perp)
        l0 (tm/- p perp)
        upper (tm/normalize (tm/- (tm/+ q perp) u0) 1.0)
        lower (tm/normalize (tm/- (tm/- q perp) l0))]
    (loop [polys [] a 0 b 20]
      (if (> b dist)
        polys
        (let [gap (gap-cycle)
              len (len-cycle)]
          (recur (conj polys (gp/polygon2 (tm/+ u0 (tm/* upper a))
                                          (tm/+ u0 (tm/* upper b))
                                          (tm/+ l0 (tm/* lower b))
                                          (tm/+ l0 (tm/* lower a))))
                 (+ b gap)
                 (+ b gap len)))))))

(defn shapes []
  (let [margin 24
        bounds (rect/rect (- margin) (- margin) (+ width (* 2 margin)) (+ height (* 2 margin)))
        gaps (repeatedly 11 (fn [] ((dr/weighted {#(dr/random-int 4 10) 4.0
                                                 #(dr/random-int 14 30) 1.0}))))
        lengths (perturb (repeatedly 19 (fn [] ((dr/weighted {#(dr/random-int 20 40) 3.0
                                                             #(dr/random-int 40 60) 1.0})))))
        widths (perturb (repeatedly 19 (fn [] ((dr/weighted {#(dr/random-int 3 6) 1.0
                                                            #(dr/random-int 6 12) 4.0
                                                            #(dr/random-int 12 18) 2.0})))))
        w0 (int (/ (apply max widths) 1.5))
        spacings (repeatedly 11 #(dr/random-int w0 (int (* 1.8 w0))))]
    ;; TODO get rid of 0,120 constants?
    (->> (clip/variable-hatching bounds (dr/random 0.1 0.7) 0 120 (dr/cyclic spacings)
                                 (dr/cyclic widths))
         (mapcat (partial poly-divide (dr/cyclic gaps) (dr/cyclic lengths)))
         (map-indexed (fn [i poly]
                        (cond (zero? (mod i 13))
                              (g/scale-size poly (/ 1 tm/PHI))
                              (zero? (mod i 19))
                              (g/scale-size poly 0.5)
                              :else poly))))))

;; FIXME: handle large gaps and overlapping lines
(defn scene []
  (csvg/svg {:width width
             :height height
             :stroke "black"
             :fill "white"
             :stroke-width 0.8}
            (for [[i shape] (map-indexed vector (shapes))]
              (vary-meta shape assoc :key (str "l" i)
                         :stroke-width (:width shape)))))

(sketch/definition morse-patterns
  {:created-at "2021-12-02"
   :type :svg
   :tags #{:deterministic}}
  (ctrl/mount (view-sketch/page-for scene :morse-patterns)
              "sketch-host"))
