(ns shimmers.sketches.cilia-phase
  (:require
   [clojure.math :as math]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg :include-macros true]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.ui.debug :as debug]
   [shimmers.common.ui.svg :as usvg]
   [shimmers.math.core :as sm]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; Concept: generate a random phase-shifted function, and then add cilia along
;; it's path. Specifically interested in trying to make another phase shifted
;; function and slowly modulate that over each cilia from left to right.

(def width 900)
(def height 600)
(defn rv [x y]
  (gv/vec2 (* width x) (* height y)))

(defn gen-rate []
  (dr/weighted {0.25 0.5 0.5 1.0
                1.0 1.0 1.5 1.0 2.0 1.0 2.5 1.0
                3.0 1.0 4.0 0.5 5.0 0.25}))

(defn gen-amp []
  (dr/weighted {0.05 1.0
                0.10 1.0 0.15 2.0
                0.20 2.0 0.25 2.0
                0.30 1.0 0.35 1.0
                0.40 1.0
                0.50 1.0}))

(defn gen-spline-fx [amplitude]
  (let [r (gen-rate)
        dr (gen-rate)
        phase-amp (gen-amp)
        c (dr/random)
        dc (dr/random)]
    {:params {:r r :dr dr :amp amplitude :phase-amp phase-amp :c c :dc dc}
     :fn
     (fn spline-fx
       ([x] (spline-fx 0 x))
       ([phase x]
        (let [phase-mod (* phase-amp (math/cos (* eq/TAU (+ (* dr x) dc phase))))]
          (* amplitude (math/sin (* eq/TAU (+ (* r x) c phase-mod)))))))}))

(defn screen-space [y amp fx x phase]
  (rv x (+ y (* amp (fx (+ x phase))))))

(defn base-spline [screen-space fx phase]
  (for [x (range 0 1 0.0025)]
    (screen-space fx x phase)))

(defn cilia-line [_ pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)]
    (gl/line2 (tm/+ pt offset) (tm/- pt offset))))

(defn cilia-line-plot [spline cx pt angle len]
  (let [offset (g/rotate (gv/vec2 len 0) angle)
        axis (g/rotate (gv/vec2 (* 0.066 len) 0) (+ angle (* eq/TAU 0.25)))
        a (tm/+ pt offset)
        b (tm/- pt offset)
        pts (for [x (range 0 1 0.025)]
              (let [fy (spline cx x)]
                (tm/+ (tm/mix a b x)
                      (tm/mix (tm/- axis) axis fy))))]
    (when (every? v/valid? pts)
      (csvg/path (csvg/segmented-path pts)))))

;; How to avoid intersecting cilia?
(defn cilias
  [{:keys [samples screen-space cilia-spline line-fx
           length-fx cilia-amp height-prop rot-fx phase]}]
  (for [x samples]
    (let [pt (screen-space (:fn line-fx) x phase)
          ;; what happens if the lookahead distance for the derivitive is oscillating?
          pt' (screen-space (:fn line-fx) (+ x 0.0001) phase)
          angle (+ (g/heading (tm/- pt' pt)) (* eq/TAU 0.25) ((:fn rot-fx) x))
          len (* height-prop (+ cilia-amp (* cilia-amp ((:fn length-fx) x))))]
      (cilia-spline (+ x phase) pt angle len))))

(defn gen-density []
  (let [mode (dr/weighted {:equal 1.33
                           :gaussian 1.33
                           :inv-smooth 1.33
                           :flat-smooth 1.33
                           :random 0.5
                           :random-normal 0.66
                           :stripes 1.33
                           :sin-stripes 1.0
                           :x-sin-stripes 1.0
                           :stair-sigmoid 2.0
                           :minkowski 1.33})
        proportion
        (dr/weighted {6 1.0
                      5.0 1.0
                      4.50 5.0
                      4.0 3.0
                      3.5 4.0
                      3 2.0
                      2.5 1.0
                      2.25 0.65
                      2.0 0.30
                      1.75 0.20
                      1.5 0.15})]
    (merge
     {:mode mode
      :density
      (math/floor (/ (max width height) proportion))}
     (case mode
       :stripes
       (let[primes (sm/primes-between 16 256)
            threshold (fn [x] (< x 64))
            mul-prime (dr/rand-nth (remove threshold primes))
            div-prime (dr/rand-nth (filter threshold primes))]
         {:mul-prime mul-prime
          :div-prime div-prime})
       :sin-stripes
       {:freq (dr/random 0.66 1.33)}
       :x-sin-stripes
       {:freq (dr/random 0.25 16)
        :amp (dr/random 0.01 0.1)}
       :stair-sigmoid
       {:freq (dr/random-int 2 32)
        :alpha (dr/random 1.0 3.0)
        :rate (* 2.5 (dr/happensity 0.4))
        :amp (dr/weighted {0.05 1.0 0.075 1.0 0.025 1.0 0.1 0.5})}
       :minkowski
       {:amp (dr/random 0.001 0.1)
        :rate (dr/random 1.0 16.0)}
       {}))))

(defn samples-from-density [{:keys [mode density] :as pts}]
  (case mode
    :equal (range -0.05 1.05 (/ 1.0 density))
    :gaussian
    (dr/gaussian-range (/ 1.0 density) (/ 1.0 (eq/sqr density))
                       true [-0.05 1.05])
    :inv-smooth
    (for [x (range -0.05 1.05 (/ 1.0 density))]
      (eq/inv-smoothstep x))
    :flat-smooth
    (for [x (range -0.05 1.05 (/ 1.0 density))]
      (eq/flat-smooth x))
    :random
    (repeatedly (* 1.1 density) #(dr/random))
    :random-normal
    (repeatedly density
                (dr/sample-between #(dr/gaussian 0.5 0.15) 0 1))
    :stripes
    (let [{:keys [mul-prime div-prime]} pts]
      (for [x (range -0.05 1.05 (/ 1.0 density))]
        (/ (mod (* x mul-prime) div-prime) div-prime)))
    :sin-stripes
    (let [{:keys [freq]} pts]
      (for [x (range -0.05 1.05 (/ 1.0 density))]
        (eq/unit-sin (* eq/TAU freq x))))
    :x-sin-stripes
    (let [{:keys [freq amp]} pts]
      (for [x (range -0.05 1.05 (/ 1.0 density))]
        (+ x (* amp (eq/sin-tau (* freq x))))))
    :stair-sigmoid
    (let [{:keys [freq alpha rate amp]} pts]
      (for [x (range -0.05 1.05 (/ 1.0 density))]
        (eq/stair-sigmoid (/ 1.0 freq) freq 0 alpha
                          (+ x (* amp (eq/sin-tau (* rate x)))))))
    :minkowski
    (let [{:keys [amp rate]} pts]
      (for [x (range -0.05 1.05 (/ 1.0 (* 1.2 density)))]
        (+ (eq/minkowski? x)
           (* amp (eq/sin-tau (+ (* rate x)))))))))

(defn line-parameters []
  (let [n (dr/weighted {(dr/random-int 3 8) 6.0
                        (dr/random-int 2 14) 2.0
                        (dr/random-int 2 20) 1.0})
        s (dr/random 1.0)
        amp (tm/clamp (dr/gaussian 0.35 0.05) 0.075 0.6)]
    {:line-params
     (into []
           (for [y (cs/midsection (tm/norm-range n))]
             {:ry y
              :amp
              (* (/ 1.0 (inc n))
                 (+ 0.025 (eq/gaussian s 0.5 -0.4 y)))
              :cilia-amp
              (* (/ 1.0 (inc n))
                 (+ 0.025 (eq/gaussian amp 0.5 -0.125 y)))
              :pts (gen-density)
              :phase
              (dr/gaussian 0.0 0.0125)}))}))

(defonce defo (debug/state))

(defn shapes [{:keys [params]}]
  (let [;; defines the path of the line
        line-fx (gen-spline-fx 1.0)
        ;; amplitude length of cilia out from line-fx
        length-fx (gen-spline-fx 0.75)
        ;; rotation around line-fx
        rot-fx (gen-spline-fx (* 0.125 math/PI))
        ;; cilia are either lines perpindicular to line-fx or the function
        ;; cilia-fx which is the perpindicular displacement from the line of
        ;; each cilia.
        cilia-fx (gen-spline-fx 1.0)
        cilia-spline
        (dr/weighted {cilia-line 1.0
                      (partial cilia-line-plot (:fn cilia-fx)) 2.5})]
    (reset! defo {:cilia (:params cilia-fx)
                  :path (:params line-fx)
                  :length (:params length-fx)
                  :rotation (:params rot-fx)})
    (mapcat
     (fn [{:keys [ry amp cilia-amp phase pts]}]
       (let [screen (partial screen-space ry amp)
             spline-pts (base-spline screen (:fn line-fx) phase)
             samples (samples-from-density pts)
             cilia
             (cilias {:samples samples
                      :screen-space screen
                      :cilia-spline cilia-spline
                      :line-fx line-fx
                      :length-fx length-fx
                      :cilia-amp cilia-amp
                      :height-prop height ;; FIXME: combine with cilia-amp
                      :rot-fx rot-fx
                      :phase phase})]
         [(csvg/path (csvg/segmented-path spline-pts))
          (csvg/group {:stroke-width 0.75} cilia)]))
     (:line-params params))))

(defn scene [{:keys [scene-id] :as args}]
  (csvg/svg-timed
    {:id scene-id
     :width width
     :height height
     :stroke "black"
     :fill "none"
     :stroke-width 1.0}
    (shapes args)))

(defn explanation [{{:keys [line-params]} :params}]
  (debug/pre-edn
   (merge {:line-params line-params} @defo)
   {:width 120
    :print-fixed-width 4}))

(sketch/definition cilia-phase
  {:created-at "2024-10-24"
   :tags #{}
   :type :svg}
  (ctrl/mount
   (usvg/let-page (assoc sketch-args :explanation-div [:div.evencols])
                  line-parameters
                  explanation
                  scene)))
