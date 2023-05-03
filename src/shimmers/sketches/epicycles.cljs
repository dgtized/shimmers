(ns shimmers.sketches.epicycles
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.algorithm.kinematic-chain :as chain]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defonce ui-state (ctrl/state {:n 3
                               :persistent true
                               :show-chain true
                               :odd-even true
                               :stretchy false}))

(defn link-update [n length t dt]
  (let [{:keys [odd-even stretchy]} @ui-state]
    (fn [i segment]
      (-> segment
          (update :angle +
                  (* (if odd-even (if (odd? i) -1 1) 1)
                     (/ (Math/pow (inc i) (inc (/ 3 n))) 10) dt))
          (assoc :length (+ length (* (if stretchy 0.33 0)
                                      (/ (- n i) n) length (Math/sin t))))))))

(defn rotate-chainlinks [chain base length dt t]
  (let [n (count (:segments chain))]
    (-> chain
        (update :segments (partial map-indexed (link-update n length t dt)))
        (chain/propagate base))))

(defn setup []
  (q/color-mode :hsl 1.0)
  (let [n (:n @ui-state)
        length (cq/rel-h (/ 0.45 n))]
    {:t 0
     :length length
     :chain (chain/->KinematicChain
             (for [i (range n)]
               (chain/->KinematicSegment (tm/+ (gv/vec2) (tm/* (gv/vec2 length 0) i))
                                         0
                                         length)))}))

(defn update-state [{:keys [length t] :as state}]
  (-> state
      (update :chain rotate-chainlinks (gv/vec2) length 0.02 t)
      (update :t + (abs (dr/gaussian 0 0.02)))))

(defn draw [{:keys [chain]}]
  (q/no-fill)
  (q/with-translation (cq/rel-vec 0.5 0.5)
    (let [{:keys [show-chain persistent]} @ui-state]
      (if persistent
        (q/stroke 0.0 0.05)
        (do
          (q/stroke 0.0 1.0)
          (q/background 1.0)))
      (when show-chain
        (doseq [[p q] (g/edges chain)]
          (q/line p q)))
      (doseq [p (g/vertices chain)]
        (cq/circle p 5)))))

(defn ui-controls []
  [:div.ui-controls
   (ctrl/checkbox ui-state "Persistent" [:persistent])
   (ctrl/numeric ui-state "Chain Links" [:n] [2 16 1])
   (ctrl/checkbox ui-state "Show Chain" [:show-chain])
   (ctrl/checkbox ui-state "Odd Even" [:odd-even])
   (ctrl/checkbox ui-state "Stretchy" [:stretchy])])

(defn page []
  [:div
   (sketch/component
    :size [800 600]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [:div.contained.explanation
    [ui-controls]]])

(sketch/definition epicycles
  {:created-at "2022-12-06"
   :tags #{}
   :type :quil}
  (ctrl/mount page))
