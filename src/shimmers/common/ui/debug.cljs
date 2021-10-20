(ns shimmers.common.ui.debug
  (:require
   [fipp.edn :as fedn]
   [fipp.ednize :refer [IEdn]]
   [shimmers.common.ui.controls :as ctrl]
   [thi.ng.geom.types :refer [Rect2]]
   [thi.ng.geom.vector :refer [Vec2]]
   [thi.ng.math.core :as tm]))

;; Simplify IEdn output for pretty printing
(extend-protocol IEdn
  Rect2
  (-edn [{:keys [p size]}]
    (tagged-literal 'Rect2 {:p p :size size}))

  Vec2
  (-edn [s]
    (let [[x y] s]
      (tagged-literal 'vec2 [(tm/roundto x 0.01)
                             (tm/roundto y 0.01)]))))

(defn state []
  (ctrl/state {}))

(defn display [atom]
  [:pre (with-out-str (fedn/pprint (deref atom)))])

(defn mount [atom]
  (ctrl/mount #(display atom) "debug-mount"))
