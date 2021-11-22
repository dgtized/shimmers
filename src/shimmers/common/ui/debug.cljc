(ns shimmers.common.ui.debug
  (:require
   [fipp.edn :as fedn]
   [fipp.ednize :refer [IEdn]]
   #?@(:cljs
       [[shimmers.common.ui.controls :as ctrl]
        [thi.ng.geom.types :refer [Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]]
        [thi.ng.geom.vector :refer [Vec2]]]
       :clj [[thi.ng.geom.types]
             [thi.ng.geom.vector]])
   [thi.ng.math.core :as tm])
  #?(:clj (:import [thi.ng.geom.types Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]
                   [thi.ng.geom.vector Vec2])))

;; identity operation for a map record that removes type info so tagged-literal
;; will not be called recursively with the original type. However this should
;; pickup extra keys if something else has been assoced in beyond the record keys.
;; TODO: worth including meta info?
(defn untyped [s]
  (zipmap (keys s) (vals s)))

;; Simplify IEdn output for pretty printing
(extend-protocol IEdn
  Circle2
  (-edn [s]
    (tagged-literal 'Circle2 (untyped s)))

  Line2
  (-edn [s]
    (tagged-literal 'Line2 (untyped s)))

  LineStrip2
  (-edn [s]
    (tagged-literal 'LineStrip2 (untyped s)))

  Polygon2
  (-edn [s]
    (tagged-literal 'Polygon2 (untyped s)))

  Rect2
  (-edn [s]
    (tagged-literal 'Rect2 (untyped s)))

  Triangle2
  (-edn [s]
    (tagged-literal 'Triangle2 (untyped s)))

  Vec2
  (-edn [s]
    (let [[x y] s]
      (tagged-literal 'vec2 [(tm/roundto x 0.01)
                             (tm/roundto y 0.01)]))))

(defn display [atom]
  [:pre (with-out-str (fedn/pprint (deref atom)))])

#?(:cljs
   (do
     (defn state []
       (ctrl/state {}))

     (defn mount [atom]
       (ctrl/mount #(display atom) "debug-mount"))))
