(ns shimmers.common.ui.debug
  (:require
   [fipp.edn :as fedn]
   [fipp.ednize :refer [IEdn]]
   #?@(:cljs
       [[shimmers.common.ui.controls :as ctrl]
        [thi.ng.geom.types :refer [Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]]
        [thi.ng.geom.vector :refer [Vec2 Vec3]]]
       :clj [[thi.ng.geom.types]
             [thi.ng.geom.vector]]))
  #?(:clj (:import [thi.ng.geom.types Circle2 Line2 LineStrip2 Polygon2 Rect2 Triangle2]
                   [thi.ng.geom.vector Vec2 Vec3])))

;; identity operation for a map record that removes type info so tagged-literal
;; will not be called recursively with the original type. However this should
;; pickup extra keys if something else has been assoced in beyond the record keys.
;; TODO: worth including meta info?
(defn untyped [s]
  (zipmap (keys s) (vals s)))

;; See https://github.com/brandonbloom/fipp/issues/83 for symbol hack, basically
;; it's forcing EdnPrinter to believe it's a symbol and not a float so it
;; doesn't wrap it in a string.
(defn fixed-width
  "Format float `v` to 2 decimal places as long as it's not infinite."
  [v]
  #?(:cljs
     (if (or (integer? v) (infinite? v))
       v
       (symbol (.toFixed v 2)))
     :clj
     (if (or (integer? v)
             (= v Double/POSITIVE_INFINITY)
             (= v Double/NEGATIVE_INFINITY))
       v
       (symbol (format "%.2f" v)))))

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
    (tagged-literal 'v2 (mapv fixed-width s)))

  Vec3
  (-edn [s]
    (tagged-literal 'v3 (mapv fixed-width s))))

(defmacro time-it
  "Evaluates expr and stores it in debug `atom` at `key`. Returns the value of expr."
  [atom key expr]
  `(let [start# (cljs.core/system-time)
         ret# ~expr]
     (cljs.core/swap! ~atom cljs.core/assoc-in ~key
                      (cljs.core/str (.toFixed (- (cljs.core/system-time) start#) 3)
                                     " msecs"))
     ret#))

(defmacro span-prof
  [desc expr]
  `(let [start# (cljs.core/system-time)
         ret# ~expr
         stop# (cljs.core/system-time)]
     (cljs.core/tap> [:profile {:desc ~desc :start start# :stop stop#}])
     ret#))

(defn profile-to [sink]
  (fn [tap-value]
    (when (= :profile (first tap-value))
      (swap! sink conj (second tap-value)))))

(defn pre-edn
  ([edn] (pre-edn edn {}))
  ([edn options]
   [:pre.debug [:code (with-out-str (fedn/pprint edn options))]]))

(defn display [atom]
  (pre-edn (deref atom)))

#?(:cljs
   (do
     (defn state
       ([] (ctrl/state {}))
       ([init] (ctrl/state init)))

     (defn mount
       ([atom] (mount atom "interface"))
       ([atom element]
        (fn [] (ctrl/mount #(display atom) element))))))

(defn with-tap-log [f]
  (let [tap-log (atom [])
        result (with-redefs [tap> (fn [v] (swap! tap-log conj v) true)]
                 (f))]
    {:result result :log @tap-log}))

(comment (with-tap-log (fn [] (tap> 1) (tap> 2) 3)))
