(ns shimmers.common.ui.debug
  #?(:cljs
     (:require
      [shimmers.common.edn :as sc-edn]
      [shimmers.common.ui.controls :as ctrl])))

#?(:cljs (def untyped sc-edn/untyped))

;; cljs only
(defmacro time-it
  "Evaluates expr and stores it in debug `atom` at `key`. Returns the value of expr."
  [atom key expr]
  `(let [start# (cljs.core/system-time)
         ret# ~expr]
     (cljs.core/swap! ~atom cljs.core/assoc-in ~key
                      (cljs.core/str (.toFixed (- (cljs.core/system-time) start#) 3)
                                     " msecs"))
     ret#))

;; cljs only
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

#?(:cljs
   (defn pre-edn
     ([edn] (pre-edn edn {}))
     ([edn options]
      [:pre.debug [:code (with-out-str (sc-edn/pprint edn options))]])))

#?(:cljs
   (defn display [atom]
     (pre-edn (deref atom))))

#?(:cljs
   (do
     (defn state
       ([] (ctrl/state {}))
       ([init] (ctrl/state init)))))

(defn with-tap-log [f]
  (let [tap-log (atom [])
        result (with-redefs [tap> (fn [v] (swap! tap-log conj v) true)]
                 (f))]
    {:result result :log @tap-log}))

(comment (with-tap-log (fn [] (tap> 1) (tap> 2) 3)))
