(ns shimmers.sketch
  (:require [quil.sketch :refer [wrap-fns]]))

;; Copied from defsketch in
;; https://github.com/quil/quil/blob/master/src/cljs/quil/sketch.clj#L22
;; to allow modifications like auto-starting explanation and metadata like date.
(defmacro defquil
  [app-name & options]
  (let [raw-opts (apply hash-map options)
        opts     (->> raw-opts
                      (merge {:host "quil-host"})
                      quil.sketch/wrap-fns)
        runner 'run-sketch
        mount (gensym "mount")]
    `(do
       (defn ~(vary-meta app-name assoc :export true) []
         (quil.sketch/sketch
          ~@(apply concat (seq opts))))

       (defn ~(vary-meta runner assoc :export true) []
         (when-let [~mount ~(:mount opts)]
           (~mount))

         (when-not (some #(= :no-start %) ~(:features opts))
           (quil.sketch/add-sketch-to-init-list
            {:fn ~app-name
             :host-id ~(:host opts)}))))))
