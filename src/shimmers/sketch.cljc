(ns shimmers.sketch
  (:require [quil.sketch :refer [wrap-fns]]))

;; Problem:
;;
;; q/defsketch both defines and mounts the sketch. To delay sketch invocation
;; until selection from index or routing route, each namespace wraps the call to
;; defsketch in an outer `run-sketch` method. The index is then built from the
;; list of run-sketches specified and when a link is followed or the app is
;; restarted, the the run-sketch is re-triggered. However that makes it
;; difficult to specify per sketch information like creation time, tags, or to
;; associate other actions at time of mount. As "run-sketch" is the magic
;; starting point that also limits each namespace to one sketch.
;;
;; defquil is a first step towards controlling the mechanism of defining
;; sketches. Currently it mostly mimics the existing mechanism of defining a
;; single `run-sketch`, while adding a hook for running a function at mount.
;;
;; Plan:
;;
;; * Handle warning about undeclared `wrap-fns`
;; * Add an SVG equivalent `defsvg` or better yet add a dispatch parameter to `defsketch`?
;; * Wrap quil.sketch/sketch call with appropriate reagent definitions so that they respect react lifecycle hooks?
;; * Allow more than one sketch per namespace
;; ** 2+ sketches in parallel (probably less useful except for long form explanation)
;; ** Multiple sketches from the same namespace, each with own index entry
;; * Propagate metadata like creation time, tags, or a display name to index view for sorting?
;; * Is it possible to wrap defsketch and reduces the overlap?
;; * Assist in passing parameters like RNG seed into the sketch at invoke?
;;
;; Modified from defsketch in
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
         (when-let [~mount ~(:on-mount opts)]
           (~mount))

         (when-not (some #(= :no-start %) ~(:features opts))
           (quil.sketch/add-sketch-to-init-list
            {:fn ~app-name
             :host-id ~(:host opts)}))))))
