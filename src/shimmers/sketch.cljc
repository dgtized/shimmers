(ns shimmers.sketch
  (:require [quil.sketch :include-macros true]
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.registry :as registry]))

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
;; - [x] add sketches to a registry automatically ala
;;   https://github.com/quil/quil/blob/master/src/clj/quil/snippets/macro.clj#L52
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
        runner (vary-meta app-name merge
                          {:export true
                           :created-at (:created-at opts)})
        sketch-start (vary-meta (symbol (str app-name '-start))
                                assoc :export true)]
    `(do
       (defn ~sketch-start []
         (quil.sketch/sketch
          ~@(apply concat (seq opts))))

       (defn ~runner []
         (when-let [mount# ~(:on-mount opts)]
           (mount#))

         (when-not (some #(= :no-start %) ~(:features opts))
           (quil.sketch/add-sketch-to-init-list
            {:fn ~sketch-start
             :host-id ~(:host opts)})))

       (let [m# (meta (var ~app-name))]
         (swap! registry/sketches assoc (str ~app-name)
                {:id (loader/namespace-to-id (:ns m#))
                 :type :quil
                 :fn ~runner
                 :created-at ~(:created-at opts)
                 :file (:file m#)
                 :line (:line m#)})))))

(defmacro defsvg
  [app-name options & body]
  (let [runner (vary-meta app-name merge {:export true})]
    `(do (defn ~runner []
           ~@body)

         (let [m# (meta (var ~app-name))]
           (swap! registry/sketches assoc (str ~app-name)
                  {:id (loader/namespace-to-id (:ns m#))
                   :type :svg
                   :fn ~runner
                   :created-at ~(:created-at options)
                   :file (:file m#)
                   :line (:line m#)})))))
