(ns shimmers.sketch
  (:require #?(:cljs [goog.dom :as dom])
            [quil.sketch :include-macros true]
            #?(:cljs [shimmers.common.ui.quil :as ui-quil])
            [shimmers.macros.loader :as loader :include-macros true]
            [shimmers.registry :as registry]))

;; Copied from
;; https://github.com/quil/quil/blob/1f214e712d834ede311fdc652eafe9cc0232c96e/src/cljs/quil/sketch.clj#L21
;; to make it available in cljc without a warning. *Somehow* it should be
;; possible to load both versions, but this works for now.
(defn wrap-fns
  "[[wrap-fns]] allows dynamic redefinition of a function such as `draw` and
  `update` in cljs. This is achieved by wrapping all provided functions to
  anonymous functions such that `my-draw` function turns into
  `(fn [& args] (apply my-draw args))`. This adds a level of indirection
  so that when quil calls `draw`, it invokes anonymous function which in
  turn always calls `my-draw` by name and if you redefine, the new version
  will be used. Hence we need this cryptic macro."
  [opts]
  (into {}
        (for [[k v] opts]
          (if (symbol? v)
            [k `(if (fn? ~v) (fn [& ~'args] (apply ~v ~'args)) ~v)]
            [k v]))))

#?(:cljs
   (defn inject-quil-host-if-missing!
     "inject a quil-host canvas for quil/p5.js"
     [host]
     (when-not (dom/getElement host)
       (let [sketch (dom/getRequiredElement "sketch-host")
             attrs {"id" host "class" "canvas-frame"}]
         (dom/appendChild sketch (dom/createDom "div" (clj->js attrs)))))))

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
;; * Consolidate `definition` and `defquil`?
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
  [sketch-page-name & options]
  (let [raw-opts (apply hash-map options)
        opts (->> raw-opts
                  (merge {:host "quil-host"
                          :sketch-id (keyword sketch-page-name)})
                  wrap-fns)
        runner (vary-meta sketch-page-name assoc :export true)
        sketch-start (vary-meta (symbol (str sketch-page-name '-start))
                                assoc :export true)]
    `(do
       (defn ~sketch-start []
         (quil.sketch/sketch
          ~@(apply concat (seq opts))))

       (defn ~runner []
         (inject-quil-host-if-missing! ~(:host opts))
         (when-let [mount# ~(:on-mount opts)]
           (mount#))

         (when-not (some #(= :no-start %) ~(:features opts))
           (quil.sketch/add-sketch-to-init-list
            {:fn ~sketch-start
             :host-id ~(:host opts)})))

       (let [page-identifier# ~(keyword sketch-page-name)
             m# (meta (var ~sketch-page-name))]
         (registry/add! page-identifier#
                        {:id page-identifier#
                         :type :quil
                         :fn ~runner
                         :created-at ~(:created-at opts)
                         :tags ~(:tags opts #{})
                         :ns (:ns m#)
                         :file (:file m#)
                         :line (:line m#)})))))


;; `component` is a macro specifically to allow repl changes to
;; setup/update/draw. Otherwise using `shimmers.common.ui.quil/sketch-component`
;; is better if defining each component programatically.
(defmacro component [& options]
  (let [raw-opts (apply hash-map options)
        opts (wrap-fns raw-opts)]
    `[shimmers.common.ui.quil/sketch-component ~opts]))

(defmacro definition
  [sketch-page-name options & body]
  (let [runner (vary-meta sketch-page-name merge {:export true})
        options (assoc options :sketch-id (keyword sketch-page-name))]
    `(do (defn ~runner []
           ~@body)

         (let [page-identifier# ~(keyword sketch-page-name)
               m# (meta (var ~sketch-page-name))]
           (registry/add! page-identifier#
                          {:id page-identifier#
                           :type ~(:type options)
                           :fn ~runner
                           :created-at ~(:created-at options)
                           :tags ~(:tags options #{})
                           :ns (:ns m#)
                           :file (:file m#)
                           :line (:line m#)})))))
