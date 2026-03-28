(ns shimmers.common.ui.svg
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.sketch :as sketch]
   [shimmers.view.sketch :as view-sketch]))

(defn download-shortcut [id filename]
  [kb/kb-action "alt-s"
   (fn [] (svg-export/download id filename))])

(defn generate-link [{:keys [sketch-id]}]
  [view-sketch/generate sketch-id])

(defn default-controls [{:keys [explanation] :as args}]
  [:div
   [:p.center [generate-link args]]
   (when explanation
     [explanation args])])

(defn with-controls [sketch-args controls]
  (assoc sketch-args :controls controls))

(defn with-explanation [sketch-args explanation]
  (assoc sketch-args :explanation explanation))

;; TODO: sketch-args *could* contains a dimensions parameter which when passed
;; to `scene` would define the size of the sketch. However upgrading to this
;; will require changes on a sketch by sketch basis to derive height/width
;; parameters (or a sceen bounds) from the parameters to scene instead of per
;; sketch globals. An implementation of `rv` that took this context would also
;; be required.
(defn page
  "Create a sketch from `sketch-args` and a `scene` function.

  This is for SVG generation as it injects a download shortcut to save the
  corresponding svg. It also creates a generate link by default that re-renders
  the page."
  [{:keys [sketch-id controls]
    :or {controls default-controls}
    :as sketch-args}
   scene]
  (fn []
    (let [default-args {:scene-id "scene"}
          {:keys [scene-id] :as args} (merge default-args sketch-args)]
      [sketch/with-explanation
       [:div.canvas-frame
        [scene args]
        [download-shortcut scene-id (name sketch-id)]]
       [controls args]])))

(defn let-page
  "Bind parameters for page before creating the scene.

  This allows a sketch page to generate parameters before rendering the scene.
  Parameters like view or debug output can change without regenerating the
  parameters from scratch, but still re-rendering the scene."
  ([sketch-args gen-params explanation scene]
   (let-page (assoc sketch-args
                    :gen-params gen-params
                    :explanation explanation)
     scene))
  ([sketch-args explanation scene]
   (let-page (assoc sketch-args :explanation explanation) scene))
  ([{:keys [gen-params] :as sketch-args} scene]
   (let [params (gen-params sketch-args)]
     (page (-> sketch-args
               (dissoc :param-gen)
               (update :params merge params))
           scene))))
