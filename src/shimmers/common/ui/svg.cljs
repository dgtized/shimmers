(ns shimmers.common.ui.svg
  (:require
   [reagent-keybindings.keyboard :as kb]
   [shimmers.common.palette :as palette]
   [shimmers.common.svg-export :as svg-export]
   [shimmers.sketch :as sketch]
   [shimmers.view.sketch :as view-sketch]))

(defn download-shortcut [id filename]
  [kb/kb-action "alt-s"
   (fn [] (svg-export/download id filename))])

(defn default-controls [{:keys [explanation] :as args}]
  [:div
   [:p.center [view-sketch/generate args]]
   (when explanation
     [explanation args])])

(defn column-controls [{:keys [explanation] :as args}]
  [:div.flexcols
   [:p.center [view-sketch/generate args]]
   (when explanation
     [explanation args])])

(defn palette-controls
  [{:keys [explanation]
    {:keys [palette]} :params
    :as args}]
  [:div
   [:div.flexpalette
    [view-sketch/generate args]
    [palette/as-svg {} palette]]
   (when explanation
     [:div.flexcenter
      [explanation args]])])

(defn with-controls [sketch-args controls]
  (assoc sketch-args :controls controls))

(defn with-explanation [sketch-args explanation]
  (assoc sketch-args :explanation explanation))

(defn with-param-gen [sketch-args generator]
  (assoc sketch-args :param-gen generator))

;; TODO: sketch-args *could* contains a dimensions parameter which when passed
;; to `scene` would define the size of the sketch. However upgrading to this
;; will require changes on a sketch by sketch basis to derive height/width
;; parameters (or a sceen bounds) from the parameters to scene instead of per
;; sketch globals. An implementation of `rv` that took this context would also
;; be required.
(defn frame-page
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

(defn page
  "Bind parameters for page before creating the scene.

  This allows a sketch page to generate parameters before rendering the scene.
  Parameters like view or debug output can change without regenerating the
  parameters from scratch, but still re-rendering the scene."
  [{:keys [param-gen] :as sketch-args} scene]
  (frame-page (if param-gen (let [params (param-gen sketch-args)
                                  args (dissoc sketch-args :param-gen)]
                              (if (map? params)
                                (update args :params merge params)
                                (assoc args :params params)))
                  sketch-args)
              scene))
