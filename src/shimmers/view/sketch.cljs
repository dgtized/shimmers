(ns shimmers.view.sketch
  (:require [goog.dom :as dom]
            [quil.core :as q]
            [reagent.dom :as rdom]
            [reitit.frontend.easy :as rfe]
            [shimmers.common.sequence :as cs]
            [shimmers.common.ui :as ui]
            [shimmers.math.deterministic-random :as dr]
            [shimmers.view.favicon :as favicon]))

;; detect window size for initial setup?
(defn fit-window []
  [(/ (.-innerWidth js/window) 2)
   (/ (.-innerHeight js/window) 2)])

;; Note that seed is required so that the path "changes", even though some
;; sketches are not using seed.
(defn sketch-link [method sketch-name]
  (method ::sketch-by-name
          {:name sketch-name}
          {:seed (dr/fresh-seed-value)}))

(defn inject-quil-host-if-missing!
  "inject a quil-host canvas for quil/p5.js"
  []
  (when-not (dom/getElement "quil-host")
    (let [host (dom/getRequiredElement "sketch-host")
          attrs {"id" "quil-host" "class" "canvas-frame"}]
      (dom/appendChild host (dom/createDom "div" (clj->js attrs))))))

(defn start-sketch [sketch]
  ;; TODO wire up :seed to pass to run-sketch

  ;; unfortunately neither Clojurescript or Javascript have an interface for
  ;; setting a seed. For quil based sketches, a seed can be specified as that is
  ;; encoded into the p5js applet state. However that will not inform
  ;; rand-nth/rand-int/rand or thi.ng/math/random calls. So need to handle that
  ;; on a sketch by sketch basis and find or implement a library to help.
  (when-let [seed (:seed sketch)]
    ;; migrates set random-seed to sketches that use it?
    ;; performance optimizations?
    (dr/random-seed seed))

  (when (= (:type sketch) :quil)
    (shimmers.view.sketch/inject-quil-host-if-missing!))

  ;; disable favicon animation during sketch
  (favicon/stop)

  (ui/screen-view (name (:id sketch)))
  (when-let [run-sketch (:fn sketch)]
    (apply run-sketch [])))

;; TODO: limit to dependencies used by sketch
(defn stop-sketch [_]
  ;; force active video capture to stop
  (doseq [video (dom/getElementsByTagName "video")]
    (.stop (first (.getTracks (aget video "srcObject")))))

  ;; kill existing sketch at quil-host if present
  (when-let [quil (q/get-sketch-by-id "quil-host")]
    (q/with-sketch quil (q/exit)))

  ;; TODO: only unmount components used by sketch?
  (doseq [id ["sketch-host"
              "interface" "explanation"
              "route-debug-mount" "debug-mount"]]
    (when-let [node (dom/getElement id)]
      (rdom/unmount-component-at-node node)))

  ;; enable favicon animation when viewing indices
  (favicon/start 100))

(defn restart-sketch [sketch]
  (sketch-link rfe/push-state (:id sketch)))

(defn cycle-sketch [sketch known-names]
  (->> (name (:id sketch))
       (cs/cycle-next known-names)
       (sketch-link rfe/push-state)))

(defn sketch-controls [sketch known-names]
  [:section.controls
   [:span
    [:button {:on-click #(cycle-sketch sketch known-names)} "Next"]
    [:button {:on-click #(restart-sketch sketch)} "Restart"]
    [:button {:on-click #(rfe/push-state :shimmers.view.index/by-alphabetical)} "All"]]
   [:span
    [:a {:href (:href (ui/code-link sketch))} (name (:id sketch))]]
   [:span#framerate]])

(defn sketch-by-name [sketch known-names]
  [:div
   [sketch-controls sketch known-names]])

(defn generate [sketch-id]
  [:button.generate {:on-click #(restart-sketch {:id sketch-id})} "Generate"])
