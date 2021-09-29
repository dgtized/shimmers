(ns shimmers.view.favicon
  (:require [goog.dom :as dom]))

;; Translated from https://medium.com/@alperen.talaslioglu/building-dynamic-favicon-with-javascript-223ad7999661
(defn favicon []
  (let [icon (dom/getElement "favicon")
        size 24
        canvas (dom/createElement "canvas")]
    (set! (.-width canvas) size)
    (set! (.-height canvas) size)
    (let [ctx (dom/getCanvasContext2D canvas)
          t (/ (.now js/Date.) 4000)
          hue (+ (mod (int t) 30) 175)
          brightness (+ 50 (int (* 20 (Math/sin t))))]
      (.beginPath ctx)
      (.arc ctx (/ size 2) (/ size 2) (/ size 2.01) 0 (* 2 Math/PI))
      (set! (.-fillStyle ctx) (str "hsl(" hue ", 80%, " brightness "%, 1.0)"))
      (.fill ctx))
    (set! (.-href icon) (.toDataURL canvas "image/png"))))

(defonce favicon-active (atom true))
(defn auto-update! [interval]
  (when @favicon-active
    (favicon)
    (.setTimeout js/window auto-update! interval)))
