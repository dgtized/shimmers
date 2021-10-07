(ns shimmers.view.favicon
  (:require [goog.dom :as dom]))

(defn circle [ctx [x y] r [hue brightness]]
  (.beginPath ctx)
  (.arc ctx x y r 0 (* 2 Math/PI))
  (set! (.-fillStyle ctx) (str "hsl(" hue ", 80%, " brightness "%, 0.45)"))
  (.fill ctx))

(defn mag [base scale f offset phase t]
  (+ base (* 0.066 base scale (f (+ offset (* phase t))))))

(defn pattern [ctx size]
  (let [m (/ size 2)
        t (/ (.now js/Date.) 6000)
        r (/ size 3)
        hue (int (mag 180 30 Math/sin 0 0.85 t))
        width (+ 80 (* 20 (Math/cos (+ 100 (* 0.66 t)))))]
    (circle ctx [(mag m 6 Math/cos 9 0.95 t)
                 (mag m 4 Math/sin 10 0.95 t)]
            r [hue
               (int (mag 50 8 Math/sin 30 0.7 t))])
    (circle ctx [(mag m 2 Math/cos -10 1.0 t)
                 (mag m 5 Math/sin -10 1.0 t)]
            r [(mod (- hue width) 360)
               (int (mag 55 6 Math/sin 10 0.8 t))])
    (circle ctx [(mag m 3 Math/cos 2 1.05 t)
                 (mag m 6 Math/sin 1 1.05 t)]
            r [(mod (+ hue width) 360)
               (int (mag 45 9 Math/cos -10 0.9 t))])))

;; Translated from https://medium.com/@alperen.talaslioglu/building-dynamic-favicon-with-javascript-223ad7999661
(defn favicon []
  (let [icon (dom/getElement "favicon")
        size 24
        canvas (dom/getElement "favicon-canvas")]
    (set! (.-width canvas) size)
    (set! (.-height canvas) size)
    (pattern (dom/getCanvasContext2D canvas) size)
    (set! (.-href icon) (.toDataURL canvas "image/png"))))

(defonce favicon-active (atom true))
(defn auto-update! [interval]
  (when @favicon-active
    (favicon)
    (.setTimeout js/window auto-update! interval)))

(defn start [interval]
  (reset! favicon-active true)
  (auto-update! interval))

(defn stop []
  (reset! favicon-active false))
