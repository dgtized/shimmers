(ns shimmers.sketches.dithering
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]
   [shimmers.common.framerate :as framerate]
   [shimmers.common.quil :as cq]
   [shimmers.common.ui.controls :as ctrl]
   [shimmers.common.video :as video]
   [shimmers.math.vector :as v]
   [shimmers.sketch :as sketch :include-macros true]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(def modes [:dither :boxes :circles
            :ring-density :sampled-ring-density :color-displace
            :flow-field :spiral-deformation
            :ascii-70 :ascii-10 :ascii-n])

(defonce ui-state (ctrl/state {:mode :dither}))

(defn setup []
  (let [width 320
        height 240]
    {:width width
     :height height
     :capture (video/capture width height)}))

(defn closest-color [color]
  (* 256 (q/round (/ color 256))))

(defn idx [x y width]
  (* 4 (+ x (* y width))))

(defn write [pixels i v]
  (aset pixels (+ i 0) v)
  (aset pixels (+ i 1) v)
  (aset pixels (+ i 2) v)
  (aset pixels (+ i 3) 255))

(defn propagate [source i error]
  (aset source i (+ error (aget source i))))

;; https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
;; TODO: atkinson dithering from https://beyondloom.com/blog/dither.html
;; TODO: GPU assisted dithering: http://alex-charlton.com/posts/Dithering_on_the_GPU/
(defn dither [capture width height]
  (let [image (q/create-image width height)
        source (q/pixels capture)
        target (q/pixels image)]
    (dotimes [y height]
      (dotimes [x width]
        (let [i (idx x y width)
              old (aget source i)
              new (closest-color old)
              error (- old new)]
          (write target (idx (- width x) y width) new)
          (propagate source (idx (inc x) y width)
                     (* error (/ 7 16)))
          (propagate source (idx (dec x) (inc y) width)
                     (* error (/ 3 16)))
          (propagate source (idx x (inc y) width)
                     (* error (/ 5 16)))
          (propagate source (idx (inc x) (inc y) width)
                     (* error (/ 1 16))))))
    (q/update-pixels image)
    image))

;; Further dithering info:
;; https://codegolf.stackexchange.com/questions/26554/dither-a-grayscale-image?newreg=40354d96e979455ea596c92e2db5a225

(defn boxes [capture width height]
  (q/rect-mode :corner)
  (q/no-stroke)
  (q/fill 0)
  (let [box-size 3
        max-x (/ width box-size)
        pixels (q/pixels capture)]
    (dotimes [y (/ height box-size)]
      (dotimes [x max-x]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              size (q/map-range (/ (+ r g b) 3) 0 255 (* box-size 1.75) 0.5)]
          (q/rect (* (- max-x x) 2 box-size) (* y 2 box-size) size size))))))

(defn circles [capture width height]
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/fill 0)
  (let [box-size 3
        max-x (/ width box-size)
        pixels (q/pixels capture)]
    (dotimes [y (/ height box-size)]
      (dotimes [x max-x]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              size (q/map-range (/ (+ r g b) 3) 0 255 (* box-size 1.8) 0.2)]
          (q/ellipse (* (- max-x x) 2 box-size) (* y 2 box-size) size size))))))

;; This would be more interesting with a particular sequence of sample locations
;; instead of a grid.
(defn ring-density [capture width height]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 1.0)
  (q/stroke 0)
  (let [box-size 6
        max-x (/ width box-size)
        pixels (q/pixels capture)]
    (dotimes [y (/ height box-size)]
      (dotimes [x max-x]
        (let [ix (idx (* x box-size) (* y box-size) width)
              r (aget pixels ix)
              g (aget pixels (+ ix 1))
              b (aget pixels (+ ix 2))
              size (tm/map-interval (+ r g b) 768 0 0.5 12)]
          (q/ellipse (* (- max-x x) 2 box-size) (* y 2 box-size) size size))))))

(defn sampling [width height]
  (repeatedly 3000 #(int (tm/random (* width height)))))

(def sample-map (memoize sampling))

(defn sampled-ring-density [capture width height]
  (q/ellipse-mode :radius)
  (q/no-fill)
  (q/stroke-weight 0.5)
  (q/stroke 0)
  (let [pixels (q/pixels capture)]
    (doseq [is (sample-map width height)]
      (let [ix (* 4 is)
            x (mod is width)
            y (/ is width)
            r (aget pixels ix)
            g (aget pixels (+ ix 1))
            b (aget pixels (+ ix 2))
            size (tm/map-interval (+ r g b) 0 768 4 24)]
        (q/ellipse (* (- width x) 2) (* y 2) size size)))))

(defn color-displace [capture width height]
  (q/rect-mode :corner)
  (q/no-stroke)
  (let [box-size 4
        max-x (/ width box-size)
        pixels (q/pixels capture)
        displace (/ box-size 4)
        v 200]
    (dotimes [y (/ height box-size)]
      (dotimes [x max-x]
        (let [r (aget pixels (idx (* x box-size) (* y box-size) width))
              g (aget pixels (+ (idx (* x box-size) (* y box-size) width) 1))
              b (aget pixels (+ (idx (* x box-size) (* y box-size) width) 2))
              rsize (q/map-range r 0 255 (* box-size 1.2) 0.2)
              gsize (q/map-range g 0 255 (* box-size 1.2) 0.2)
              bsize (q/map-range b 0 255 (* box-size 1.2) 0.2)
              cx (* (- max-x x) 2 box-size)
              cy (* y 2 box-size)]
          (q/fill v 0 0 255)
          (q/ellipse (+ cx displace) (+ cy displace) rsize rsize)
          (q/fill 0 v 0 255)
          (q/ellipse (- cx displace) (+ cy displace) gsize gsize)
          (q/fill 0 0 v 48)
          (q/ellipse cx (- cy displace) bsize bsize))))))

(defn grayscale-at [pixels width p]
  (let [x (int (:x p))
        y (int (:y p))
        r (aget pixels (idx x y width))
        g (aget pixels (+ (idx x y width) 1))
        b (aget pixels (+ (idx x y width) 2))]
    (/ (+ r g b) 768.0)))

(defn flow-points [pixels width p r n]
  (reductions (fn [p] (->> (grayscale-at pixels width p)
                          (* tm/TWO_PI)
                          (v/polar r)
                          (tm/+ p)))
              p (range n)))

(defn flow-field [capture width height]
  (q/stroke 0)
  (q/no-fill)
  (q/stroke-weight 0.3)
  (let [pixels (q/pixels capture)
        sample-size 8
        box-size 2]
    (dotimes [y (/ height sample-size)]
      (dotimes [x (/ width sample-size)]
        (let [starting-point (tm/* (gv/vec2 x y) sample-size)]
          (q/begin-shape)
          (doseq [[x y] (flow-points pixels width starting-point sample-size 8)]
            (q/curve-vertex (* box-size (- width x)) (* box-size y)))
          (q/end-shape))))))

;; Future Idea: delta-motion, track the level change of a pixel across n frames,
;; and show a flow field from the difference

;; TODO: tune some more, it's too jittery from video noise?
;; Re-purposed from deformed-spiral sketch
(defn spiral [center dr dtheta steps noise-at t]
  (for [theta (range 0 (* steps dtheta) dtheta)]
    (let [pos (v/polar (* dr (/ theta tm/TWO_PI)) (+ theta t))
          n (noise-at pos)]
      (tm/+ center pos (v/polar 9.0 (* n tm/TWO_PI))))))

(defn spiral-deformation [capture width height]
  (q/stroke 0)
  (q/stroke-weight 0.8)
  (q/no-fill)
  (let [pixels (q/pixels capture)
        screen (rect/rect 0 0 width height)
        center (cq/rel-vec 0.5 0.5)
        t (/ (q/frame-count) 400)
        noise-at (fn [[x y]] (let [[i j] (tm/* (tm/+ center (gv/vec2 x y)) 0.5)
                                  p (gv/vec2 (- width i) j)]
                              (if (g/contains-point? screen p)
                                (grayscale-at pixels width p)
                                0)))
        points (spiral center 10.0 0.3 720 noise-at t)]
    (cq/draw-curve-path points)))

;; http://paulbourke.net/dataformats/asciiart/
(def ascii-70 (vec " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"))
(def ascii-10 (vec " .:-=+*#%@"))
(def ascii-n (map str (range 10)))

(defn ascii [ascii capture width height]
  (q/fill 0)
  (let [resolution (count ascii)
        pixels (q/pixels capture)
        sample-size 4
        box-size 8]
    (q/text-size box-size)
    (dotimes [y (/ height sample-size)]
      (dotimes [x (/ width sample-size)]
        (let [v (grayscale-at pixels width (tm/* (gv/vec2 x y) sample-size))]
          (q/text-char (nth ascii (int (* v resolution)))
                       (* (- (/ width sample-size) x) box-size)
                       (* (inc y) box-size)))))))

(defn draw [{:keys [capture width height]}]
  (q/background 255)
  (let [ui-mode (:mode (deref ui-state))]
    (case ui-mode
      :dither (q/image (dither capture width height) 0 0 (* width 2) (* height 2))
      :boxes (boxes capture width height)
      :circles (circles capture width height)
      :ring-density (ring-density capture width height)
      :sampled-ring-density (sampled-ring-density capture width height)
      :color-displace (color-displace capture width height)
      :flow-field (flow-field capture width height)
      :spiral-deformation (spiral-deformation capture width height)
      :ascii-70 (ascii ascii-70 capture width height)
      :ascii-10 (ascii ascii-10 capture width height)
      :ascii-n (ascii ascii-n capture width height)))
  ;; (q/image capture (+ 10 width) 0)
  )

(defn page []
  [sketch/with-explanation
   (sketch/component
    :size [640 480]
    :setup setup
    :draw draw
    :middleware [m/fun-mode framerate/mode])
   [ctrl/change-mode ui-state modes]])

(sketch/definition dithering
  {:created-at "2020-11-21"
   :tags #{:camera}
   :type :quil}
  (ctrl/mount page))
