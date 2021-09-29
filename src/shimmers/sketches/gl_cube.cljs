(ns shimmers.sketches.gl-cube
  "Proof of concept spike using thi.ng directly to animate with webgl shaders.
  Cribbed from a whole list of examples."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [shimmers.common.ui.controls :as ctrl]
            [shimmers.sketch :as sketch :include-macros true]
            [thi.ng.geom.aabb :as aabb]
            [thi.ng.geom.attribs :as attr]
            [thi.ng.geom.core :as geom]
            [thi.ng.geom.gl.camera :as cam]
            [thi.ng.geom.gl.core :as gl]
            [thi.ng.geom.gl.glmesh :as glmesh]
            [thi.ng.geom.gl.shaders :as shaders]
            [thi.ng.geom.gl.webgl.animator :as anim]
            [thi.ng.geom.gl.webgl.constants :as glc]
            [thi.ng.geom.matrix :as mat]))

;; https://github.com/Rovanion/webgl-clojurescript-tutorial
;; and https://github.com/thi-ng/geom/blob/develop/examples/gl/webgl.org
(def shader-spec
  {:vs "void main() { vUV = uv; gl_Position = proj * view * model * vec4(position, 1.0); }"
   :fs "void main() { gl_FragColor = vec4(vUV, 1.0, 1.0); }"
   :uniforms {:view :mat4
              :proj :mat4
              :model :mat4}
   :attribs {:position :vec3
             :uv :vec2}
   :varying  {:vUV :vec2}})

(def cube (-> (aabb/aabb 1)
              (geom/center)
              (geom/scale-size 0.8)
              (geom/as-mesh {:mesh (glmesh/indexed-gl-mesh 12 #{:uv})
                             :attribs {:uv (attr/face-attribs (attr/uv-cube-map-v 256 false))}})))

(defn combine-model-shader-and-camera
  [gl-ctx model shader-spec camera]
  (-> model
      (gl/as-gl-buffer-spec {})
      (assoc :shader (shaders/make-shader-from-spec gl-ctx shader-spec))
      (gl/make-buffers-in-spec gl-ctx glc/static-draw)
      (cam/apply camera)))

(defn spin [t]
  (-> mat/M44
      (geom/rotate-x (/ t 10))
      (geom/rotate-y (/ t 20))
      (geom/rotate-z (/ t 5))))

(defn draw-frame! [gl-ctx frame t]
  (doto gl-ctx
    (gl/clear-color-and-depth-buffer 0 0 0 1 1)
    (gl/draw-with-shader (assoc-in frame
                                   [:uniforms :model] (spin t)))))

;; Cribbed from https://gist.github.com/PlumpMath/66ad1d1654597056bbdde24b9808a883
;; and http://timothypratley.blogspot.com/2017/01/reagent-deep-dive-part-2-lifecycle-of.html
(defn canvas-thing [attributes mount]
  (r/create-class
   {:component-did-mount
    (fn [this]
      (r/set-state this {:active true})
      (mount this))
    :component-will-unmount
    (fn [this]
      (r/set-state this {:active false}))
    :reagent-render
    (fn [_]
      [:canvas attributes])}))

(defn page []
  [canvas-thing {:width 800 :height 600}
   (fn [this]
     (let [canvas (rdom/dom-node this)
           gl-ctx (gl/gl-context canvas)
           camera (cam/perspective-camera {})
           frame (combine-model-shader-and-camera gl-ctx cube shader-spec camera)]
       ;; Continuing animating until canvas is inactive
       ;; UNKNOWN: Does the gl rendering context & shaders need to be disposed of?
       ;; TODO: calculate & update framerate?
       (anim/animate (fn [t] (when (:active (r/state this))
                              (draw-frame! gl-ctx frame t))))))])

(sketch/defthing gl-cube
  {:created-at "2021-09-29"
   :tags #{:webgl}}
  (ctrl/mount page "thing-host"))
