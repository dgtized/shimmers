(ns ^:figwheel-hooks shimmers.test-runner
  (:require [shimmers.algorithm.line-clipping-test]
            [shimmers.algorithm.lines-test]
            [shimmers.algorithm.minimum-spanning-tree-test]
            [shimmers.algorithm.quadtree-test]
            [shimmers.algorithm.rtree-test]
            [shimmers.algorithm.space-colonization-test]
            [shimmers.algorithm.square-packing-test]
            [shimmers.algorithm.polygon-detection-test]
            [shimmers.algorithm.wave-function-collapse-test]
            [shimmers.automata.memory-test]
            [shimmers.automata.simplify-test]
            [shimmers.common.sequence-test]
            [shimmers.math.core-test]
            [shimmers.math.geometry-test]
            [shimmers.math.geometry.ellipse-test]
            [shimmers.math.geometry.group-test]
            [shimmers.math.geometry.intersection-test]
            [shimmers.math.geometry.triangle-test]
            [shimmers.math.graph-test]
            [shimmers.math.hexagon-test]
            [cljs-test-display.core :as td]
            [cljs.test :as t]
            [fipp.edn :as fedn]))

(enable-console-print!)

(defn prettier [content]
  (td/n :pre {}
        (td/n :code {} (with-out-str (fedn/pprint content)))))

;; modified from https://github.com/bhauman/cljs-test-display/issues/5#issuecomment-619090019
;; pretty print expected vs actual with fedn/pprint
(set! td/comparison
      (fn comparison [{:keys [expected actual]}]
        (td/div
         (prettier expected)
         (td/div :actual (td/div :arrow "▶")
                 (prettier actual)))))

;; to view, visit http://localhost:9500/figwheel-extra-main/tests
(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app-tests")
   'shimmers.algorithm.line-clipping-test
   'shimmers.algorithm.lines-test
   'shimmers.algorithm.minimum-spanning-tree-test
   'shimmers.algorithm.quadtree-test
   'shimmers.algorithm.rtree-test
   'shimmers.algorithm.space-colonization-test
   'shimmers.algorithm.square-packing-test
   'shimmers.algorithm.polygon-detection-test
   'shimmers.algorithm.wave-function-collapse-test
   'shimmers.automata.memory-test
   'shimmers.automata.simplify-test
   'shimmers.common.sequence-test
   'shimmers.math.core-test
   'shimmers.math.geometry-test
   'shimmers.math.geometry.ellipse-test
   'shimmers.math.geometry.group-test
   'shimmers.math.geometry.intersection-test
   'shimmers.math.geometry.triangle-test
   'shimmers.math.graph-test
   'shimmers.math.hexagon-test))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
