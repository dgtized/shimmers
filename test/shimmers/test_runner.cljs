(ns ^:figwheel-hooks shimmers.test-runner
  (:require [shimmers.algorithm.space-colonization-test]
            [shimmers.automata.simplify-test]
            [shimmers.common.sequence-test]
            [shimmers.math.core-test]
            [shimmers.math.geometry-test]
            [shimmers.math.hexagon-test]
            [cljs.test]
            [cljs-test-display.core]))

(enable-console-print!)

;; to view, visit http://localhost:9500/figwheel-extra-main/tests
(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app-tests")
   'shimmers.algorithm.space-colonization-test
   'shimmers.automata.simplify-test
   'shimmers.common.sequence-test
   'shimmers.math.core-test
   'shimmers.math.geometry-test
   'shimmers.math.hexagon-test))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
