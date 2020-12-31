(ns ^:figwheel-hooks shimmers.test-runner
  (:require [shimmers.automata.simplify-test]
            [shimmers.algorithm.space-colonization-test]
            [cljs.test]
            [cljs-test-display.core]))

(enable-console-print!)

;; to view, visit http://localhost:9500/figwheel-extra-main/tests
(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app-tests")
   'shimmers.automata.simplify-test
   'shimmers.algorithm.space-colonization-test))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
