(ns ^:figwheel-hooks shimmers.test-runner
  (:require [shimmers.automata.simplify-test]
            [cljs.test]
            [cljs-test-display.core]))

(enable-console-print!)

(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app-tests")
   'shimmers.automata.simplify-test))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
