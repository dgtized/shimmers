(ns ^:figwheel-hooks shimmers.figwheel-runner
  (:require
   [cljs-test-display.core :as td]
   [figwheel.main.testing :refer-macros [run-tests]]
   [fipp.edn :as fedn]
   ;; load namespaces to test
   [shimmers.test-namespaces]))

(enable-console-print!)

(defn prettier [content]
  (td/n :pre {}
        (td/n :code {} (with-out-str (fedn/pprint content {:width 80})))))

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
  (run-tests (cljs-test-display.core/init! "app-tests")))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
