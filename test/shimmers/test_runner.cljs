(ns ^:figwheel-hooks shimmers.test-runner
  (:require [shimmers.algorithm.line-clipping-test]
            [shimmers.algorithm.lines-test]
            [shimmers.algorithm.minimum-spanning-tree-test]
            [shimmers.algorithm.rtree-test]
            [shimmers.algorithm.space-colonization-test]
            [shimmers.algorithm.square-packing-test]
            [shimmers.algorithm.polygon-detection-test]
            [shimmers.automata.memory-test]
            [shimmers.automata.simplify-test]
            [shimmers.common.sequence-test]
            [shimmers.math.core-test]
            [shimmers.math.geometry-test]
            [shimmers.math.geometry.group-test]
            [shimmers.math.hexagon-test]
            [cljs-test-display.core :as td]
            [cljs.pprint :as pp]
            [cljs.test :as t]
            [clojure.string :as str]
            [goog.dom :as gdom]
            [goog.dom.classlist :as classlist]
            [pjstadig.print :as p]
            [pjstadig.util :as util]))

(enable-console-print!)

;; borrowed from https://github.com/bhauman/cljs-test-display/issues/5#issuecomment-619090019
(def dummy-env (t/empty-env))

;; reporting with diffs
(set! td/add-fail-node!
      (fn add-fail-node! [m]
        (let [out (binding [cljs.test/*current-env* dummy-env] ;; we don't want `humane-test-output` to modify the env
                    (with-out-str
                      (util/report- (p/convert-event m))))
              clean-out (->> (str/split out #"\n")
                             (drop-while #(not (str/starts-with? % "expected")))
                             (str/join "\n")
                             (str (with-out-str (pp/pprint (:expected m))) "\n"))
              node (td/div :test-fail
                           (td/contexts-node)
                           (td/div :fail-body
                                   (when-let [message (:message m)]
                                     (td/div :test-message message))
                                   (td/n :pre {}
                                         (td/n :code {} clean-out))))
              curr-node (td/current-node)]
          (js/console.log clean-out)
          (classlist/add curr-node "has-failures")
          (classlist/add (td/current-node-parent) "has-failures")
          (gdom/appendChild curr-node node))))

;; to view, visit http://localhost:9500/figwheel-extra-main/tests
(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app-tests")
   'shimmers.algorithm.line-clipping-test
   'shimmers.algorithm.lines-test
   'shimmers.algorithm.minimum-spanning-tree-test
   'shimmers.algorithm.rtree-test
   'shimmers.algorithm.space-colonization-test
   'shimmers.algorithm.square-packing-test
   'shimmers.algorithm.polygon-detection-test
   'shimmers.automata.memory-test
   'shimmers.automata.simplify-test
   'shimmers.common.sequence-test
   'shimmers.math.core-test
   'shimmers.math.geometry-test
   'shimmers.math.geometry.group-test
   'shimmers.math.hexagon-test))

(defn ^:after-load render-on-reload []
  (test-run))

(test-run)
