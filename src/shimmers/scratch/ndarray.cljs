(ns shimmers.scratch.ndarray
  "From https://github.com/thi-ng/ndarray/blob/master/src/index.org and
  https://github.com/thi-ng/ndarray/blob/master/src/contours.org."
  (:require [thi.ng.ndarray.core :as nd]))

(comment
  (def a (nd/ndarray :float64 (range 9) [3 3]))
  (nd/set-at a 1 1 0)
  (seq a)
  ;; => (0 1 2 3 4 5 6 7 8)
  (seq (nd/step a -1 nil))
  ;; => (6 7 8 3 4 5 0 1 2)
  (seq (nd/step a 1 nil))
  ;; => (0 1 2 3 4 5 6 7 8)
  (seq (nd/step a -1 -1))
  (-> a (nd/truncate-h 2 2) (nd/truncate-l 1 1) nd/index-seq))
