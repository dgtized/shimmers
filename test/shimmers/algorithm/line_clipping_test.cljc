(ns shimmers.algorithm.line-clipping-test
  (:require
   [clojure.test :as t :refer [deftest is] :include-macros true]
   [shimmers.math.geometry.line]
   [shimmers.algorithm.line-clipping :as sut]
   [thi.ng.geom.line :as gl]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

;; TODO: explore https://clojuredocs.org/clojure.test/assert-expr instead of d=
(defn d= [a b] (tm/delta= a b 1e-2))
(def r (rect/rect 1 1 2 2))

(deftest encoding
  (is (= #{:low-x :low-y} (sut/encode-endpoint (gv/vec2 0 0) r)))
  (is (= #{:low-x} (sut/encode-endpoint (gv/vec2 0 2) r)))
  (is (= #{:low-y} (sut/encode-endpoint (gv/vec2 2 0) r)))
  (is (= #{:high-x :high-y} (sut/encode-endpoint (gv/vec2 4 4) r)))
  (is (= #{:high-x} (sut/encode-endpoint (gv/vec2 4 2) r)))
  (is (= #{:high-y} (sut/encode-endpoint (gv/vec2 2 4) r))))

(deftest clip-line
  (is (nil? (sut/clip-line r (gv/vec2 4 4) (gv/vec2 5 4))))
  (is (nil? (sut/clip-line r (gv/vec2 4 4) (gv/vec2 4 2))))
  (is (nil? (sut/clip-line r (gv/vec2 0 0) (gv/vec2 10 0))))
  (is (d= (gl/line2 1 1.5 2 2) (sut/clip-line r (gv/vec2 0 1) (gv/vec2 2 2))))
  (is (d= (gl/line2 2 1 3 1.5) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 10 5))))
  (is (d= (gl/line2 1 1 2 2) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 2 2))))
  (is (d= (gl/line2 1 1 3 3) (sut/clip-line r (gv/vec2 0 0) (gv/vec2 4 4))))
  (is (d= (gl/line2 1 2 3 2) (sut/clip-line r (gv/vec2 0 2) (gv/vec2 4 2)))
      "horizontal line clipping")
  (is (d= (gl/line2 2 1 2 3) (sut/clip-line r (gv/vec2 2 0) (gv/vec2 2 4)))
      "vertical line clipping"))

(deftest hatching
  (is (= 4 (count (sut/hatch-rectangle (rect/rect 2 2 4) 1.0 0.0))))
  (is (every? true?
              (map d=
                   [(gl/line2 [[2 24.68] [101.99 79.32]])
                    (gl/line2 [[2 53.17] [91.38 101.99]])
                    (gl/line2 [[12.62 2] [101.99 50.83]])
                    (gl/line2 [[2 81.66] [39.23 101.99]])
                    (gl/line2 [[64.77 2] [101.99 22.34]])]
                   (sut/hatch-rectangle (rect/rect 2 2 100) 25.0 0.5 [0.5 0.5])))))

(comment (t/run-tests))
