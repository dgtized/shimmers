(ns shimmers.automata.programs)

(defn op->instruction [op]
  (if (vector? op)
    op
    (case op
      :forward [:forward 50]
      :left [:rotate (- (/ Math/PI 2))]
      :right [:rotate (+ (/ Math/PI 2))])))

(defn compile [program]
  (map op->instruction program))

(defn rotate [degrees]
  [:rotate (/ (* degrees Math/PI) 180)])

;; (weighted [frequency instruction] ...)
(defn weighted [& options]
  (into [] (mapcat (fn [[n instruction]]
                     (repeat n instruction))
                   (partition 2 options))))
(comment (weighted 1 [:forward 10] 3 [:forward 5]))

(def petals (compile [:forward :forward :left :forward :left [:rotate 1]]))
(def skribbles [[:forward 20]
                [:one-of [[:forward 10] [:forward 20]]]
                [:rotate 1]
                [:one-of [[:color [0 50 200 50]] [:color [0 0 0 25]]]]])
(def test-halt [[:forward 50] [:halt 0]])
(def test-fork [[:forward 10]
                [:fork 0]
                [:one-of [(rotate 60) (rotate -60) [:halt 0]]]])

(def make-tree
  [[:forward 10]
   [:one-of [[:fork 0] [:fork 0] [:fork 0]
             [:halt 0] [:halt 0] [:halt 0] [:halt 0]
             [:rotate 0.5] [:rotate -0.5] [:rotate 0.25] [:rotate -0.25]
             [:forward 2] [:forward 5] [:forward 2] [:forward 5]
             [:heading (* 3 (/ Math/PI 2))]]]])

(def test-recursive
  [[:one-of [[:forward 10]
             [:one-of [[:rotate 0.5] [:rotate -0.5]]]]]])

(def recursive-tree
  [[:forward 10]
   [:one-of [[:one-of [[:rotate 0.5] [:rotate -0.5]]]
             [:one-of [[:forward 2] [:forward 5]]]]]
   [:one-of (weighted 2 [:forward 10]
                      2 [:fork 0]
                      1 [:heading (* 3 (/ Math/PI 2))]
                      1 [:halt 0])]])

(def test-random [[:forward [:random 50]] [:rotate 1]])
(def test-goto [[:forward 100] [:rotate 1] [:forward 20] [:goto 1]])

(def test-interesting
  [[:rotate 5]
   [:one-of [[:forward 18]
             [:halt 0]
             [:rotate -0.8]]]
   [:fork 0]
   [:rotate -0.1]
   [:forward 23]
   [:heading 4.8]
   [:rotate 0.3]])

(def test-interesting2
  [[:forward 42]
   [:fork 0]
   [:color [0 0 0 10]]
   [:heading 2.4]
   [:one-of [[:forward 43]
             [:halt 0]
             [:fork 0]
             [:rotate 4.2]]]
   [:fork 0]
   [:rotate -0.9]
   [:color [:gradient :rainbow1]]
   [:rotate 2.1]])

(def interesting-star-grid
  [[:forward 1.0]
   [:one-of [[:fork 0.0]
             [:rotate -0.1]
             [:forward 47.0]
             [:fork 0.0]]]
   [:fork 0.0]
   [:rotate 1.4]])

(def interesting-gears
  [[:forward 63.0]
   [:rotate -0.6]
   [:fork 0.0]
   [:one-of [[:color [0 0 0 10]]
             [:fork 0.0]
             [:fork 0.0]
             [:rotate -0.7]]]
   [:rotate -0.1]])
