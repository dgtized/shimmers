(ns shimmers.math.dust
  (:require
   [clojure.math :as math]
   [thi.ng.math.core :as tm]))

(defn sample-mix
  "Given a list of values `xs`, interpolate between them with `t`.

  Optionally provide a `mix-f` with args `(a,b,t)`, ie interpolate from `a` to
  `b` by sliding `t` from 0.0 to 1.0."
  ([xs t] (sample-mix tm/mix* xs t))
  ([mix-f xs t]
   (let [n (count xs)
         v (* t (count xs))
         idx (math/floor v)
         st (tm/fract v)]
     (mix-f (nth xs (mod idx n))
            (nth xs (mod (inc idx) n))
            st))))

(comment
  (let [xs [0.1 0.2 0.9]]
    (map (fn [t] [t
                 (sample-mix xs t)
                 (sample-mix (fn [a b t]
                               (tm/mix* a b (tm/smoothstep* 0.05 0.95 t)))
                             xs t)])
         (range 0 3 0.05))))

(defn quantize
  [xs value]
  (loop [lower 0
         upper (dec (count xs))]
    (if (> lower upper)
      nil
      (let [mid (quot (+ lower upper) 2)
            midvalue (nth xs mid)]
        (cond
          (tm/delta= value midvalue)
          [mid midvalue]
          (= lower (dec upper))
          (let [v0 (nth xs lower)
                v1 (nth xs upper)]
            (if (< (- value v0) (- v1 value))
              v0 v1))
          (> midvalue value) (recur lower mid)
          (< midvalue value) (recur mid upper))))))

(comment
  (map (fn [v] [v (quantize (range 5) v)])
       (range -1 6 0.33)))
