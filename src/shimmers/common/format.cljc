(ns shimmers.common.format)

;; See https://github.com/brandonbloom/fipp/issues/83 for symbol hack, basically
;; it's forcing EdnPrinter to believe it's a symbol and not a float so it
;; doesn't wrap it in a string.
(defn fixed-width
  "Format float `v` to `width` decimal places as long as it's not infinite."
  [v width]
  #?(:cljs
     (if (or (integer? v) (infinite? v))
       v
       (symbol (.toFixed v width)))
     :clj
     (if (or (integer? v)
             (= v Double/POSITIVE_INFINITY)
             (= v Double/NEGATIVE_INFINITY))
       v
       (symbol (format (str "%." width "f") v)))))
