(ns shimmers.math.geometry.group)

(defn group
  [children]
  (if (sequential? children)
    children
    [children]))
