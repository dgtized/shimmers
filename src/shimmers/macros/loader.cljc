(ns shimmers.macros.loader)

(defmacro sketches-with-meta
  "Attaches :meta information for each sketch during compile time."
  [& sketches]
  (into [] (for [pair (partition 2 sketches)]
             `{:id ~(first pair)
               :fn ~(second pair)
               :meta (meta (var ~(second pair)))})))
