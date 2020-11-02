(ns shimmers.macros.loader)

(defmacro sketch-meta [sketch]
  `{:fn ~sketch :meta (meta (var ~sketch))})
