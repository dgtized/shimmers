(ns shimmers.common.ui.canvas-attributes)

(defmacro defattr
  [sym field]
  `(defn ~sym
     ([ctx#] (~field ctx#))
     ([ctx# value#] (set! (~field ctx#) value#) ctx#)))
