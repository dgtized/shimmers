(ns shimmers.registry
  "Registry of loaded sketches that can participate in routing/indexing and
  display.")

(def sketches (atom {}))

(defn add! [sketch]
  (swap! sketches assoc (:sketch-id sketch) sketch))
