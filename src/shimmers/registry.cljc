(ns shimmers.registry
  "Registry of loaded sketches that can participate in routing/indexing and
  display.")

(def sketches (atom {}))

(defn add! [name sketch]
  (swap! sketches assoc name sketch))
