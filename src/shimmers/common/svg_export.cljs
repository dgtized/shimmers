(ns shimmers.common.svg-export)

;; cribbed from http://bl.ocks.org/curran/7cf9967028259ea032e8
(defn as-file [svg]
  (let [svg-as-xml (.serializeToString (js/XMLSerializer.) svg)
        data-url (str "data:image/svg+xml," (js/encodeURIComponent svg-as-xml))]
    data-url))

(defn download [id]
  (fn []
    (let [el (.getElementById js/document id)
          data-url (as-file el)
          link (.createElement js/document "a")]
      (.appendChild (.-body js/document) link)
      (.setAttribute link "href" data-url)
      (.setAttribute link "download" "test.svg")
      (.click link))))
