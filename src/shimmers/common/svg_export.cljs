(ns shimmers.common.svg-export)

(def svg-header
  "<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
")

;; cribbed from http://bl.ocks.org/curran/7cf9967028259ea032e8
(defn as-file [svg comment]
  (let [svg-as-xml (.serializeToString (js/XMLSerializer.) svg)
        data-url (str "data:image/svg+xml,"
                      (js/encodeURIComponent svg-header)
                      (js/encodeURIComponent (str "<!--\n" comment "-->\n"))
                      (js/encodeURIComponent svg-as-xml))]
    data-url))

(defn download [id filename comment]
  (fn []
    (let [el (.getElementById js/document id)
          data-url (as-file el comment)
          link (.createElement js/document "a")]
      (.appendChild (.-body js/document) link)
      (.setAttribute link "href" data-url)
      (.setAttribute link "download" filename)
      (.click link))))
