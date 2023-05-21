(ns shimmers.common.palette
  (:require
   [clojure.string :as str]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.rect :as rect]
   [thi.ng.geom.vector :as gv]
   [thi.ng.math.core :as tm]))

(defn url->colors [url]
  (-> url
      (str/split #"/")
      last
      (str/split #"-")))

(defn from-urls [urls]
  (->> urls
       (map url->colors)
       (map (partial map (partial str "#")))))

(defn to-url [palette]
  (->> palette
       (map (fn [color] (str/replace-first color "#" "")))
       (str/join "-")
       (str "https://artsexperiments.withgoogle.com/artpalette/colors/")))

(defn as-svg
  [{:keys [width height class]
    :or {width 400 height 30
         class "palette"}}
   palette]
  (let [cell (/ width (count palette))
        rect (rect/rect 0 0 cell height)]
    (if (seq palette)
      [:a {:href (to-url palette)}
       (csvg/svg {:class class :width width :height height}
         (for [[idx color] (map-indexed vector palette)]
           (-> rect
               (g/translate (tm/* (gv/vec2 idx 0) (gv/vec2 cell 0)))
               (with-meta {:fill (str color)}))))]
      [:p])))

(defn named-url [[name url]]
  {:name name
   :url url
   :colors (->> url url->colors (map (partial str "#")))})

(def palette-urls
  {:blue-yellow-tan-brown
   "https://artsexperiments.withgoogle.com/artpalette/colors/617caa-d1b053-976e27-7a94ae-c9b27d"
   :shell-blue-yellow-grey
   "https://artsexperiments.withgoogle.com/artpalette/colors/c8cccc-7c9aa8-ede4da-a5b6c0-e0c1a2"
   :shell-grey-blues
   "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d"
   :shell-grey-blues-bold
   "https://artsexperiments.withgoogle.com/artpalette/colors/adc7e5-e1e6e7-5087ba-b89474-222982"
   :purple-shell-brown
   "https://artsexperiments.withgoogle.com/artpalette/colors/51467c-dccfbe-d4ba90-aa8c60-726665"
   :shell-aqua-blue-green
   "https://artsexperiments.withgoogle.com/artpalette/colors/d4ddda-51988e-274b75-a0b5c0-2d5429"
   :slate-shell-red-tan-yellow
   "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67"
   :yellow-blue-slate-grey-red
   "https://artsexperiments.withgoogle.com/artpalette/colors/c5962a-30497c-dddecf-7b7b75-8f3020"
   :slate-black-green-forest-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/b1bfc5-212720-6f8f48-49583d-5081ad"
   :red-black-yellow-grey-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/ca2825-161519-d6c844-979593-0b5999"
   :orange-black-blue-shell-red
   "https://artsexperiments.withgoogle.com/artpalette/colors/db9003-332f2e-20778c-d8cdb9-ba3a29"
   :orange-maroon-blues
   "https://artsexperiments.withgoogle.com/artpalette/colors/0c3c56-236884-ce5110-3e160e-338bab"
   :blues-orange-black-shell
   "https://artsexperiments.withgoogle.com/artpalette/colors/204354-34a3bb-f34c1c-241f1e-c0bbb8"
   :blue-pink-brown-orange-lblue
   "https://artsexperiments.withgoogle.com/artpalette/colors/2cb4c8-e7b9a6-5b463c-dc6031-a4bfc1"
   :slate-red-yellow-blue-brown
   "https://artsexperiments.withgoogle.com/artpalette/colors/abb6ba-803f37-e0a964-355b83-b37958"
   :blue-khacki-green-pink-aqua
   "https://artsexperiments.withgoogle.com/artpalette/colors/4585b3-cec69a-479186-cc989d-5dacaa"
   :yellow-pink-blue-aqua-shell
   "https://artsexperiments.withgoogle.com/artpalette/colors/ece39f-f1a39e-8aaccd-5fa6aa-cddcd3"
   :yellow-slate-white-mint-red
   "https://artsexperiments.withgoogle.com/artpalette/colors/f8ce32-525456-f4f3f3-66debf-d51f07"
   :black-shell-red-maroon-red
   "https://artsexperiments.withgoogle.com/artpalette/colors/150d0a-e8dfda-ca202a-420f11-99292f"
   :aqua-foam-black-greeen-teal
   "https://artsexperiments.withgoogle.com/artpalette/colors/69d4d3-ccf5ee-020804-0b4736-389b84"
   }
  )

(def db
  (map named-url palette-urls))

(defn by-name [id]
  (cs/find-first (fn [{:keys [name]}]  (= name id)) db))

(defn by-names [ids]
  (filter (fn [{:keys [name]}] (contains? (set ids) name))
          db))

(comment (by-name :blue-yellow-tan-brown)
         (by-names [:blue-yellow-tan-brown]))

(def blue-yellow-tan-brown
  "https://artsexperiments.withgoogle.com/artpalette/colors/617caa-d1b053-976e27-7a94ae-c9b27d")

(def shell-blue-yellow-grey
  "https://artsexperiments.withgoogle.com/artpalette/colors/c8cccc-7c9aa8-ede4da-a5b6c0-e0c1a2")

(def shell-grey-blues
  "https://artsexperiments.withgoogle.com/artpalette/colors/e7eef0-759acd-81a4d1-9f9a98-454d7d")

(def shell-grey-blues-bold
  "https://artsexperiments.withgoogle.com/artpalette/colors/adc7e5-e1e6e7-5087ba-b89474-222982")

(def purple-shell-brown
  "https://artsexperiments.withgoogle.com/artpalette/colors/51467c-dccfbe-d4ba90-aa8c60-726665")

(def shell-aqua-blue-green
  "https://artsexperiments.withgoogle.com/artpalette/colors/d4ddda-51988e-274b75-a0b5c0-2d5429")

(def slate-shell-red-tan-yellow
  "https://artsexperiments.withgoogle.com/artpalette/colors/2f403d-e9e6d9-b4533a-9b9270-ddbd67")

(def yellow-blue-slate-grey-red
  "https://artsexperiments.withgoogle.com/artpalette/colors/c5962a-30497c-dddecf-7b7b75-8f3020")

(def slate-black-green-forest-blue
  "https://artsexperiments.withgoogle.com/artpalette/colors/b1bfc5-212720-6f8f48-49583d-5081ad")

(def red-black-yellow-grey-blue
  "https://artsexperiments.withgoogle.com/artpalette/colors/ca2825-161519-d6c844-979593-0b5999")

(def orange-black-blue-shell-red
  "https://artsexperiments.withgoogle.com/artpalette/colors/db9003-332f2e-20778c-d8cdb9-ba3a29")

(def orange-maroon-blues
  "https://artsexperiments.withgoogle.com/artpalette/colors/0c3c56-236884-ce5110-3e160e-338bab")

(def blues-orange-black-shell
  "https://artsexperiments.withgoogle.com/artpalette/colors/204354-34a3bb-f34c1c-241f1e-c0bbb8")
