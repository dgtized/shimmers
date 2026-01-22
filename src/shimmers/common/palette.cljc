(ns shimmers.common.palette
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [shimmers.common.sequence :as cs]
   [shimmers.common.svg :as csvg]
   [shimmers.math.deterministic-random :as dr]
   [shimmers.math.equations :as eq]
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
    :or {width 256
         height 16
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
      (csvg/svg {:width width :height height :stroke "none"}))))

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
   :green-shades
   "https://artsexperiments.withgoogle.com/artpalette/colors/54a867-c2d7bf-98c79e-1e4629-22823c"
   :grey-charcoal-tan-mint-olive
   "https://artsexperiments.withgoogle.com/artpalette/colors/5d6a66-303737-ded5c2-97bfb0-88968d"
   :brown-tan-shell-blue-brown
   "https://artsexperiments.withgoogle.com/artpalette/colors/634123-a19169-cbc2ac-264888-89693b"
   :yellow-brown
   "https://artsexperiments.withgoogle.com/artpalette/colors/aca192-5d503f-e4c111-806d4e-3d352d"
   :shell-olive-blue-charcoal-red
   "https://artsexperiments.withgoogle.com/artpalette/colors/eee5ce-7e937a-8eadb4-495659-aa604d"
   :blue-grey-red-white-lavender
   "https://artsexperiments.withgoogle.com/artpalette/colors/485985-5a5357-a94d3d-efeef2-b6a2a8"
   :teals-shell-yellow-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/538591-52806f-bec8c1-d8d47f-3875b4"
   :charcoal-grey-yellow-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/3f4340-8c8d83-accbc6-ecd6b5-6f9eb8"
   :sand-sky-water-black-grey
   "https://artsexperiments.withgoogle.com/artpalette/colors/d8b082-68a1aa-6f8585-191513-90867c"
   :grey-brown-yellow-grey-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/c7c8c4-594540-c6b165-9f9a98-454d7d"
   :tan-shell-forest-green-offwhite
   "https://artsexperiments.withgoogle.com/artpalette/colors/a18a74-e5e2d4-577a44-688a4f-e1dbc1"
   :sand-brown-red-tan-yellow
   "https://artsexperiments.withgoogle.com/artpalette/colors/dbbe95-6d462e-8a1d20-9f7c5c-d09b56"
   :sand-black-maroon-brown-yellow
   "https://artsexperiments.withgoogle.com/artpalette/colors/d1b779-382517-8d4a31-9b6838-ba8b38"
   :sand-brown-red-slate-blue
   "https://artsexperiments.withgoogle.com/artpalette/colors/b99c53-6d543c-7f2f1c-b7a689-303967"})

(def db
  (map named-url palette-urls))

(defn by-name [id]
  (cs/find-first (fn [{:keys [name]}]  (= name id)) db))

(defn by-names [ids]
  (filter (fn [{:keys [name]}] (contains? (set ids) name))
          db))

(comment
  (by-name :blue-yellow-tan-brown)
  (by-names [:blue-yellow-tan-brown]))

;; https://www.youtube.com/watch?v=f4s1h2YETNY led me to:
;; https://iquilezles.org/articles/palettes/ and
;; http://dev.thi.ng/gradients/
;; TODO: just re-use thi.ng.color.gradients/cosine-gradient-color?
(defn smooth-palette
  "Output an RGB triplet ranged from 0.0 to 1.0."
  [[ax ay az] [bx by bz] [cx cy cz] [dx dy dz] t]
  (gv/vec3 (+ ax (* bx (math/cos (* eq/TAU (+ (* cx t) dx)))))
           (+ ay (* by (math/cos (* eq/TAU (+ (* cy t) dy)))))
           (+ az (* bz (math/cos (* eq/TAU (+ (* cz t) dz)))))))

(def smooth-palettes
  {:gold-blue
   (partial smooth-palette
            (gv/vec3 0.5 0.5 0.5)
            (gv/vec3 0.5 0.5 0.5)
            (gv/vec3 1.0 1.0 1.0)
            (gv/vec3 0.0 0.1 0.2))})

(comment
  (let [palette (:gold-blue smooth-palettes)]
    (map (fn [t] [t (palette t)]) (range 0.0 2.0 0.05))))

(defn generate
  ([] (generate db))
  ([palettes]
   {:palette (:colors (dr/rand-nth palettes))}))
