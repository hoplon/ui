(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [javelin.core         :refer [cell= cell]]
    [hoplon.core          :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui            :refer [window elem path line-path image video b t ]]
    [hoplon.ui.attrs      :refer [- r font hsl rgb]]
    [hoplon.ui.transforms :refer [linear]])
  (:require-macros
    [hoplon.ui.app   :refer [view-tpl]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path= [c path] (cell= (get-in c path) (partial swap! c assoc-in path)))

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce sess (cell {:state :transitions}))

;;; derivations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def state (path= sess [:state]))
(def route (cell= [[state]] #(reset! state (ffirst %))))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def g 16)

;-- colors --------------------------------------------------------------------;

(def black  (rgb 0x1F1F1F))
(def orange (rgb 0xE73624))
(def blue   (rgb 0x009BFF))
(def yellow (rgb 0xF5841F))
(def white  (rgb 0xFFFFFF))

;-- typography ----------------------------------------------------------------;

(def baloo (font :truetype "baloo.ttf" :generic :sans-serif))

(def +menu-text+  {:t 18 :tf baloo :tc :white :tw 1})
(def +label-text+ {:t 16 :tf baloo :tc :white :tw 1})

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def menu-items
  [:transitions "transitions"
   :scales      "scales"
   :media       "media"])

(def transforms
  ["Linear" linear])

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn media-view []
  (elem :sh (r 1 1)
    #_(elem :sh (>sm md) :p 50 :g 50 :a :mid
      (image :a :mid :b 2 :bc :black :url "http://placehold.it/200x100"
        (elem "content"))
      (image :s 200 :a :mid :b 2 :bc :black :fit :fill :url "http://placehold.it/200x100"
        (elem "filled"))
      (image :s 200 :p 20 :b 2 :bc :black :fit :cover :url "http://placehold.it/200x100"
        (elem "covered"))
      (image :s 200 :a :mid :b 2 :bc :black :fit :contain :url "http://placehold.it/200x100"
        (elem "contained"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :fill :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "filled"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :cover :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "covered"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :contain :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "contained")))))

(defn scales-view []
  (let [xs (mapv (partial * 30) (range 21))]
    (elem :sh (r 1 1) :sv (- (r 1 1) 80) :p g :a :mid
      (elem :s 600 :b 4 :bc :grey :d :pile
        (for [[index [label function]] (map-indexed vector (partition 2 transforms))]
          (path +label-text+ :s (r 1 1) :b 600 :p g :k 4 :kc (hsl (* (count transforms) 20) (r 1 2) (r 1 2)) :av :mid
            :src (mapcat vector xs (mapv (linear [0 600] [0 -600]) xs))))))))

(defn transitions-view []
  (let [size (cell 150)]
    (elem :sh (r 1 1) :p g :g g
      (for [[index [label function]] (map-indexed vector (partition 2 transforms))]
        (elem +label-text+ :sh (t size 1000 function) :sv 60 :p g :c (hsl (* (count transforms) 20) (r 1 2) (r 1 2)) :av :mid :m :pointer :click #(swap! size (partial + 150))
          label)))))

(window :src route :title "Hoplon UI"
  (elem :sh (r 1 1) :sv 80 :av :mid :p g :g g :c orange :bt 8 :bc yellow
    (image :s 40 :m :pointer :click #(reset! state :home) :url "hoplon-logo.png")
    (elem :sh (>sm (- (r 1 1) (+ 60 g))) :g g :ah :end
      (for [[*state label] (partition 2 menu-items) :let [sel (cell= (= *state state))]]
        (elem +menu-text+ :sh (>sm :auto) :m :pointer :bb (cell= (when sel 3)) :bc :white :click #(reset! state *state)
          label))))
  (case-tpl state
    :media       (media-view)
    :scales      (scales-view)
    :transitions (transitions-view)))
