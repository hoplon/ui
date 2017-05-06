(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [hoplon.ui.transforms :as t]
    [javelin.core         :refer [cell= cell]]
    [hoplon.core          :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui            :refer [window elem line lines file path line-path image video b t ]]
    [hoplon.ui.attrs      :refer [- r font hsl rgb]]))

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
(def grey   (rgb 0xCCCCCC))
(def orange (rgb 0xE73624))
(def blue   (rgb 0x009BFF))
(def yellow (rgb 0xF5841F))
(def white  (rgb 0xFFFFFF))

;-- typography ----------------------------------------------------------------;

(def baloo (font :truetype "baloo.ttf" :generic :sans-serif))

(def +title+ {:t 26 :tf baloo :tc black :tw 2})
(def +menu+  {:t 18 :tf baloo :tc white :tw 1.5})
(def +label+ {:t 16 :tf baloo :tc white :tw 1})
(def +field+ {:t 24 :tf baloo :tc black :tw 1})

;-- attributes -----------------------------------------------------------------

(def -button- {:ph 12 :pv 6 :r 3 :a :mid :c grey :b 2 :bc black :m :pointer})
(def -field-  {:ph 12 :pv 6 :r 3 :c grey :b 2 :bc black})

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def menu-items
  [:forms       "forms"
   :transitions "transitions"
   :scales      "scales"
   :media       "media"])

(def transforms
  ["Linear"    t/linear
   "Quadratic-In" t/quadratic-in
   "Quadratic-Out" t/quadratic-out
   "Quadratic-In-Out" t/quadratic-in-out
   "Cubic-In" t/cubic-in
   "Cubic-Out" t/cubic-out
   "Cubic-In-Out" t/cubic-in-out
   "Quartic-In" t/quartic-in
   "Quartic-Out" t/quartic-out
   "Quartic-In-Out" t/quartic-in-out
   "Quintic-In" t/quintic-in
   "Quintic-Out" t/quintic-out
   "Quintic-In-Out" t/quintic-in-out
   "Sine-In" t/sine-in
   "Sine-Out" t/sine-out
   "Sine-In-Out" t/sine-in-out])

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

(defn forms-view []
  (elem :sh (r 1 1) :p g
    (elem +title+ :sh (r 1 1)
      "Forms")
    (let [data (cell (or (:data sess) {}))]
      (cell= (prn :data data))
      (elem +label+ :s (r 1 1) :p (b 16 sm 50) :g 16 :ah :end
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Name"    :src (path= data [:name]))
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Email"   :src (path= data [:email]))
        (lines -field-  +field+ :sh (r 1 1) :rows 10 :prompt "Message" :src (path= data [:message]))
        (file  -button- +field+ :sh (r 1 1) :prompt "photo" :src (path= data [:photo]))
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! sess assoc :data data)
          "Submit")))))

(defn scales-view []
  (let [xs                 (mapv (partial * 30) (range 21))
        indexed-transforms (map-indexed vector (partition 2 transforms))]
    (elem :sh (r 1 1) :sv (- (r 1 1) 80) :p g :a :mid
      (elem :s 600 :b 4 :bc :grey :d :pile
        (elem :s (r 1 1) :p g
          (for [[index [label _]] indexed-transforms]
            (elem +label+ :sh (r 1 1) :tc (hsl (* index 20) (r 1 2) (r 1 2))
              label)))
        (for [[index [label function]] indexed-transforms]
          (path +label+ :s (r 1 1) :b 600 :k 4 :kc (hsl (* index 20) (r 1 2) (r 1 2)) :av :mid
            :src (interleave xs (mapv (function [0 600] [0 -600]) xs))
            (prn label (interleave xs (mapv (function [0 600] [0 -600]) xs))))))
      (elem :s 600 :b 4 :bc :grey :d :pile
        (elem :s (r 1 1) :p g
          (for [[index [label _]] indexed-transforms]
            (elem +label+ :sh (r 1 1) :tc (hsl (* index 20) (r 1 2) (r 1 2))
              label)))
        (for [[index [label function]] indexed-transforms]
          (path +label+ :s (r 1 1) :b 600 :k 4 :kc (hsl (* index 20) (r 1 2) (r 1 2)) :av :mid
            :src (interleave xs (mapv (function [300 600] [-300 -600]) xs))
            (prn label (interleave xs (mapv (function [300 600] [-300 -600]) xs)))))))))

(defn transitions-view []
  (let [size (cell 150)]
    (elem :sh (r 1 1) :p g :g g
      (for [[index [label function]] (map-indexed vector (partition 2 transforms))]
        (elem :sh (r 1 1)
          (elem +label+ :sh (t size 1000 function) :sv 60 :p g :c (hsl (* index 20) (r 1 2) (r 1 2)) :av :mid :m :pointer :click #(swap! size (partial + 150))
            label))))))

(window :src route :title "Hoplon UI" :scroll true
  (elem :sh (r 1 1) :sv 80 :av :mid :p g :g g :c orange :bt 4 :bc yellow
    (image :s 50 :m :pointer :click #(reset! state :home) :url "hoplon-logo.png")
    (elem :sh (>sm (- (r 1 1) (+ 60 g))) :g g :ah :end
      (for [[*state label] (partition 2 menu-items) :let [sel (cell= (= *state state))]]
        (elem +menu+ :sh (>sm :auto) :m :pointer :bb (cell= (when sel 3)) :bc :white :click #(reset! state *state)
          label))))
  (case-tpl state
    :forms       (forms-view)
    :media       (media-view)
    :scales      (scales-view)
    :transitions (transitions-view)))
