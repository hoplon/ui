(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [hoplon.ui.transforms :as t]
    [javelin.core         :refer [cell cell= dosync]]
    [hoplon.core          :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui            :refer [window elem line lines file path line-path image video b t]]
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

(def bd 3)
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

(def -button- {:ph 12 :pv 6 :r 3 :a :mid :c grey :b bd :bc black :m :pointer})
(def -field-  {:ph 12 :pv 6 :r 3         :c grey :b bd :bc black})

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def menu-items
  [:forms       "forms"
   :transitions "transitions"
   :scales      "scales"
   :media       "media"
   :components  "components"])

(def images
  ["Mondrian One"   "mondrian-1.png"
   "Mondrian Two"   "mondrian-2.png"
   "Mondrian Three" "mondrian-3.png"])

(def transforms
  ["Linear"             t/linear
   "Quadratic-In"       t/quadratic-in
   "Quadratic-Out"      t/quadratic-out
   "Quadratic-In-Out"   t/quadratic-in-out
   "Cubic-In"           t/cubic-in
   "Cubic-Out"          t/cubic-out
   "Cubic-In-Out"       t/cubic-in-out
   "Quartic-In"         t/quartic-in
   "Quartic-Out"        t/quartic-out
   "Quartic-In-Out"     t/quartic-in-out
   "Quintic-In"         t/quintic-in
   "Quintic-Out"        t/quintic-out
   "Quintic-In-Out"     t/quintic-in-out
   "Sine-In"            t/sine-in
   "Sine-Out"           t/sine-out
   "Sine-In-Out"        t/sine-in-out
   "Exponential-In"     t/exp-in
   "Exponential-Out"    t/exp-out
   "Exponential-In-Out" t/exp-in-out
   "Circular-In"        t/circ-in
   "Circular-Out"       t/circ-out
   "Circular-In-Out"    t/circ-in-out])

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn media-view []
  (elem :sh (r 1 1)
    (elem :sh (>sm md) :p 50 :g 50 :a :mid
      (image :a :mid :b bd :bc :black :src "http://placehold.it/200x100"
        (elem "content"))
      (image :s 200 :a :mid :b bd :bc :black :fit :fill :src "http://placehold.it/200x100"
        (elem "filled"))
      (image :s 200 :p 20 :b bd :bc :black :fit :cover :src "http://placehold.it/200x100"
        (elem "covered"))
      (image :s 200 :a :mid :b bd :bc :black :fit :contain :src "http://placehold.it/200x100"
        (elem "contained"))
      (video :s 200 :a :mid :b bd :bc :black :fit :fill :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "filled"))
      (video :s 200 :a :mid :b bd :bc :black :fit :cover :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "covered"))
      (video :s 200 :a :mid :b bd :bc :black :fit :contain :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "contained")))))

(defn components-view []
  (elem :sh (r 1 1) :p g
    (elem +title+ :sh (r 1 1)
      "Carousel")
    (let [idx    (cell 0)
          images (partition 2 images)]
      (elem :s (r 1 1) :g g :a :mid
        (elem -button- :click #(swap! idx (fn [i] (mod (dec i) (count images))))
          "Prev")
        (elem :sh 800 :sv 600 :d :pile
          (for [[idx* [label filename]] (map-indexed vector images)]
            (image :s (r 1 1) :o (cell= (r ~(t (cell= (if (= idx* idx) 1 0)) 2000 t/linear) 1)) :src filename)))
        (elem -button- :click #(swap! idx (fn [i] (mod (inc i) (count images))))
          "Next")))))

(defn forms-view []
  (elem :sh (r 1 1) :p g
    (elem +title+ :sh (r 1 1)
      "Forms")
    (let [data (cell (or (:data sess) {}))]
      (cell= (prn :data data))
      (elem +label+ :s (r 1 1) :p (b 16 sm 50) :g 16 :ah :end
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Name"    :src (path= data [:name]))
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Address" :src (path= data [:address]))
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Email"   :src (path= data [:email]))
        (lines -field-  +field+ :sh (r 1 1) :rows 10 :prompt "Message" :src (path= data [:message]))
        (file  -button- +field+ :sh (r 1 1) :prompt "photo" :src (path= data [:photo]))
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! sess assoc :data data)
          "Submit")))))

(defn scales-view []
  (let [size 300
        step 5
        col  4
        bx  (cell 30)
        by  (cell 30)
        ex  (cell 270)
        ey  (cell 270)
        xs  (cell= (range bx (+ ex step) step))]
    (elem :sh (r 1 1) :sv (- (r 1 1) 80) :ah :mid
      (elem :sh (+ (* (+ 300 g) col) (* g 2) (- g)) :p g :g g
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! bx (fn [x] (- x step)))
          "Beg Left  5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! bx (fn [x] (+ x step)))
          "Beg Right 5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! by (fn [y] (+ y step)))
          "Beg Up    5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! by (fn [y] (- y step)))
          "Beg Down  5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! ex (fn [x] (- x step)))
          "End Left  5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! ex (fn [x] (+ x step)))
          "End Right 5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! ey (fn [y] (+ y step)))
          "End Up    5")
        (elem  -button- +field+ :sh (>sm 300) :click #(swap! ey (fn [y] (- y step)))
          "End Down  5")
        (for [[index [label function]] (map-indexed vector (partition 2 transforms))
              :let [g  8
                    px (cell 0)
                    py (cell 0)
                    tx (t px 2000 function)
                    ty (t py 2000 function)]]
          (elem :sh size :gv g
            (elem +label+ :s (r 3 4) :ph g :tc (hsl (* index 20) (r 1 2) (r 1 2))
              label)
            (elem +label+ :s (r 1 4) :ph g :ah :end :tc (hsl (* index 20) (r 1 2) (r 1 2)) :m :pointer :click #(dosync (reset! px (- @ex @bx)) (reset! py (- @ey @by)))
              "run >")
            (elem :s (>sm 300) :b bd :bc :grey :d :pile
              (path :s (r 1 1) :b size :k 4 :kc (hsl (* index 20) (r 1 2) (r 1 2)) :av :mid
                :src (cell= (interleave xs (mapv (function [bx ex] [(- by) (- ey)]) xs))))
              (elem :s (r 1 1) :pl (cell= (- bx bd 2)) :ah :beg
                (elem :sv (r 1 1) :sh 2 :c grey))
              (elem :s (r 1 1) :pb (cell= (- by bd 2)) :av :end
                (elem :sh (r 1 1) :sv 2 :c grey))
              (elem :s (r 1 1) :pl (cell= (- ex bd 2)) :ah :beg
                (elem :sv (r 1 1) :sh 2 :c grey))
              (elem :s (r 1 1) :pb (cell= (- ey bd 2)) :av :end
                (elem :sh (r 1 1) :sv 2 :c grey)))
            (elem :sh (r 1 1) :pl bx :pr (cell= (- size ex))
              (elem +label+ :sh tx :sv 4 :r (/ 4 2) :c (hsl (* index 20) (r 1 2) (r 1 2))))))))))

(defn transitions-view []
  (let [size (cell 150)]
    (elem :sh (r 1 1) :p g :g g
      (for [[index [label function]] (map-indexed vector (partition 2 transforms))]
        (elem :sh (r 1 1)
          (elem +label+ :sh (t size 1000 function) :sv 60 :p g :c (hsl (* index 20) (r 1 2) (r 1 2)) :av :mid :m :pointer :click #(swap! size (partial + 150))
            label))))))

(window :src route :title "Hoplon UI" :scroll true
  (elem :sh (r 1 1) :sv 80 :av :mid :p g :g g :c orange :bt 4 :bc yellow
    (image :s 50 :m :pointer :click #(reset! state :home) :src "hoplon-logo.png")
    (elem :sh (>sm (- (r 1 1) (+ 60 g))) :g g :ah :end
      (for [[*state label] (partition 2 menu-items) :let [sel (cell= (= *state state))]]
        (elem +menu+ :sh (>sm :auto) :m :pointer :bb (cell= (when sel 3)) :bc :white :click #(reset! state *state)
          label))))
  (case-tpl state
    :forms       (forms-view)
    :media       (media-view)
    :scales      (scales-view)
    :transitions (transitions-view)
    :components  (components-view)))
