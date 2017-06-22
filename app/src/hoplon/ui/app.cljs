(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [hoplon.ui.interpolators :as i]
    [javelin.core    :refer [defc cell cell= dosync lens? alts!]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [window elem line lines file files path line-path image video b t]]
    [hoplon.ui.attrs :refer [- r font hsl lgr rgb sdw]]
    [hoplon.ui.utils :refer [clamp loc x y]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path= [c path] (cell= (get-in c path) (partial swap! c assoc-in path)))

(defn cache [c]
  (let [v (cell nil)
        a (alts! c v)]
    (cell= (first a) #(if (lens? c) (reset! c %) (reset! v %)))))

(defn tx= [c f]
  (let [val (cell nil)
        set #(dosync (when (not= % @c) (f %)) (reset! val nil))]
    (cell= (or val c) #(if (= % ::tx) (set val) (reset! val %)))))

(defn commit! [cell]
  (reset! cell ::tx))

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce sess (cell {:state :components}))

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

(def bd   3)
(def g    16)
(def knob 16)

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

(defc cities
  ["Dublin"   34
   "London"   72
   "Boston"   45
   "Madrid"   54
   "NYC"      80
   "Berlin"   45])

(def transforms
  ["Linear"          i/linear
   "Quadratic In"    i/quadratic-in
   "Quadratic Out"   i/quadratic-out
   "Quadratic"       i/quadratic
   "Cubic In"        i/cubic-in
   "Cubic Out"       i/cubic-out
   "Cubic"           i/cubic
   "Quartic In"      i/quartic-in
   "Quartic Out"     i/quartic-out
   "Quartic"         i/quartic
   "Quintic In"      i/quintic-in
   "Quintic Out"     i/quintic-out
   "Quintic"         i/quintic
   "Sine In"         i/sine-in
   "Sine Out"        i/sine-out
   "Sine"            i/sine
   "Exponential In"  i/exp-in
   "Exponential Out" i/exp-out
   "Exponential"     i/exp
   "Circular In"     i/circ-in
   "Circular Out"    i/circ-out
   "Circular"        i/circ])

;;; components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem slider [{:keys [sh sv s src] r* :r :as attrs}]
  ;- change sizes to body after api refactoring where border becomes stroke
  ;- substract border from sizes / add border to body bh bv b
  ;- support different types of transitions
  ;- consider passing the knob as a child, how to proxy values to knob
  ;- switch from grab to grabbing mouse cursors during drag
  ;- consider tool tip with values
  ;- consider passing curried interpolator function
  ;- handle overflowing margins without overflow: none due to perf problem. new
  ;  box model may fix.
  (let [src    (cache src)
        w      (cell= (or sh s))
        h      (cell= (or sv s))
        kd     (cell= (min 32 w h))
        kr     (cell= (/ kd 2))
        dx->rx (cell= (clamp (i/linear [0       100] [0  (- w kd)]) 0 100))
        rx->dx (cell= (clamp (i/linear [kr (- w kr)] [0       100]) 0 100))
        dy->ry (cell= (clamp (i/linear [0       100] [(- h kd)  0]) 0 100))
        ry->dy (cell= (clamp (i/linear [(- h kr) kr] [0       100]) 0 100))
        pos    (cell= [(dx->rx (x src)) (dy->ry (y src))] #(reset! src [(@rx->dx (x %)) (@ry->dy (y %))]))
        sdw    (sdw 2 2 (rgb 0 0 0 (r 1 14)) 2 0)]
    (elem :d (sdw :inset true) :r r* :m :pointer
      :pl   (t (cell= (x pos)) 300 i/quadratic-out)
      :pt   (t (cell= (y pos)) 300 i/quadratic-out)
      :down #(reset! pos (loc %))
      :move #(when (= (.-which %) 1) (reset! pos (loc %)))
      (dissoc attrs :src)
      (elem :s kd :r (cell= (or r* 16)) :c yellow :b 2 :bc (white :a 0.6) :d sdw :m :grab))))

(defelem hslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector src 0) #(reset! src (x %))) (dissoc attrs :src)))

(defelem vslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector 0 src) #(reset! src (y %))) (dissoc attrs :src)))

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn media-view []
  (elem :sh (r 1 1)
    (elem :sh (>sm md) :p 50 :g 50 :a :mid
      (image :a :mid :b bd :bc :black :src "http://placehold.ii/200x100"
        (elem "content"))
      (image :s 200 :a :mid :b bd :bc :black :fit :fill :src "http://placehold.ii/200x100"
        (elem "filled"))
      (image :s 200 :p 20 :b bd :bc :black :fit :cover :src "http://placehold.ii/200x100"
        (elem "covered"))
      (image :s 200 :a :mid :b bd :bc :black :fit :contain :src "http://placehold.ii/200x100"
        (elem "contained"))
      (video :s 200 :a :mid :b bd :bc :black :fit :fill :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "filled"))
      (video :s 200 :a :mid :b bd :bc :black :fit :cover :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "covered"))
      (video :s 200 :a :mid :b bd :bc :black :fit :contain :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "contained")))))

(defn components-view []
  (elem :sh (r 1 1) :p g :g g
    (elem +title+ :sh (r 1 1)
      "Carousel")
    (let [idx    (cell 0)
          images (partition 2 images)]
      (elem :s (r 1 1) :g g :a :mid
        (elem -button- :click #(swap! idx (fn [i] (mod (dec i) (count images))))
          "Prev")
        (elem :sh 800 :sv 600 :d :pile
          (for [[idx* [label filename]] (map-indexed vector images)]
            (image :s (r 1 1) :o (cell= (r ~(t (cell= (if (= idx* idx) 1 0)) 2000 i/linear) 1)) :src filename)))
        (elem -button- :click #(swap! idx (fn [i] (mod (inc i) (count images))))
          "Next")))
    (elem :sh (r  1 1) :a :mid
      (elem +title+ :sh (r 1 1)
        "Bar Chart")
      (let [size   500
            gu     5]
        (elem :sh (r 1 1) :a :end
          (elem :sh (r 1 1)
            (elem :sv size
              (elem +field+ "100" :sh (r 1 1) :a :end)
                 ;;issues with y-axis label
              (elem +field+ "0" :sh (r 1 1) :a :end))
            (elem :sv size :sh (r 9 10) :g gu :av :end :b bd :bc :grey :ah :mid
              (let [num (cell= (/ (count cities) 2))]
                (for-tpl [[index [label value]] (cell= (map-indexed vector (partition 2 cities)))]
                  (elem :sh (cell= (r 1 num)) :g gu :av :beg
                    (elem :sh (r 1 1) :sv (t (cell= ((i/linear [0 100] [0 size]) value)) 800 i/cubic-out) :g gu :c :orange :ah :mid
                      (elem +label+ :s (r 1 1) :ah :mid value)
                      (elem +field+ :sh (r 1 1) :a :mid label))))))))))
   (elem +title+ :sh (r 1 1)
     "Sliders")
   (let [is     (cell #{})
         si->ci #(when % (inc (* 2 %)))
         cs     (cell= (get cities (si->ci (apply min (vec is)))) #(when (seq @is) (apply swap! cities assoc (interleave (map si->ci @is) (repeat (int %))))))]
     (elem :sh (r 1 1) :g g :a :mid
       (elem :sh (+ 400 32 g) :g g
          (elem :sh 400 :ah :mid :g 40
            (for-tpl [[i [label value]] (cell= (map-indexed vector (partition 2 cities)))]
              (elem :s 32 :r 18 :c (cell= (if (is i) yellow grey)) :b 2 :bc grey :d (sdw 2 2 (rgb 0 0 0 (r 1 14)) 2 0 true) :m :pointer :click #(swap! is (if (@is @i) disj conj) @i))))
         (slider  :s 400          :r 18 :c grey :src (cell= [cs cs] #(reset! cs (x %))))
         (vslider :sh 32  :sv 400 :r 18 :c grey :src cs)
         (hslider :sh 400 :sv 32  :r 18 :c grey :src cs))))
   (elem :sh (r 1 1) :sv 400)
   (elem +title+ :sh (r 1 1)
     "Color Pickers")
   (elem :sh (r 1 1) :g 100 :a :mid
     (slider :s 400 :r 18  :c (apply lgr 0 (map #(hsl % (r 360 360) (r 1 2)) (range 0 360 10))))
     (slider :s 400 :r 200 :c (apply lgr 0 (map #(hsl % (r 360 360) (r 1 2)) (range 0 360 10))) :src [50 50]))
   (elem :sh (r 1 1) :sv 400)))

(defn forms-view []
  (elem :sh (r 1 1) :p g
    (elem +title+ :sh (r 1 1)
      "Forms")
    (let [data (cell (or (:data sess) {}))]
      (cell= (prn :data data))
      (elem +label+ :s (r 1 1) :p (b 16 sm 50) :g 16 :ah :end
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Name"    :src (path= data [:name])    :autocomplete :given-name)
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Address" :src (path= data [:address]) :autocomplete :address-line1)
        (line  -field-  +field+ :sh (r 1 1)          :prompt "Email"   :src (path= data [:email])   :autocomplete :email)
        (lines -field-  +field+ :sh (r 1 1) :rows 10 :prompt "Message" :src (path= data [:message]))
        (files -button- +field+ :sh (r 1 1)          :prompt "Photo"   :src (path= data [:photo]) :types [:image/*])
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

(window :src route :scroll true :title "Hoplon UI"
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
