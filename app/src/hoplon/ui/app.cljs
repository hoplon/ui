(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [hoplon.ui.interpolators :as i]
    [hoplon.ui.app.content   :as c]
    [clojure.string    :refer [lower-case upper-case starts-with?]]
    [javelin.core      :refer [defc cell cell= cell-let dosync lens? alts!]]
    [hoplon.core       :refer [defelem for-tpl when-tpl if-tpl case-tpl]]
    [hoplon.ui         :refer [window pane fore line lines file files path line-path image video markdown b= t=]]
    [hoplon.ui.attrs   :refer [- r]]
    [hoplon.ui.colors  :refer [->hex hex rgb hsv hsl hsi hcl triadic]]
    [hoplon.ui.effects :refer [sdw lgr]]
    [hoplon.ui.fonts   :refer [font]]
    [hoplon.ui.utils   :refer [x y w h mouse down? lb clamp debounce prv nxt current with-ready xssoc-in]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path= [c path] (cell= (get-in c path) (partial swap! c xssoc-in path)))

(defn cache [c]
  (let [v (cell nil)
        a (alts! c v)]
    (cell= (first a) #(if (lens? c) (reset! c %) (reset! v %)))))

(defn tx= [c f]
  (let [val (cell nil)
        set #(dosync (when (not= % @c) (f %)) (reset! val nil))]
    (cell= (or val c) #(if (= % ::tx) (set val) (reset! val %)))))

(defn deb= [c f & [ms]]
  "debouncing transaction lens"
  (let [val (cell nil)
        set #(when (and (lens? c) (not= % @c)) (f %))
        deb (debounce (or ms 1000) set)
        deb #(do (deb %) (reset! val %))
        src (alts! val c)]
    (cell= (some identity src) #(if (= % ::tx) (set val) (deb %)))))

(defn commit! [cell]
  (reset! cell ::tx))

;;; models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce sess (cell {:state :colors :data {:spam true}}))

;;; derivations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def state (path= sess [:state]))
(def data  (path= sess [:data]))
(def route (cell= [[state]]  #(reset! state (ffirst %))))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b= (r 1 1) sm bks))

;-- sizes ---------------------------------------------------------------------;

(def bd   3)
(def rd   4)
(def g    16)
(def knob 16)

;-- colors --------------------------------------------------------------------;

(def black  0x1F1F1FFF)
(def grey   0xCCCCCCFF)
(def orange 0xE73624FF)
(def blue   0x009BFFFF)
(def yellow 0xF5841FFF)
(def white  0xFFFFFFFF)

;-- typography ----------------------------------------------------------------;

(def baloo       (font :truetype "baloo.ttf"       :generic :sans-serif))
(def inconsolata (font :opentype "inconsolata.otf" :generic :sans-serif))

(def +title+    {:t 26 :tf baloo       :tc black :tw 2})
(def +subtitle+ {:t 21 :tf baloo       :tc black :tw 2})
(def +menu+     {:t 18 :tf baloo       :tc white :tw 1.5})
(def +label+    {:t 16 :tf baloo       :tc white :tw 1})
(def +body+     {:t 16 :tf baloo       :tc black :tw 1 :ms :all})
(def +field+    {:t 18 :tf baloo       :tc black :tw 1})
(def +code+     {:t 18 :tf inconsolata :tc black :ms :all})

;-- attributes -----------------------------------------------------------------

(def -label-  {:mh 12 :mv 6})
(def -code-   (merge -label- {:r rd :fc grey :s 1 :sc black}))
(def -input-  {:r rd :fc grey :s bd :sc black})
(def -button- (merge -label- -input- {:a :mid :p :pointer}))
(def -field-  (merge -label- -input-))

(def mdattrs
  {:markdown         +body+
   :header           {:eh (r 1 1)}
   :header-1         +title+
   :header-2         +subtitle+
   :header-3         {:t 20}
   :header-4         {:t 16}
   :header-5         {:t 14}
   :header-6         {:t 13}
   :bullet-list      {:eh (r 1 1) :ml 32}
   :number-list      {:eh (r 1 1) :ml 32}
   :bullet-list-item {:eh (r 1 1) :l :disc}
   :number-list-item {:eh (r 1 1) :l :decimal}
   :paragraph        +body+ #_{:eh (r 1 1)}
   :code-block       {:eh (r 1 1) :s 1}
   :inline-code      (merge -code- +code+)
   :image            {:eh (r 1 1)}
   :line-break       {:eh (r 1 1)}
   :link             {:mr :pointer :tc orange}})
   ;:italic           {:tf sans-serif-italic}
   ;:strong           {:tf sans-serif-strong}})

;;; content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def menu-items
  [:colors      "colors"
   :forms       "forms"
   :transitions "transitions"
   :scales      "scales"
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

(defelem slider [{:keys [eh ev e src] r* :r :as attrs}]
  ;- change sizes to body after api refactoring where border becomes stroke
  ;- substract border from sizes / add border to body bh bv b
  ;- support different types of transitions
  ;- consider passing the knob as a child, how to proxy values to knob
  ;- switch from grab to grabbing mouse cursors during drag
  ;- consider tool tip with values
  ;- consider passing curried interpolator function
  ;- handle overflowing margins without overflow: none due to perf problem. new
  ;  box model may fix.
  (let [src    (deb= src #(reset! src %) 500)
        w      (cell= (or eh e))
        h      (cell= (or ev e))
        kd     (cell= (min 32 w h))
        kr     (cell= (/ kd 2))
        dx->rx (cell= (partial i/linear 0        100      0        (- w kd)))
        dy->ry (cell= (partial i/linear 0        100      (- h kd) 0))
        rx->dx (cell= (partial i/linear kr       (- w kr) 0        100))
        ry->dy (cell= (partial i/linear (- h kr) kr       0        100))
        pos    (cell= [(dx->rx (clamp (x src) 0 100)) (dy->ry (clamp (y src) 0 100))] #(reset! src [(clamp (@rx->dx (x %)) 0 100) (clamp (@ry->dy (y %)) 0 100)]))
        sdw    (sdw 2 2 0x12 2 0 true)]
    (pane :d (sdw :inset true) :r r* :p :pointer
      :ml   (t= (cell= (x pos)) 500 i/quadratic-out)
      :mt   (t= (cell= (y pos)) 500 i/quadratic-out)
      :overflowv :hidden
      :up   #(reset! pos (mouse %))
      :move #(when (down? %) (reset! pos (mouse %)))
      (dissoc attrs :src)
      (pane :e kd :r (cell= (or r* 16)) :fc yellow :s 2 :sc (assoc white :a -2.6) :d sdw :p :grab))))

(defelem hslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector src 0) #(reset! src (x %))) (dissoc attrs :src)))

(defelem vslider [{:keys [src] :as attrs}]
  (slider :src (cell= (vector 0 src) #(reset! src (y %))) (dissoc attrs :src)))

(defelem hswitch [{:keys [e eh ev b bl br src knob] r* :r :as attrs}]
  (let [src (deb= src #(reset! src %) 400)
        bw  (+ (or bl b) (or br b))
        w   (cell= (- (or eh e) bw))
        sw  (cell= (/ w 2))
        sdw (sdw 2 2 0x12 2 0)]
    (pane :d (assoc sdw :inset true) :r rd :p :pointer
      :ml   (t= (cell= (if-not src 0 sw)) 400 i/quadratic-out)
      :down #(swap! src not)
      (dissoc attrs :src)
      (pane :eh sw :ev (r 1 1) :fc black :a :mid :d sdw
        knob
        (if-tpl src "✔" "X")))))

(defelem triangle [{:keys [body fc dir] :as attrs}]
  (let [half (/ body 2)]
    (pane :e 0
      :sl  (case dir :l 0    :r body :t half :b half)
      :sr  (case dir :l body :r 0    :t half :b half)
      :st  (case dir :l half :r half :t 0    :b body)
      :sb  (case dir :l half :r half :t body :b 0)
      :scl (when (= dir :r) fc)
      :scr (when (= dir :l) fc)
      :sct (when (= dir :b) fc)
      :scb (when (= dir :t) fc)
      (dissoc attrs :fc :body :dir))))

(defelem popup-menu [{:keys [src items prompt popup] :as attrs}]
  (let [padding [:m :mh :mv :ml :mr :mt :pb]
        key (cache src)
        pop (cell false)]
    (pane :p :pointer :click #(swap! pop not) :down-off #(when @pop (reset! pop false)) (apply dissoc attrs :src popup padding)
      (pane :eh (- (r 1 1) 40) (select-keys attrs padding)
        (cell= (get items key prompt)))
      (pane :e 40 :a :mid :sl 3 :sc black
        (triangle :body 15 :fc black :dir :b))
      (fore :y 46 :eh (r 1 1) :rb rd :fc :white :s bd :st 0 :sc black :v pop popup
        (let [over? (cell false)]
          (pane +field+ :eh (r 1 1) :mh g :mv (/ g 2) prompt :p :pointer :down #(reset! key nil) :out #(reset! over? false) :over #(reset! over? true) :fc (cell= (if over? grey white))))
        (for-tpl [[k v] (cell= (if (sequential? items) (map-indexed vector items) items))]
          (let [over?    (cell false)
                selected? (cell= (= k key))]
            (pane +field+ :eh (r 1 1) :mh g :mv (/ g 2) :p :pointer :down (fn [] (reset! key @k) (reset! over? false)) :out #(reset! over? false) :over #(reset! over? true) :fc (cell= (cond selected? orange over? grey :else white))
              v)))))))

(defelem typeahead [{:keys [src items prompt popup] :as attrs}]
  (let [padding [:m :mh :mv :ml :mr :mt :pb]
        key   (cache src)
        input (cell nil)
        pop   (cell nil)
        pop   (cell= (or input pop) #(reset! pop %))]
    (pane :p :pointer :click #(swap! pop not) :down-off #(when @pop (reset! pop false)) (apply dissoc attrs :src popup padding)
      (line +field+ :eh (- (r 1 1) 40) :src (cell= (get items key input) #(reset! input %)) :prompt prompt (select-keys attrs padding))
      (pane :e 40 :a :mid :sl 3 :sc black
        (triangle :body 15 :fc black :dir :b))
      (fore :y 46 :eh (r 1 1) :rb rd :fc :white :s bd :st 0 :sc black :v pop popup
        (let [over? (cell false)]
          (pane +field+ :eh (r 1 1) :mh g :mv (/ g 2) prompt :p :pointer :down #(reset! key nil) :out #(reset! over? false) :over #(reset! over? true) :fc (cell= (if over? grey white))))
        (for-tpl [[k v] (cell= (filter (fn [[_ v]] (starts-with? (lower-case v) (lower-case (or input "")))) (if (sequential? items) (map-indexed vector items) items)))]
          (let [over?    (cell false)
                selected? (cell= (= k key))]
            (pane +field+ :eh (r 1 1) :mh g :mv (/ g 2) :p :pointer :down (fn [] (reset! key @k) (reset! over? false)) :out #(reset! over? false) :over #(reset! over? true) :fc (cell= (cond selected? orange over? grey :else white))
              v)))))))

(defelem scroll [{:keys [src prev next]} elems]
  (let [chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ#"
        vv  600
        cv  (/ vv (count chars))
        iv  40
        v   (cell= (* iv (count src)))
        num (cell= (/ vv iv))
        items (cell #queue[])]
    (pane :eh (r 1 1) :s 2 :sc black :ev vv
      (pane :eh (- (r 1 1) cv) :ev (r 1 1) :d :pile :scroll true
        (pane :e (r 1 1) :ev v)
        (pane :e (r 1 1) :pt
          (for-tpl [[k v] src]
            (pane :eh (r 1 1) :ev iv :sv 1 :sc grey :av :mid
              v))))
      (pane :eh cv :ev (r 1 1)
        (for [c chars]
          (pane :e cv :a :mid :s 1 :sc grey :p :pointer :click #(prn :clicked)
            c))))))

;;; scenes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def identical-colors
  ["hex" (hex "F00")
   "rgb" (rgb 255 0 0)
   "hsl" (hsl 360 1 0.5)
   "hsv" (hsv 360 1 1)
   "hsi" (hsi 360 1 1)
   "hcl" (hcl 360 1 0.299)

   "hex" (hex "0F0")
   "rgb" (rgb 0 255 0)
   "hsl" (hsl 120 1 0.5)
   "hsv" (hsv 120 1 1)
   "hsi" (hsi 120 1 1)
   "hcl" (hcl 120 0.5 0.293)

   "hex" (hex "00F")
   "rgb" (rgb 0 0 255)
   "hsl" (hsl 240 1 0.5)
   "hsv" (hsv 240 1 1)
   "hsi" (hsi 240 1 1)
   "hcl" (hcl 240 .504 0.242)])

(defn colors-scene []
  (pane :eh (r 1 1) :m (b= 16 sm 50) :g 32
    (pane +title+ :eh (r 1 1)
      "Cylindrical Color Systems")
    (for [[label model l] (partition 3 ["hsl" hsl 0.5 "hsv" hsv 1 "hsi" hsi 1 "hcl" hcl 1])]
      (pane :eh (b= (r 1 1) sm (r 1 2)) :g 2
        (pane +title+ :eh (r 1 1) :mh 10
          label)
        (for [value (range 0 360 15) :let [c (model value 0.7 l)]]
          (pane +label+ :eh (r 1 4) :ev 40 :a :mid :fc c :t 12 :tc (second (triadic c))
            (cell= (upper-case (->hex c)))))))
    (pane +title+ :eh (r 1 1)
      "Cubic Color System")
    (pane :eh (r 1 1) :g 4
      (for [rr (range 0 0xFF 32)]
        (pane :eh (r 1 4) :g 1
          (for [gg (range 0 0xFF 32) bb (range 0 0xFF 32) :let [color (rgb rr gg bb)]]
            (pane +label+ :eh (r 1 8) :ev 40 :a :mid :fc color)))))
    (pane +title+ :eh (r 1 1)
      "Identical Colors Across Systems")
    (pane :eh (r 1 1) :g 2
      (for [[label value] (partition 2 identical-colors)]
        (pane +label+ :eh (r 1 6) :m g :a :mid :fc value
            (cell= (str label "\n" (->hex value))))))))

(defn forms-scene []
  (pane :eh (r 1 1) :m (b= 16 sm 50) :g 16
    #_(markdown
      "#Scroll
       A scrolling component." mdattrs)
    #_(scroll :src c/states)
    (markdown
      "#Forms
       This form populates the map below." mdattrs)
    (pane +label+ :e (r 1 1) :g 16 :ah :end
      (line       -field-  +field+ :eh (r 1 1)           :prompt "Name"    :src (path= data [:name])    :autocomplete :given-name)
      (line       -field-  +field+ :eh (r 1 1)           :prompt "Address" :src (path= data [:address]) :autocomplete :address-line1)
      (typeahead  -field-  +field+ :eh (>sm (r 2 5))     :prompt "State"   :src (path= data [:state])   :items c/states :popup {:ev 400 :scroll true})
      (popup-menu -field-  +field+ :eh (>sm (r 2 5))     :prompt "Country" :src (path= data [:country]) :items c/countries)
      (line       -field-  +field+ :eh (>sm (r 1 5))     :prompt "Zip"     :src (path= data [:zip])     :autocomplete :postal-code)
      (line       -field-  +field+ :eh (>sm (r 1 2))     :prompt "Email"   :src (path= data [:email])   :autocomplete :email)
      (files      -button- +field+ :eh (>sm (r 1 2))     :prompt "Photo"   :src (path= data [:photo])   :types [:image/*])
      (lines      -field-  +field+ :eh (r 1 1) :rows 10  :prompt "Message" :src (path= data [:message]))
      (pane       -label-  +field+ :eh (b= (- (r 1 1) (+ 92 16)) sm nil) :ev 46 :av :mid
        "Spam Me")
      (hswitch    -input- :eh 92 :ev 46 :src (path= data [:spam]))
      (pane  -button- +field+ :eh (>sm 300) :click #(swap! sess assoc :data data)
        "Submit"))
    (pane -code- +code+ :eh (r 1 1)
      (cell= (str data)))))

(defn media-scene []
  (pane :eh (r 1 1)
    (pane :eh (>sm md) :m 50 :g 50 :a :mid
      (image :a :mid :s bd :sc black :src "http://placehold.ii/200x100"
        (pane "content"))
      (image :e 200 :a :mid :s bd :sc black :fit :fill :src "http://placehold.ii/200x100"
        (pane "filled"))
      (image :e 200 :m 20 :s bd :sc black :fit :cover :src "http://placehold.ii/200x100"
        (pane "covered"))
      (image :e 200 :a :mid :s bd :sc black :fit :contain :src "http://placehold.ii/200x100"
        (pane "contained"))
      (video :e 200 :a :mid :s bd :sc black :fit :fill :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (pane "filled"))
      (video :e 200 :a :mid :s bd :sc black :fit :cover :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (pane "covered"))
      (video :e 200 :a :mid :s bd :sc black :fit :contain :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (pane "contained")))))

(defn components-scene []
  (pane :eh (r 1 1) :m g :g g
    (pane +title+ :eh (r 1 1)
      "Carousel")
    (let [idx    (cell 0)
          images (partition 2 images)]
      (pane :e (r 1 1) :g g :a :mid
        (pane -button- :click #(swap! idx (partial prv (count images)))
          "Prev")
        (pane :eh 800 :ev 600 :d :pile
          (for [[idx* [label filename]] (map-indexed vector images)]
            (image :e (r 1 1) :o (cell= (r ~(t= (cell= (if (= idx* idx) 1 0)) 2000 i/linear) 1)) :src filename)))
        (pane -button- :click #(swap! idx (partial nxt (count images)))
          "Next")))
    (pane :eh (r  1 1) :a :mid
      (pane +title+ :eh (r 1 1)
        "Bar Chart")
      (let [size   500
            gu     5]
        (pane :eh (r 1 1) :a :end
          (pane :eh (r 1 1)
            (pane :ev size
              (pane +field+ "100" :eh (r 1 1) :a :end)
                 ;;issues with y-axis label
              (pane +field+ "0" :eh (r 1 1) :a :end))
            (pane :ev size :eh (r 9 10) :g gu :av :end :s bd :sc :grey :ah :mid
              (let [num (cell= (/ (count cities) 2))]
                (for-tpl [[index [label value]] (cell= (map-indexed vector (partition 2 cities)))]
                  (pane :eh (cell= (r 1 num)) :g gu :av :beg
                    (pane :eh (r 1 1) :ev (t= (cell= (i/linear 0 100 0 size value)) 800 i/cubic-out) :g gu :fc yellow :ah :mid
                      (pane +label+ :e (r 1 1) :ah :mid value)
                      (pane +field+ :eh (r 1 1) :a :mid label))))))))))
   (pane +title+ :eh (r 1 1)
     "Sliders")
   (let [is     (cell #{})
         si->ci #(when % (inc (* 2 %)))
         cs     (cell= (get cities (si->ci (apply min (vec is)))) #(when (seq @is) (apply swap! cities assoc (interleave (map si->ci @is) (repeat (int %))))))]
     (pane :eh (r 1 1) :g g :a :mid
       (pane :eh (+ 400 32 g) :g g
          (pane :eh 400 :ah :mid :g 40
            (for-tpl [[i [label value]] (cell= (map-indexed vector (partition 2 cities)))]
              (pane :e 32 :r 18 :fc (cell= (if (is i) yellow grey)) :s 2 :sc grey :d (sdw 2 2 0x12 2 0 true) :p :pointer :click #(swap! is (if (@is @i) disj conj) @i))))
         (slider  :e 400          :r 18 :fc grey :src (cell= [cs cs] #(reset! cs (x %))))
         (vslider :eh 32  :ev 400 :r 18 :fc grey :src cs)
         (hslider :eh 400 :ev 32  :r 18 :fc grey :src cs))))
   (pane :eh (r 1 1) :ev 400)
   (pane +title+ :eh (r 1 1)
     "Color Pickers")
   (pane :eh (r 1 1) :g 100 :a :mid
     (slider :e 400 :r 18  :fc (apply lgr 0 (map #(hsl % 1 0.5) (range 0 360 10))))
     (slider :e 400 :r 200 :fc (apply lgr 0 (map #(hsl % 1 0.5) (range 0 360 10))) :src [50 50]))
   (pane :eh (r 1 1) :ev 400)))

(defn scales-scene []
  (let [size 300 step 5 col  4
        bx (cell 30) by (cell 30) ex (cell 270) ey (cell 270)
        xs (cell= (range bx (+ ex step) step))]
    (pane :eh (+ (* (+ 300 g) col) (* g 2) (- g)) :m g :g g
      (for [[label v f] (partition 3 ["B \u2190" bx - "B \u2192" bx + "B \u2191"  by + "B \u2193" by - "E \u2190" ex - "E \u2192" ex + "E \u2191" ey + "E \u2193" ey -])]
        (pane  -button- +field+ :eh (>sm 142) :click #(swap! v  (fn [v] (f v step)))
          label))
      (for [[index [label interpolator]] (map-indexed vector (partition 2 transforms))
        :let [c  (hsl (* index 20) .5 .5) px (cell 0)]]
        (pane :eh size
          (pane +label+ :e (r 3 4) :ml 8 :ah :beg :tc c
            label)
          (pane +label+ :e (r 1 4) :mr 8 :ah :end :tc c :p :pointer :click #(reset! px (- @ex @bx))
            \u25b6)
          (pane :e (>sm 300) :s bd :sc grey :d :pile
            (path :e (r 1 1) :s size :k 4 :kc c :av :mid
              :src (cell= (interleave xs (mapv (partial interpolator bx ex (- by) (- ey)) xs))))
            (pane :e (r 1 1) :ml (cell= (- bx bd 2)) :ah :beg
              (pane :ev (r 1 1) :eh 2 :fc grey))
            (pane :e (r 1 1) :ml (cell= (- ex bd 2)) :ah :beg
              (pane :ev (r 1 1) :eh 2 :fc grey))
            (pane :e (r 1 1) :mb (cell= (- ey bd 2)) :av :end
              (pane :eh (r 1 1) :ev 2 :fc grey))
            (pane :e (r 1 1) :mb (cell= (- by bd 2)) :av :end
              (pane :eh (r 1 1) :ev 2 :fc grey)))
          (pane :eh (r 1 1) :ml bx :mr (cell= (- size ex)) :mt 8
            (pane :eh (t= px 2000 interpolator) :ev 4 :r (/ rd 2) :fc c)))))))

(defn transitions-scene []
  (let [size (cell 150)]
    (pane :eh (r 1 1) :m g :g g
      (for [[index [label interpolator]] (map-indexed vector (partition 2 transforms))]
        (pane :eh (r 1 1)
          (pane +label+ :eh (t= size 1000 interpolator) :ev 60 :m g :fc (hsl (* index 15) 0.6 0.5) :av :mid :p :pointer :click #(swap! size (partial + 150))
            label))))))

(window :src route :scroll true :title "Hoplon UI" :ah :mid
  (pane :eh (r 1 1) :ev (b= :auto sm 80) :av :mid :m g :g g :fc orange :st 4 :sc yellow
    (image :e 50 :p :pointer :click #(reset! state :home) :src "hoplon-logo.png")
    (pane :eh (>sm (- (r 1 1) (+ 60 g))) :g g :ah :end
      (for [[*state label] (partition 2 menu-items) :let [sel-bc (cell= (if (= *state state) white orange))]]
        (pane +menu+ :eh (>sm :auto) :mh (b= g sm nil) :sl (b= 3 sm nil) :sb (b= nil sm 3) :scl (b= sel-bc sm orange) :scb (b= orange sm sel-bc) :p :pointer :click #(reset! state *state)
          label))))
  (case-tpl state
    :forms       (forms-scene)
    :colors      (colors-scene)
    :media       (media-scene)
    :scales      (scales-scene)
    :transitions (transitions-scene)
    :components  (components-scene)))
