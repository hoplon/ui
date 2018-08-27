(ns ^{:hoplon/page "index.html"} hoplon.ui.app
  (:refer-clojure
    :exclude [-])
  (:require
    [hoplon.ui.interpolators :as i]
    [hoplon.ui.app.content   :as c]
    [clojure.string    :refer [lower-case upper-case starts-with?]]
    [javelin.core      :refer [defc cell cell= cell-let dosync lens? alts!]]
    [hoplon.core       :refer [defelem for-tpl when-tpl if-tpl case-tpl]]
    [hoplon.ui         :refer [window view fore line lines file files path line-path image video markdown b= t=]]
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

(def -label-  {:ph 12 :pv 6})
(def -code-   (merge -label- {:r rd :c grey :b 1 :bc black}))
(def -input-  {:r rd :c grey :b bd :bc black})
(def -button- (merge -label- -input- {:a :mid :m :pointer}))
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
   :bullet-list      {:eh (r 1 1) :pl 32}
   :number-list      {:eh (r 1 1) :pl 32}
   :bullet-list-item {:eh (r 1 1) :l :disc}
   :number-list-item {:eh (r 1 1) :l :decimal}
   :paragraph        +body+ #_{:eh (r 1 1)}
   :code-block       {:eh (r 1 1) :b 1}
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
    (view :d (sdw :inset true) :r r* :m :pointer
      :pl   (t= (cell= (x pos)) 500 i/quadratic-out)
      :pt   (t= (cell= (y pos)) 500 i/quadratic-out)
      :overflowv :hidden
      :up   #(reset! pos (mouse %))
      :move #(when (down? %) (reset! pos (mouse %)))
      (dissoc attrs :src)
      (view :e kd :r (cell= (or r* 16)) :c yellow :b 2 :bc (assoc white :a -2.6) :d sdw :m :grab))))

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
    (view :d (assoc sdw :inset true) :r rd :m :pointer
      :pl   (t= (cell= (if-not src 0 sw)) 400 i/quadratic-out)
      :down #(swap! src not)
      (dissoc attrs :src)
      (view :eh sw :ev (r 1 1) :c black :a :mid :d sdw
        knob
        (if-tpl src "✔" "X")))))

(defelem triangle [{:keys [body c dir] :as attrs}]
  (let [half (/ body 2)]
    (view :e 0
      :bl  (case dir :l 0    :r body :t half :b half)
      :br  (case dir :l body :r 0    :t half :b half)
      :bt  (case dir :l half :r half :t 0    :b body)
      :bb  (case dir :l half :r half :t body :b 0)
      :bcl (when (= dir :r) c)
      :bcr (when (= dir :l) c)
      :bct (when (= dir :b) c)
      :bcb (when (= dir :t) c)
      (dissoc attrs :c :body :dir))))

(defelem popup-menu [{:keys [src items prompt popup] :as attrs}]
  (let [padding [:p :ph :pv :pl :pr :pt :pb]
        key (cache src)
        pop (cell false)]
    (view :m :pointer :click #(swap! pop not) :down-off #(when @pop (reset! pop false)) (apply dissoc attrs :src popup padding)
      (view :eh (- (r 1 1) 40) (select-keys attrs padding)
        (cell= (get items key prompt)))
      (view :e 40 :a :mid :bl 3 :bc black
        (triangle :body 15 :c black :dir :b))
      (fore :y 46 :eh (r 1 1) :rb rd :c :white :b bd :bt 0 :bc black :v pop popup
        (let [over? (cell false)]
          (view +field+ :eh (r 1 1) :ph g :pv (/ g 2) prompt :m :pointer :down #(reset! key nil) :out #(reset! over? false) :over #(reset! over? true) :c (cell= (if over? grey white))))
        (for-tpl [[k v] (cell= (if (sequential? items) (map-indexed vector items) items))]
          (let [over?    (cell false)
                selected? (cell= (= k key))]
            (view +field+ :eh (r 1 1) :ph g :pv (/ g 2) :m :pointer :down (fn [] (reset! key @k) (reset! over? false)) :out #(reset! over? false) :over #(reset! over? true) :c (cell= (cond selected? orange over? grey :else white))
              v)))))))

(defelem typeahead [{:keys [src items prompt popup] :as attrs}]
  (let [padding [:p :ph :pv :pl :pr :pt :pb]
        key   (cache src)
        input (cell nil)
        pop   (cell nil)
        pop   (cell= (or input pop) #(reset! pop %))]
    (view :m :pointer :click #(swap! pop not) :down-off #(when @pop (reset! pop false)) (apply dissoc attrs :src popup padding)
      (line +field+ :eh (- (r 1 1) 40) :src (cell= (get items key input) #(reset! input %)) :prompt prompt (select-keys attrs padding))
      (view :e 40 :a :mid :bl 3 :bc black
        (triangle :body 15 :c black :dir :b))
      (fore :y 46 :eh (r 1 1) :rb rd :c :white :b bd :bt 0 :bc black :v pop popup
        (let [over? (cell false)]
          (view +field+ :eh (r 1 1) :ph g :pv (/ g 2) prompt :m :pointer :down #(reset! key nil) :out #(reset! over? false) :over #(reset! over? true) :c (cell= (if over? grey white))))
        (for-tpl [[k v] (cell= (filter (fn [[_ v]] (starts-with? (lower-case v) (lower-case (or input "")))) (if (sequential? items) (map-indexed vector items) items)))]
          (let [over?    (cell false)
                selected? (cell= (= k key))]
            (view +field+ :eh (r 1 1) :ph g :pv (/ g 2) :m :pointer :down (fn [] (reset! key @k) (reset! over? false)) :out #(reset! over? false) :over #(reset! over? true) :c (cell= (cond selected? orange over? grey :else white))
              v)))))))

(defelem scroll [{:keys [src prev next]} elems]
  (let [chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ#"
        vv  600
        cv  (/ vv (count chars))
        iv  40
        v   (cell= (* iv (count src)))
        num (cell= (/ vv iv))
        items (cell #queue[])]
    (view :eh (r 1 1) :b 2 :bc black :ev vv
      (view :eh (- (r 1 1) cv) :ev (r 1 1) :d :pile :scroll true
        (view :e (r 1 1) :ev v)
        (view :e (r 1 1) :pt
          (for-tpl [[k v] src]
            (view :eh (r 1 1) :ev iv :bv 1 :bc grey :av :mid
              v))))
      (view :eh cv :ev (r 1 1)
        (for [c chars]
          (view :e cv :a :mid :b 1 :bc grey :m :pointer :click #(prn :clicked)
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
  (view :eh (r 1 1) :p (b= 16 sm 50) :g 32
    (view +title+ :eh (r 1 1)
      "Cylindrical Color Systems")
    (for [[label model l] (partition 3 ["hsl" hsl 0.5 "hsv" hsv 1 "hsi" hsi 1 "hcl" hcl 1])]
      (view :eh (b= (r 1 1) sm (r 1 2)) :g 2
        (view +title+ :eh (r 1 1) :ph 10
          label)
        (for [value (range 0 360 15) :let [c (model value 0.7 l)]]
          (view +label+ :eh (r 1 4) :ev 40 :a :mid :c c :t 12 :tc (second (triadic c))
            (cell= (upper-case (->hex c)))))))
    (view +title+ :eh (r 1 1)
      "Cubic Color System")
    (view :eh (r 1 1) :g 4
      (for [rr (range 0 0xFF 32)]
        (view :eh (r 1 4) :g 1
          (for [gg (range 0 0xFF 32) bb (range 0 0xFF 32) :let [color (rgb rr gg bb)]]
            (view +label+ :eh (r 1 8) :ev 40 :a :mid :c color)))))
    (view +title+ :eh (r 1 1)
      "Identical Colors Across Systems")
    (view :eh (r 1 1) :g 2
      (for [[label value] (partition 2 identical-colors)]
        (view +label+ :eh (r 1 6) :p g :a :mid :c value
            (cell= (str label "\n" (->hex value))))))))

(defn forms-scene []
  (view :eh (r 1 1) :p (b= 16 sm 50) :g 16
    #_(markdown
      "#Scroll
       A scrolling component." mdattrs)
    #_(scroll :src c/states)
    (markdown
      "#Forms
       This form populates the map below." mdattrs)
    (view +label+ :e (r 1 1) :g 16 :ah :end
      (line       -field-  +field+ :eh (r 1 1)           :prompt "Name"    :src (path= data [:name])    :autocomplete :given-name)
      (line       -field-  +field+ :eh (r 1 1)           :prompt "Address" :src (path= data [:address]) :autocomplete :address-line1)
      (typeahead  -field-  +field+ :eh (>sm (r 2 5))     :prompt "State"   :src (path= data [:state])   :items c/states :popup {:ev 400 :scroll true})
      (popup-menu -field-  +field+ :eh (>sm (r 2 5))     :prompt "Country" :src (path= data [:country]) :items c/countries)
      (line       -field-  +field+ :eh (>sm (r 1 5))     :prompt "Zip"     :src (path= data [:zip])     :autocomplete :postal-code)
      (line       -field-  +field+ :eh (>sm (r 1 2))     :prompt "Email"   :src (path= data [:email])   :autocomplete :email)
      (files      -button- +field+ :eh (>sm (r 1 2))     :prompt "Photo"   :src (path= data [:photo])   :types [:image/*])
      (lines      -field-  +field+ :eh (r 1 1) :rows 10  :prompt "Message" :src (path= data [:message]))
      (view       -label-  +field+ :eh (b= (- (r 1 1) (+ 92 16)) sm nil) :ev 46 :av :mid
        "Spam Me")
      (hswitch    -input- :eh 92 :ev 46 :src (path= data [:spam]))
      (view  -button- +field+ :eh (>sm 300) :click #(swap! sess assoc :data data)
        "Submit"))
    (view -code- +code+ :eh (r 1 1)
      (cell= (str data)))))

(defn media-scene []
  (view :eh (r 1 1)
    (view :eh (>sm md) :p 50 :g 50 :a :mid
      (image :a :mid :b bd :bc black :src "http://placehold.ii/200x100"
        (view "content"))
      (image :e 200 :a :mid :b bd :bc black :fit :fill :src "http://placehold.ii/200x100"
        (view "filled"))
      (image :e 200 :p 20 :b bd :bc black :fit :cover :src "http://placehold.ii/200x100"
        (view "covered"))
      (image :e 200 :a :mid :b bd :bc black :fit :contain :src "http://placehold.ii/200x100"
        (view "contained"))
      (video :e 200 :a :mid :b bd :bc black :fit :fill :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (view "filled"))
      (video :e 200 :a :mid :b bd :bc black :fit :cover :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (view "covered"))
      (video :e 200 :a :mid :b bd :bc black :fit :contain :autoplay :true :src "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (view "contained")))))

(defn components-scene []
  (view :eh (r 1 1) :p g :g g
    (view +title+ :eh (r 1 1)
      "Carousel")
    (let [idx    (cell 0)
          images (partition 2 images)]
      (view :e (r 1 1) :g g :a :mid
        (view -button- :click #(swap! idx (partial prv (count images)))
          "Prev")
        (view :eh 800 :ev 600 :d :pile
          (for [[idx* [label filename]] (map-indexed vector images)]
            (image :e (r 1 1) :o (cell= (r ~(t= (cell= (if (= idx* idx) 1 0)) 2000 i/linear) 1)) :src filename)))
        (view -button- :click #(swap! idx (partial nxt (count images)))
          "Next")))
    (view :eh (r  1 1) :a :mid
      (view +title+ :eh (r 1 1)
        "Bar Chart")
      (let [size   500
            gu     5]
        (view :eh (r 1 1) :a :end
          (view :eh (r 1 1)
            (view :ev size
              (view +field+ "100" :eh (r 1 1) :a :end)
                 ;;issues with y-axis label
              (view +field+ "0" :eh (r 1 1) :a :end))
            (view :ev size :eh (r 9 10) :g gu :av :end :b bd :bc :grey :ah :mid
              (let [num (cell= (/ (count cities) 2))]
                (for-tpl [[index [label value]] (cell= (map-indexed vector (partition 2 cities)))]
                  (view :eh (cell= (r 1 num)) :g gu :av :beg
                    (view :eh (r 1 1) :ev (t= (cell= (i/linear 0 100 0 size value)) 800 i/cubic-out) :g gu :c yellow :ah :mid
                      (view +label+ :e (r 1 1) :ah :mid value)
                      (view +field+ :eh (r 1 1) :a :mid label))))))))))
   (view +title+ :eh (r 1 1)
     "Sliders")
   (let [is     (cell #{})
         si->ci #(when % (inc (* 2 %)))
         cs     (cell= (get cities (si->ci (apply min (vec is)))) #(when (seq @is) (apply swap! cities assoc (interleave (map si->ci @is) (repeat (int %))))))]
     (view :eh (r 1 1) :g g :a :mid
       (view :eh (+ 400 32 g) :g g
          (view :eh 400 :ah :mid :g 40
            (for-tpl [[i [label value]] (cell= (map-indexed vector (partition 2 cities)))]
              (view :e 32 :r 18 :c (cell= (if (is i) yellow grey)) :b 2 :bc grey :d (sdw 2 2 0x12 2 0 true) :m :pointer :click #(swap! is (if (@is @i) disj conj) @i))))
         (slider  :e 400          :r 18 :c grey :src (cell= [cs cs] #(reset! cs (x %))))
         (vslider :eh 32  :ev 400 :r 18 :c grey :src cs)
         (hslider :eh 400 :ev 32  :r 18 :c grey :src cs))))
   (view :eh (r 1 1) :ev 400)
   (view +title+ :eh (r 1 1)
     "Color Pickers")
   (view :eh (r 1 1) :g 100 :a :mid
     (slider :e 400 :r 18  :c (apply lgr 0 (map #(hsl % 1 0.5) (range 0 360 10))))
     (slider :e 400 :r 200 :c (apply lgr 0 (map #(hsl % 1 0.5) (range 0 360 10))) :src [50 50]))
   (view :eh (r 1 1) :ev 400)))

(defn scales-scene []
  (let [size 300 step 5 col  4
        bx (cell 30) by (cell 30) ex (cell 270) ey (cell 270)
        xs (cell= (range bx (+ ex step) step))]
    (view :eh (+ (* (+ 300 g) col) (* g 2) (- g)) :p g :g g
      (for [[label v f] (partition 3 ["B \u2190" bx - "B \u2192" bx + "B \u2191"  by + "B \u2193" by - "E \u2190" ex - "E \u2192" ex + "E \u2191" ey + "E \u2193" ey -])]
        (view  -button- +field+ :eh (>sm 142) :click #(swap! v  (fn [v] (f v step)))
          label))
      (for [[index [label interpolator]] (map-indexed vector (partition 2 transforms))
        :let [c  (hsl (* index 20) .5 .5) px (cell 0)]]
        (view :eh size
          (view +label+ :e (r 3 4) :pl 8 :ah :beg :tc c
            label)
          (view +label+ :e (r 1 4) :pr 8 :ah :end :tc c :m :pointer :click #(reset! px (- @ex @bx))
            \u25b6)
          (view :e (>sm 300) :b bd :bc grey :d :pile
            (path :e (r 1 1) :b size :k 4 :kc c :av :mid
              :src (cell= (interleave xs (mapv (partial interpolator bx ex (- by) (- ey)) xs))))
            (view :e (r 1 1) :pl (cell= (- bx bd 2)) :ah :beg
              (view :ev (r 1 1) :eh 2 :c grey))
            (view :e (r 1 1) :pl (cell= (- ex bd 2)) :ah :beg
              (view :ev (r 1 1) :eh 2 :c grey))
            (view :e (r 1 1) :pb (cell= (- ey bd 2)) :av :end
              (view :eh (r 1 1) :ev 2 :c grey))
            (view :e (r 1 1) :pb (cell= (- by bd 2)) :av :end
              (view :eh (r 1 1) :ev 2 :c grey)))
          (view :eh (r 1 1) :pl bx :pr (cell= (- size ex)) :pt 8
            (view :eh (t= px 2000 interpolator) :ev 4 :r (/ rd 2) :c c)))))))

(defn transitions-scene []
  (let [size (cell 150)]
    (view :eh (r 1 1) :p g :g g
      (for [[index [label interpolator]] (map-indexed vector (partition 2 transforms))]
        (view :eh (r 1 1)
          (view +label+ :eh (t= size 1000 interpolator) :ev 60 :p g :c (hsl (* index 15) 0.6 0.5) :av :mid :m :pointer :click #(swap! size (partial + 150))
            label))))))

(window :src route :scroll true :title "Hoplon UI" :ah :mid
  (view :eh (r 1 1) :ev (b= :auto sm 80) :av :mid :p g :g g :c orange :bt 4 :bc yellow
    (image :e 50 :m :pointer :click #(reset! state :home) :src "hoplon-logo.png")
    (view :eh (>sm (- (r 1 1) (+ 60 g))) :g g :ah :end
      (for [[*state label] (partition 2 menu-items) :let [sel-bc (cell= (if (= *state state) white orange))]]
        (view +menu+ :eh (>sm :auto) :ph (b= g sm nil) :bl (b= 3 sm nil) :bb (b= nil sm 3) :bcl (b= sel-bc sm orange) :bcb (b= orange sm sel-bc) :m :pointer :click #(reset! state *state)
          label))))
  (case-tpl state
    :forms       (forms-scene)
    :colors      (colors-scene)
    :media       (media-scene)
    :scales      (scales-scene)
    :transitions (transitions-scene)
    :components  (components-scene)))
