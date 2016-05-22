(ns hoplon.ui.middlewares
  (:require
    [clojure.string  :refer [join]]
    [javelin.core    :refer [cell?]]
    [hoplon.ui.attrs :refer [rt hx ev bk ratio? hex? eval? break? ->attr attr?]] ;;todo: user attr? in validations?
    [hoplon.ui.elems :refer [out mid in bind-cells ->elem elem?]])
  (:require-macros
    [hoplon.ui.middlewares :refer [bind-in!]]
    [javelin.core          :refer [cell= with-let]]))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *exceptions* nil)

(def globals     [:initial :inherit])
(def adjusts     [:none])
(def haligns     [:left :right :center :justify])
(def valigns     [:top :middle :bottom :baseline :sub :super :text-top :text-bottom])
(def colors      [:transparent :antiquewhite :aqua :aquamarine :azure :beige
                  :bisque :black :blanchedalmond :blue :blueviolet :brown
                  :burlywood :cadetblue :chartreuse :chocolate :coral
                  :cornflowerblue :cornsilk :crimson :darkblue :darkcyan
                  :darkgoldenrod :darkgray :darkgreen :darkgrey :darkkhaki
                  :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred
                  :darksalmon :darkseagreen :darkslateblue :darkslategray
                  :darkslategrey :darkturquoise :darkviolet :deeppink
                  :deepskyblue :dimgray :dimgrey :dodgerblue :firebrick
                  :floralwhite :forestgreen :fuchsia :gainsboro :ghostwhite
                  :gold :goldenrod :gray :green :greenyellow :grey :honeydew
                  :hotpink :indianred :indigo :ivory :khaki :lavender
                  :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcoral
                  :lightcyan :lightgoldenrodyellow :lightgray :lightgreen
                  :lightgrey :lightpink :lightsalmon :lightseagreen
                  :lightskyblue :lightslategray :lightslategrey :lightsteelblue
                  :lightyellow :lime :limegreen :linen :maroon :mediumaquamarine
                  :mediumblue :mediumorchid :mediumpurple :mediumseagreen
                  :mediumslateblue :mediumspringgreen :mediumturquoise
                  :mediumvioletred :midnightblue :mintcream :mistyrose :moccasin
                  :navajowhite :navy :oldlace :olive :olivedrab :orangered
                  :orchid :palegoldenrod :palegreen :paleturquoise
                  :palevioletred :papayawhip :peachpuff :peru :pink :plum
                  :powderblue :purple :rebeccapurple :red :rosybrown :royalblue
                  :saddlebrown :salmon :sandybrown :seagreen :seashell :sienna
                  :silver :skyblue :slateblue :slategray :slategrey :snow
                  :springgreen :steelblue :tan :teal :thistle :tomato :turquoise
                  :violet :wheat :white :whitesmoke :yellow :yellowgreen])
(def cursors     [:alias :all-scroll :auto :cell :context-menu :col-resize :copy
                  :crosshair :default :e-resize :ew-resize :grab :grabbing :help
                  :move :n-resize :ne-resize :nesw-resize :ns-resize :nw-resize
                  :nwse-resize :no-drop :none :not-allowed :pointer :progress
                  :row-resize :s-resize :se-resize :sw-resize :text
                  :vertical-text :w-resize :wait :zoom-in :zoom-out])
(def decorations [:none :underline :overline :line-through])
(def families    [:serif :sans-serif :monospace :cursive :fantasy])
(def kernings    [:auto :normal :none])
(def lengths     [:auto])
(def sizes       [:xx-small :x-small :small :medium :large :x-large :xx-large :larger :smaller])
(def spacings    [:normal])
(def stretches   [:ultra-condensed :extra-condensed :condensed :semi-condensed :normal :semi-expanded :expanded :extra-expanded :ultra-expanded])
(def styles      [:normal :italic :oblique])
(def syntheses   [:none :weight :style :weight-style])
(def overflows   [:visible :hidden :scroll :auto])
(def weights     [:normal :bold :bolder :lighter])

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn throw-ui-exception [& msg]
  (swap! *exceptions* conj {:msg (apply str msg)}))

(defn vstr [vs]
  (join " " (map ->attr vs)))

(defn bind-with [f vs] ;;todo: consolidate with bind-cells
  (let [watch (fn [i v] (if (cell? v) @(add-watch v (gensym) #(f (assoc vs i %4))) v))]
    (f (map-indexed watch vs))))

(defn swap-elems! [e f & vs] ;; todo: factor out
  (cond (cell?   e) (add-watch e (gensym) #(apply f @e %&))  ;; todo: broken (do-watch e #(apply f @e %&))
        (vector? e) (doseq [e e] (apply swap-elems! e f vs)) ;;todo: handled by IElemValue if (hoplon.ui/elem?)
        (elem?   e) (apply f e vs)
        (string? e) identity ;;todo: string literals should construct their own elems
        :else       (throw-ui-exception "Invalid child of type " (type e) " with values " vs ".")))

(defn in?            [v & kwvecs]  (some #{v} (apply conj kwvecs)))

(defn validate-cells [validator]
  (fn [& vs]
    (doseq [v vs :let [valid? (bind-cells validator)]]
      (when-not (valid? v) ;
        (throw-ui-exception "Error validating attribute value " v ".")))
    true))

;;; validation fns ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adjust? [v]
  (cond (keyword? v) (in? v adjusts globals)
        (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
        (nil?     v) :initial
        :else        false))

(defn alignh? [v]
  (cond (keyword? v) (in? v haligns lengths globals)
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn alignv? [v]
  (cond (keyword? v) (in? v valigns lengths globals)
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn color? [v]
  (cond (keyword? v) (in? v colors globals)
        (hex?     v) v
        (nil?     v) :initial
        :else        false))

(defn cursor? [v]
  (cond (keyword? v) (in? v cursors globals)
        (nil?     v) :inital
        :else        false))

(defn family? [v]
  (cond (keyword? v) (in? v families globals)
        (string?  v) v
        (nil?     v) :initial
        :else        false))

(defn style? [v]
  (cond (keyword? v) (in? v styles globals)
        (nil?     v) :initial
        :else        false))

(defn kerning? [v]
  (cond (keyword? v) (in? v kernings globals)
        (nil?     v) :initial
        :else        false))

(defn length? [v]
  (cond (keyword? v) (in? v lengths globals)
        (eval?    v) v
        (ratio?   v) v
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn opacity? [v]
  (cond (keyword? v) (in? v globals)
        (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
        (nil?     v) :initial
        :else        false))

(defn overflow? [v]
  (cond (keyword? v) (in? v overflows globals)
        (nil?     v) :initial
        :else        false))

(defn size? [v] ;; todo: support other units
  (cond (keyword? v) (in? v sizes globals)
        (ratio?   v) v
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn spacing? [v]
  (cond (keyword? v) (in? v spacings globals)
        (ratio?   v) v
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn stretch? [v]
  (cond (keyword? v) (in? v stretches globals)
        (nil?     v) :initial
        :else        false))

(defn synthesis? [v]
  (cond (keyword? v) (in? v syntheses globals)
        (nil?     v) :initial
        :else        false))

(defn decoration? [v]
  (cond (keyword? v) (in? v decorations globals)
        (nil?     v) :initial
        :else        false))

(defn weight? [v]
  (cond (keyword? v) (in? weights globals)
        (integer? v) (and (>= v 100) (<= v 900) (= (mod v 100)))
        (nil?     v) :initial
        :else        false))

(def adjusts?     (validate-cells adjust?))
(def alignhs?     (validate-cells alignh?))
(def alignvs?     (validate-cells alignv?))
(def colors?      (validate-cells color?))
(def cursors?     (validate-cells cursor?))
(def decorations? (validate-cells decoration?))
(def families?    (validate-cells family?))
(def kernings?    (validate-cells kerning?))
(def lengths?     (validate-cells length?))
(def opacities?   (validate-cells opacity?))
(def overflows?   (validate-cells overflow?))
(def sizes?       (validate-cells size?))
(def spacings?    (validate-cells spacing?))
(def stretches?   (validate-cells stretch?))
(def styles?      (validate-cells style?))
(def syntheses?   (validate-cells synthesis?))
(def weights?     (validate-cells weight?))

;;; middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn align [ctor]
  "set the text-align and vertical-align attributes on the elem and proxy the
   vertical-align attribute to the outer element of each child.  set vertical
   height of the inner element to auto when the align vertical attribute is set.

  the vertical alignment is proxied to the outer elements of the children so
  that, in addition to aligning the lines of children within the elem, the
  children are also aligned in the same manner within their respective lines."
  (fn [{:keys [ah av] :as attrs} elems]
    {:pre [(alignhs? ah) (alignvs? av)]}
    (swap-elems! elems #(bind-in! %1 [out .-style .-verticalAlign] %2) av)
    (with-let [e (ctor (dissoc attrs :ah :av) elems)]
      (bind-in! e [in  .-style .-height]        (cell= (if av :auto :inherit))) ;; initial instead?
      (bind-in! e [mid .-style .-textAlign]     ah)
      (bind-in! e [mid .-style .-verticalAlign] av))))

(defn color [ctor]
  "set the background color an the inner element."
  (fn [{:keys [c o m] :as attrs} elems]
    {:pre [(colors? c) (opacities? o) (cursors? m)]}
    (with-let [e (ctor (dissoc attrs :c :o :m) elems)]
      (bind-in! e [mid .-style .-backgroundColor] c)
      (bind-in! e [mid .-style .-opacity]         o)
      (bind-in! e [mid .-style .-cursor]          m))))

(defn overflow [ctor]
  "set the overflow style on the elem's middle element."
  (fn [{:keys [v vh vv] :as attrs} elems]
    {:pre [(overflows? v vh vv)]}
    (with-let [e (ctor (dissoc attrs :v :vh :vv) elems)]
      (bind-in! e [in .-style .-overflow] (or vh v) (or vv v)))))

(defn pad [ctor]
  "set the padding on the elem's inner element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [p pl pr pt pb ph pv] :as attrs} elems]
    {:pre [(lengths? p pl pr pt pb ph pv)]}
    ;; todo: dissallow pct based paddings since tied to opposite dimension
    (with-let [e (ctor (dissoc attrs :p :pl :pr :pt :pb :ph :pv) elems)]
      (bind-in! e [in .-style .-padding] (or pt pv p) (or pr ph p) (or pb pv p) (or pl ph p)))))

(defn round [ctor]
  "set the radius on the middle element."
  (fn [{:keys [r rtl rtr rbl rbr] :as attrs} elems]
    {:pre [(lengths? r rtl rtr rbl rbr)]}
    (with-let [e (ctor (dissoc attrs :r :rtl :rtr :rbl :rbr) elems)]
      (bind-in! e [mid .-style .-borderRadius] (or rtl r) (or rtr r) (or rbr r) (or rbl r)))))

(defn shadow [ctor]
  "set the shadow on the middle element."
  (fn [{:keys [d dh dv db ds dc] :as attrs} elems]
    {:pre [(lengths? d dh dv db ds) (colors? dc)]}
    (with-let [e (ctor (dissoc attrs :d :dh :dv :db :ds :dc) elems)]
      (bind-in! e [mid .-style .-boxShadow] (or dh d) (or dv d) (or db 1) ds dc))))

(defn size [ctor]
  "set the size on the outer element when it is expressed as a ratio, and on the
   inner element when it is a length.

   since ratios are expressed in terms of the parent, they include the margin
   (implemented as the padding between the inner and outer elements). fixed
   lengths are set on the middle, however, to exclude the margin so that when a
   margin is added, it will push out against the parent container instead of
   being subtracted from the size of the elem."
  (fn [{:keys [w w- w+ h h- h+] :as attrs} elems]
    {:pre [(lengths? w w- w+ h h- h+)]}
    (with-let [e (ctor (dissoc attrs :w :w- :w+ :h :h- :h+) elems)]
      (let [box #(if (or (ratio? %) (eval? %)) out mid)]
        (bind-in! e [(box w)  .-style .-width]     w)
        (bind-in! e [(box w-) .-style .-minWidth]  w-)
        (bind-in! e [(box w+) .-style .-maxWidth]  w+)
        (bind-in! e [(box h)  .-style .-height]    h)
        (bind-in! e [(box h-) .-style .-minHeight] h-)
        (bind-in! e [(box h+) .-style .-maxHeight] h+)))))

(defn space [ctor]
  "set the padding on the outer element of each child and a negative margin on
   the inner element of the elem itself equal to the padding.

   outer padding on the children creates an even gutter between them, while the
   negative inner margin on the elem itself offsets this padding to fencepost
   the children flush with the edges of the container."
  (fn [{:keys [g gh gv] :as attrs} elems]
    {:pre [(lengths? g gh gv)]}
    (let [mh (cell= (/ (or gh g) 2))
          mv (cell= (/ (or gv g) 2))
          ph (cell= (- mh))
          pv (cell= (- mv))]
      (swap-elems! elems #(bind-in! % [out .-style .-padding] %2) mv mh mv mh)
      (with-let [e (ctor (dissoc attrs :g :gh :gv) elems)]
        (bind-in! e [in .-style .-margin] pv ph)))))

(defn stroke [ctor]
  "set the border on the elem's middle element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [s sl sr st sb sh sv sc scl scr sct scb sch scv] :as attrs} elems]
    {:pre [(lengths? s sl sr st sb sh sv) (colors? sc scl scr sct scb sch scv)]}
    (with-let [e (ctor (dissoc attrs :s :sl :sr :st :sb :sh :sv :sw :sc :scl :scr :sct :scb :sch :scv) elems)]
      (bind-in! e [mid .-style .-borderWidth] (or st sv s 0)  (or sr sh s 0)  (or sb sv s 0)  (or sl sh s 0))
      (bind-in! e [mid .-style .-borderColor] (or sct scv sc) (or scr sch sc) (or scb scv sc) (or scl sch sc))
      (bind-in! e [mid .-style .-borderStyle] :solid))))

(defn font [ctor]
  "set the font styles pertaining to the attribute"
  (fn [{:keys [f fw fh ft ff fc fu fi fk fa fs fy] :as attrs} elems]
    {:pre [(sizes? f) (spacings? fw fh) (weights? ft) (families? ff) (colors? fc) (decorations? fu) (styles? fi) (adjusts? fa) (stretches? fs) (syntheses? fy)]}
    (with-let [e (ctor (dissoc attrs :f :fw :fh :ft :ff :fc :fu :fi :fk :fa :fs :fy) elems)]
      (bind-in! e [in .-style .-fontSize]       f)
      (bind-in! e [in .-style .-letterSpacing]  fw)
      (bind-in! e [in .-style .-lineHeight]     fh)
      (bind-in! e [in .-style .-fontWeight]     ft)
      (bind-in! e [in .-style .-fontFamily]     ff)
      (bind-in! e [in .-style .-color]          fc)
      (bind-in! e [in .-style .-textDecoration] fu)
      (bind-in! e [in .-style .-fontStyle]      fi)
      (bind-in! e [in .-style .-fontKerning]    fk)
      (bind-in! e [in .-style .-fontSizeAdjust] fa)
      (bind-in! e [in .-style .-fontStretch]    fs)
      (bind-in! e [in .-style .-fontSynthesis]  fy))))

(defn destyle [ctor]
  "neutralize the default styling of the inner element.

  this allows native components to be styled freely using attributes mapped to
  the middle element."
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (bind-in! e [in .-style .-width]           (rt 1 1)) ;; display block should force to 100%
      (bind-in! e [in .-style .-height]          (rt 1 1))
      (bind-in! e [in .-style .-outline]         :none)
      (bind-in! e [in .-style .-backgroundColor] :transparent)
      (bind-in! e [in .-style .-borderStyle]     :none)
      (bind-in! e [in .-style .-textAlign]       :inherit)))) ;; cursor: pointer, :width: 100%

(defn handle-exception [ctor]
  "handle errors by highlighting the corresponding component"
  (fn [attrs elems]
    (binding [*exceptions* (atom [])]
      (with-let [e (ctor attrs elems)]
        (when (not-empty @*exceptions*)
          (doseq [{:keys [msg]} @*exceptions*]
            (.log js/console msg))
          (bind-in! e [out .-title] (join "\n" (mapv :msg @*exceptions*)))
          (bind-in! e [out .-style .-border] 3 :solid :red))))))

(defn skin [ctor]
  "add an svg skin to the component."
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (let [skin (.createElementNS js/document "http://www.w3.org/2000/svg" "svg")] ;; if skin then hide mid styles
        (set! (.. skin -style -position) "absolute")
        (set! (.. skin -style -top)      "0")
        (set! (.. skin -style -left)     "0")
        (set! (.. skin -style -width)    "100%")
        (set! (.. skin -style -height)   "100%")
        (set! (.. skin -style -zIndex)   "-1")
        (set! (.. skin -innerHTML)       "<rect x='0' y='0' width='100%' height='100%' rx='10' ry='10' fill='#CCC' />")
        (.appendChild (mid e) skin)))))

(defn img [ctor]
  (fn [{:keys [url] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :url) elems)]
      (bind-in! e [in .-src] url))))

(defn parse-args [ctor]
  (fn [& args]
     (apply ctor (#'hoplon.core/parse-args args))))

; (defn attrs? [attrs]
;   (doseq [[k v] attrs]
;     (throw-ui-exception "Attribute " k " with value " v " cannot be applied to element."))
;   true)
;
; (defn elems? [attrs]
;   (doseq [[k v] elems]
;     ()
;     (throw-ui-exception "Attribute " k " with value " v " cannot be applied to element.")
;     true))

;; todo:
;; margin
;; fixed
;; constraint (absolute positioning within parent)
;; offset     (use outer css margin to move out of current position)
;; todo, once clear cases become apparent
;; baseline-shift
;; background, url (str "url(" v ") no-repeat 50% 50% / cover")
;; user-select, selectable
;; :toggle as as mid-attr
;; update, previously implemented on do multimethod, to form middleware
