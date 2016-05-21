(ns hoplon.ui.elems
  (:require
    [clojure.string  :refer [join]]
    [javelin.core    :refer [cell?]]
    [hoplon.core     :refer [do-watch]]
    [hoplon.ui.value :refer [IDOM ev rt px kw hx ratio? model? hex? model]])
  (:require-macros
    [hoplon.ui.elems :refer [bind-in!]]
    [javelin.core    :refer [cell= with-let]]))

(declare elem?)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn throw-ui! [elem & msg]
  (let [error-msg (apply str "UI ERROR: " msg)]
    (when elem.style
      (set! (.. elem -title) error-msg)
      (set! (.. elem -style -border) "2px dotted red"))
    (println error-msg)))

(defn ->attr [v]
  (cond (number?  v) (model (px v))
        (model?   v) (model v)
        (string?  v) v
        (nil?     v) ""))

(defn vstr [vs]
  (join " " (map ->attr vs)))

(defn bind-with [f vs]
  (let [watch (fn [i v] (if (cell? v) @(add-watch v (gensym) #(f (assoc vs i %4))) v))]
    (f (map-indexed watch vs))))

(defn swap-elems! [e f & vs]
  (cond (cell?   e) (do-watch e #(apply f @e %&))
        (vector? e) (doseq [e e] (apply swap-elems! e f vs)) ;; loop?
        (elem?   e) (apply f e vs)
        (string? e) identity
        :else       (throw-ui! e "Invalid child of type " (type e) " with values " vs ".")))

(defn- child-vec
  [this]
  (let [x (.-childNodes this)
        l (.-length x)]
    (loop [i 0 ret (transient [])]
      (or (and (= i l) (persistent! ret))
          (recur (inc i) (conj! ret (.item x i)))))))

(defn- ->node [x]
  (cond (elem?   x) (model x)
        (string? x) (.createTextNode js/document x)
        (number? x) (.createTextNode js/document (str x)))) ;; :else throw-ui

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IBox
  (-out [e])
  (-mid [e])
  (-in  [e]))

(defprotocol IContain
  (-sync [e elems]))

(defprotocol IRender ;; outer, events bubble to outer
  (-attach [e])
  (-detach [e]))

;;; elem box model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Elem [root]
  IBox
  (-out [_]     root)
  (-mid [_] (-> root .-firstChild))
  (-in  [_] (-> root .-firstChild .-firstChild))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Elem: " (.-tagName (-out this)) " " (.-tagName (-mid this)) " "(.-tagName (-in this)) ">"))
  IDOM
  (-model [_] root)
  IContain
  (-sync [this elems]
    (let [new  (remove nil? (flatten elems))
          new? (set new)]
      (loop [[x & xs] new
             [k & ks :as elems] (child-vec (-in this))]
        (when (or x k)
          (recur xs (cond (= x k) ks
                          (not k) (with-let [ks ks]
                                    (if (cell? x)
                                      (do-watch x #(-sync this (->node %2)))
                                      (.appendChild (-in this) (->node x))))
                          (not x) (with-let [ks ks]
                                    (when-not (new? k)
                                        ;; remove handlers
                                        (.removeChild (-in this) (->node k))))
                          :else   (with-let [_ elems]
                                    (.insertBefore (-in this) (->node x) (->node k))))))))))

(defn observe [target callback]
  (doto (js/MutationObserver. callback)
        (.observe target #js{:childList true})))

(defn nest [tags]
  "construct a linked list of dom elements from a vector of html tags"
  (with-let [e (.createElement js/document (nth tags 0))]
    (when-let [tags (not-empty (subvec tags 1))]
      (.appendChild e (nest tags)))))

(defn mkelem [& tags]
  (fn [attrs elems]
    #_{:pre [(empty? attrs)]}
    (let [o #(.. % -style)
          m #(.. % -firstChild -style)
          i #(.. % -firstChild -firstChild -style)
          b (nest tags)]
      (set! (-> b o .-boxSizing)     "border-box")   ;; include border and padding in dimensions to prevent visual errors from shifting layout
      (set! (-> b o .-display)       "inline-table") ;; layout ltr ttb when width not 100% to support responsive design
      (set! (-> b o .-verticalAlign) "top")          ;; inline-block/table elems must be explititly told to align themselves to the top
      (set! (-> b o .-textAlign)     "initial")      ;; prevent inheritance of alignment from parent
      (set! (-> b m .-boxSizing)     "border-box")   ;; include border and padding in dimensions
      (set! (-> b m .-display)       "table-cell")   ;; cells in tables enable sane vertical alignment
      (set! (-> b m .-position)      "relative")     ;; for svg skin
      (set! (-> b m .-height)        "inherit")      ;; assume the height of the parent and proxy it to the inner div
      (set! (-> b i .-cursor)        "inherit")      ;; inherit mouse
      (set! (-> b i .-display)       "block")        ;; prevent white space from creeping in around inline elements
      (set! (-> b i .-position)      "relative")     ;; make positioned children adjust relative to container plus padding
      (set! (-> b i .-height)        "inherit")      ;; display block fills the width, but needs to be told to fill the height (unless vertical alignment is set)
      (with-let [e (Elem. b) #_(.cloneNode b true)]
        (doseq [[k v] attrs]
          (throw-ui! (-out e) "Attribute " k " with value " v " cannot be applied to element."))
        (-sync e elems)))))

(defn elem? [v] (instance? Elem v))

(defn out [e] (-out e))
(defn mid [e] (-mid e))
(defn in  [e] (-in  e))

;;; validations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn adjust? [v]
  (cond
    (cell?    v) (adjust? @(add-watch v (gensym) #(adjust? %4)))
    (keyword? v) (apply some #{v} adjusts globals)
    (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
    (nil?     v) true
    :else        false))

(defn alignh? [v]
  (cond
    (cell?    v) (alignh? @(add-watch v (gensym) #(alignh? %4)))
    (keyword? v) (some #{v} (apply conj haligns lengths globals))
    (number?  v) v
    (nil?     v) true
    :else        false))

(defn alignv? [v]
  (cond
    (cell?    v) (alignv? @(add-watch v (gensym) #(alignv? %4)))
    (keyword? v) (some #{v} (apply conj valigns lengths globals))
    (number?  v) v
    (nil?     v) true
    :else        false))

(defn color? [v]
  (cond
    (cell?    v) (color? @(add-watch v (gensym) #(color? %4)))
    (keyword? v) (apply some #{v} colors globals)
    (hex?     v) v
    (nil?     v) true
    :else        false))

(defn cursor? [v]
  (cond
    (cell?    v) (cursor? @(add-watch v (gensym) #(cursor? %4)))
    (keyword? v) (some #{v} (apply conj cursors globals))
    (nil?     v) true
    :else        false))

(defn family? [v] ;; todo: support other units
  (cond
    (cell?    v) (family? @(add-watch v (gensym) #(family? %4)))
    (keyword? v) (some #{v} (apply conj families globals))
    (string?  v) v
    (nil?     v) true
    :else        false))

(defn italic? [v]
  (cond
    (cell?    v) (italic? @(add-watch v (gensym) #(italic? %4)))
    (keyword? v) (some #{v} (apply conj styles globals))
    (nil?     v) true
    :else        false))

(defn kerning? [v]
  (cond
    (cell?    v) (kerning? @(add-watch v (gensym) #(kerning? %4)))
    (keyword? v) (some #{v} (apply conj kernings globals))
    (nil?     v) true
    :else        false))

(defn length? [v] ;; todo: lengths & ration/pcts are diff types
  (cond
    (cell?    v) (length? @(add-watch v (gensym) #(length? %4)))
    (keyword? v) (some #{v} (apply conj lengths globals))
    (ratio?   v) v
    (number?  v) v
    (nil?     v) true
    :else        false))

(defn opacity? [v]
  (cond
    (cell?    v) (opacity? @(add-watch v (gensym) #(opacity? %4)))
    (keyword? v) (some #{v} globals)
    (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
    (nil?     v) true
    :else        false))

(defn overflow? [v]
  (cond
    (cell?    v) (overflow? @(add-watch v (gensym) #(overflow? %4)))
    (keyword? v) (some #{v} (apply conj overflows globals))
    (nil?     v) true
    :else        false))

(defn size? [v] ;; todo: support other units
  (cond
    (cell?    v) (size? @(add-watch v (gensym) #(size? %4)))
    (keyword? v) (some #{v} (apply conj sizes globals))
    (ratio?   v) v
    (number?  v) v
    (nil?     v) true
    :else        false))

(defn spacing? [v]
  (cond
    (cell?    v) (spacing? @(add-watch v (gensym) #(spacing? %4)))
    (keyword? v) (some #{v} (apply conj spacings globals))
    (ratio?   v) v
    (number?  v) v
    (nil?     v) true
    :else        false))

(defn stretch? [v]
  (cond
    (cell?    v) (stretch? @(add-watch v (gensym) #(stretch? %4)))
    (keyword? v) (apply some #{v} stretches globals)
    (nil?     v) true
    :else        false))

(defn synthesis? [v]
  (cond
    (cell?    v) (synthesis? @(add-watch v (gensym) #(synthesis? %4)))
    (keyword? v) (apply some #{v} syntheses globals)
    (nil?     v) true
    :else        false))

(defn underline? [v]
  (cond
    (cell?    v) (cursor? @(add-watch v (gensym) #(cursor? %4)))
    (keyword? v) (some #{v} (apply conj decorations globals))
    (nil?     v) true
    :else        false))

(defn weight? [v]
  (cond
    (cell?    v) (weight? @(add-watch v (gensym) #(weight? %4)))
    (keyword? v) (some #{v} (apply conj weights globals))
    (integer? v) (and (>= v 100) (<= v 900) (= (mod v 100)))
    (nil?     v) true
    :else        false))

(defn colors? [& vs]
  (every? color? vs))

(defn lengths? [& vs]
  (every? length? vs))

;;; attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn align [ctor]
  "set the text-align and vertical-align attributes on the elem and proxy the
   vertical-align attribute to the outer element of each child.  set vertical
   height of the inner element to auto when the align vertical attribute is set.

  the vertical alignment is proxied to the outer elements of the children so
  that, in addition to aligning the lines of children within the elem, the
  children are also aligned in the same manner within their respective lines."
  (fn [{:keys [ah av] :as attrs} elems]
    {:pre [(alignh? ah) (alignv? av)]}
    (swap-elems! elems #(bind-in! %1 [out .-style .-verticalAlign] %2) av)
    (with-let [e (ctor (dissoc attrs :ah :av) elems)]
      (bind-in! e [in  .-style .-height]        (cell= (if av :auto :inherit))) ;; initial instead?
      (bind-in! e [mid .-style .-textAlign]     ah)
      (bind-in! e [mid .-style .-verticalAlign] av))))

(defn color [ctor]
  "set the background color an the inner element."
  (fn [{:keys [c o m] :as attrs} elems]
    {:pre [(color? c) (opacity? o) (cursor? m)]}
    (with-let [e (ctor (dissoc attrs :c :o :m) elems)]
      (bind-in! e [mid .-style .-backgroundColor] c)
      (bind-in! e [mid .-style .-opacity]         o)
      (bind-in! e [mid .-style .-cursor]          m))))

(defn overflow [ctor]
  "set the overflow style on the elem's middle element."
  (fn [{:keys [v vh vv] :as attrs} elems]
    {:pre [(overflow? v) (overflow? vh) (overflow? vv)]}
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
    {:pre [(lengths? d dh dv db ds) (color? dc)]}
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
      (let [box #(if (ratio? %) out mid)]
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
      (bind-in! e [mid .-style .-borderWidth] (or st sv s 0)    (or sr sh s 0)    (or sb sv s 0)    (or sl sh s 0))
      (bind-in! e [mid .-style .-borderColor] (or sct scv sc) (or scr sch sc) (or scb scv sc) (or scl sch sc))
      (bind-in! e [mid .-style .-borderStyle] :solid))))

(defn font [ctor]
  "set the font styles pertaining to the attribute"
  (fn [{:keys [f fw fh ft ff fc fu fi fk fa fs fy] :as attrs} elems]
    {:pre [(size? f) (spacing? fw) (spacing? fh) (weight? ft) (family? ff) (color? fc) (underline? fu) (italic? fi) (adjust? fa) (stretch? fs) (synthesis? fy)]}
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
      (bind-in! e [in .-style .-width]  (rt 1 1))
      (bind-in! e [in .-style .-height] (rt 1 1))
      (bind-in! e [in .-style .-outline] :none)
      (bind-in! e [in .-style .-backgroundColor] :transparent)
      (bind-in! e [in .-style .-borderStyle]     :none)
      (bind-in! e [in .-style .-textAlign]       :inherit)))) ;; cursor: pointer, :width: 100%

(defn error [ctor]
  "handle any errors corresponding to the component by highlighting the
   corresponding component."
  (fn [attrs elems]
    (let [e (atom nil)]
      (try (ctor attrs elems)
           (catch js/Object err
            ;  (bind-in! @e [out .-title msg] err)
            ;  (bind-in! @e [out .-style .-border] 2 :dotted :red)
             (.log js/console (str "caught exception" @e)))))))

(defn skin [ctor]
  "add an svg skin to the component."
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (let [skin (.createElementNS js/document "http://www.w3.org/2000/svg" "svg")]
        (set! (.. skin -style -position) "absolute")
        ; (set! (.. skin -style -left)  "0px")
        ; (set! (.. skin -right) "0px")
        (set! (.. skin -style -top)  "0")
        (set! (.. skin -style -left)  "0")
        (set! (.. skin -style -width)  "100%")
        (set! (.. skin -style -height) "100%")
        ; (set! (.. skin -viewBox)  "1 1 auto")
        (set! (.. skin -style -zIndex) "-1")
        (set! (.. skin -innerHTML) "<rect x='0' y='0' width='100%' height='100%' rx='10' ry='10' fill='#CCC' />")
        (.appendChild (mid e) skin)))))




(defn img [ctor]
  (fn [{:keys [url] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :url) elems)]
      (bind-in! e [in .-src] url))))

(defn parse-args [ctor]
  (fn [& args]
     (apply ctor (#'hoplon.core/parse-args args))))

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

;;; element primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def elem   (-> (mkelem "div" "div" "div")    color font overflow size pad stroke round shadow align space error parse-args))
(def button (-> (mkelem "div" "div" "button") destyle skin color font overflow size pad stroke round shadow align error parse-args))
(def image  (-> (mkelem "div" "div" "img")    color font overflow size pad stroke round shadow img error parse-args))
