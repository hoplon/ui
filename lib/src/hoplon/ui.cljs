(ns hoplon.ui
  (:refer-clojure
    :exclude [/ - name load binding bound-fn])
  (:require
    [hoplon.core          :as h]
    [hoplon.svg           :as s]
    [hoplon.ui.validation :as v]
    [clojure.string  :refer [blank? join lower-case split ends-with?]]
    [cljs.reader     :refer [read-string]]
    [javelin.core    :refer [cell cell?]]
    [hoplon.ui.utils :refer [clean dash name]]
    [hoplon.ui.attrs :refer [/ - r ratio? calc? ->attr ->font ->dec font extrinsic explicit]]
    [hoplon.ui.elems :refer [box doc out mid in elem?]]
    [hoplon.binding]
    [cljsjs.markdown])
  (:require-macros
    [hoplon.core     :refer [with-timeout]]
    [hoplon.binding  :refer [binding bound-fn]]
    [hoplon.ui.utils :refer [bestow inherit]]
    [hoplon.ui       :refer [set-in! bind-in!]]
    [javelin.core    :refer [cell= with-let set-cell!=]]))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:inherit +fonts+)
(def ^:inherit +clicks+)
(def ^:inherit +pointer+)
(def ^:inherit +state+)

(def empty-icon-url  "data:;base64,iVBORw0KGgo=")
(def empty-image-url "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==")

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn route->hash [[path & [qmap]]]
  "transforms a urlstate of the form [[\"foo\" \"bar\"] {:baz \"barf\"}]
   to hash string in the form \"foo/bar&baz=barf\""
  (let [pair (fn [[k v]] (str (name k) "=" (pr-str v)))
        pstr (when (not-empty path) (apply str "/" (interpose "/" (map name path))))
        qstr (when (not-empty qmap) (apply str "?" (interpose "&" (map pair qmap))))]
    (str pstr qstr)))

(defn hash->route [hash]
  "transforms a hash string to a urlstate of the form
   [[\"foo\" \"bar\"] {:baz \"barf\"}]"
  (let [[rstr qstr] (split (subs hash 1) #"\?")
        pair        #(let [[k v] (split % #"=" 2)] [(keyword k) (read-string v)])
        qmap        (->> (split qstr #"&") (map pair) (when (not-empty qstr)) (into {}))
        path        (->> (split rstr #"/") (remove empty?) (mapv keyword))]
    (vec [path qmap])))

(defn font-face [[id width weight slope] sources]
  (let [loc    #(str "local('" % "')")
        url    (fn [[f u]] (str "url('" u "') format('" (name f) "')"))
        src    (join "," (into (mapv loc (:system sources)) (mapv url (dissoc sources :system))))
        props  {"font-family"   id
                "font-stretch"  (name width)
                "font-weight"   (name weight)
                "font-style"    (name slope)
                "src"           src}]
    (str "@font-face{" (apply str (mapcat (fn [[k v]] (str k ":" v ";")) (clean props))) "}")))

(defn copy! [text]
  (let [foc (.-activeElement js/document)
        tgt (with-let [e (.createElement js/document "textarea")]
              (set! (.-value e) text))]
    (.appendChild (.-body js/document) tgt)
    (.focus tgt)
    (.setSelectionRange tgt 0 (.. tgt -value -length))
    (.execCommand js/document "copy")
    (.focus foc)
    (.removeChild (.-body js/document) tgt)))

(defn debounce [ms f]
  (let [id (atom nil)]
    (fn [& args]
      (js/clearTimeout @id)
      (reset! id (js/setTimeout #(apply f args) ms)))))

(def visibility->status
  "maps the visibility string to a status keyword"
  {"visible"   :foreground
   "hidden"    :background
   "prerender" :background
   "unloaded"  :terminated})

(defn bind-with! [f value]
  (if (cell? value) (f @(add-watch value (gensym) #(f %4))) (f value)))

(defn vstr [vs]
  (join " " (mapv ->attr vs)))

(defn swap-elems! [e f v] ;; todo: factor out
  (cond (cell?       e) (cell= (swap-elems! e f v))
        (sequential? e) (doseq [e e] (swap-elems! e f v)) ;;todo: handled with IElemValue if (hoplon.ui/elem?)
        (elem?       e) (f e v)))

;;; attribute middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *sx*)
(def ^:dynamic *sy*)

(defn parse-args [ctor]
  (fn [& args]
     (apply ctor (#'hoplon.core/parse-args args))))

(defn size [ctor] ; _e_xtent
  "set the size on the outer element when it is expressed as a ratio, and on the
   inner element when it is a length.

   since ratios are expressed in terms of the parent, they include the margin
   (implemented as the padding between the inner and outer elements). fixed
   lengths are set on the middle, however, to exclude the margin so that it will
   push out against the parent container instead of being subtracted from the
   size of the elem.  both the inner and middle elements are bound separately to
   accomodate cells that might return ratios, evals, and fixed sizes at
   different times, such as the cell returned by the breakpoints function.

   when the collective size of the elem's children is greater than an explicitly
   set size in the vertical orientation, a scrollbar will automatically appear.
   horizontal scrolling is disallowed due to the fact that the browser does not
   permit them to be set to auto and visible independently; setting oveflowX to
   auto in the horizontal will set it to auto in the vertical as well, even if
   it is explictly set to visible."
  (fn [{:keys [s sh sv sh- sh+ scroll] :as attrs} elems]
    {:pre [(v/lengths? s sh sv sh- sh+)]}
    (binding [*sx* (or sh s)
              *sy* (or sv s)]
    (with-let [e (ctor attrs elems)]
      ;; when ratio subtract (- size margin)
      (bind-in! e [out .-style .-width]     (cell= (extrinsic *sx*)))
      (bind-in! e [out .-style .-minWidth]  (cell= (extrinsic  sh-)))
      (bind-in! e [out .-style .-maxWidth]  (cell= (extrinsic  sh+)))
      (bind-in! e [out .-style .-height]    (cell= (extrinsic *sy*)))
      (bind-in! e [mid .-style .-width]     (cell= (explicit  *sx*)))
      (bind-in! e [mid .-style .-minWidth]  (cell= (explicit   sh-)))
      (bind-in! e [mid .-style .-maxWidth]  (cell= (explicit   sh+)))
      (bind-in! e [mid .-style .-height]    (cell= (explicit  *sy*)))
      (bind-in! e [mid .-style .-maxHeight] (cell= (explicit  *sy*))) ;; todo: review
      (bind-in! e [in  .-style .-overflowY] (cell= (when (and scroll *sy*) :auto))))))) ;; default likely breaks 100% height where a sibling overflows

(defn positionable [ctor]
  "place the children within the element via the margin and gutter properties.

   :m :mh :mv :ml :mr :mt :mb. margins add space between the body and the fill.
   - <ISize>
   - :initial
   - :inherit

   :g :gh :hv. gutters add space between the child elems.
   - <ISize>
   - :initial
   - :inherit

   margins are implemented by setting the padding property on the elem's inner
   element.  gutters are implemented by setting the padding on the outer element
   of each child and a negative margin on the inner element of the elem itself
   equal to the gutter minus the padding.  the outer padding property on the
   children creates an even gutter between them, while a negative inner margin
   on the elem itself offsets this padding to fencepost the children flush with
   the edges of the container when the size of the gutter is greater than the
   size of the margin."
  (fn [{:keys [p ph pv pl pr pt pb g gh gv] :as attrs} elems] ; p -> m
    {:pre [(v/lengths? p ph pv pl pr pt pb g gh gv)]}
    (let [gho (cell= (/ (or gh g 0) "2"))
          gvo (cell= (/ (or gv g 0) "2"))]
     (swap-elems! elems #(do (bind-in! % [out .-style .-paddingLeft]   %2)
                             (bind-in! % [out .-style .-paddingRight]  %2)) gho)
     (swap-elems! elems #(do (bind-in! % [out .-style .-paddingTop]    %2)
                             (bind-in! % [out .-style .-paddingBottom] %2)) gvo)
      (with-let [e (ctor attrs elems)]
        (bind-in! e [in .-style .-marginLeft]   (cell= (- (or pl ph p 0) gho)))
        (bind-in! e [in .-style .-marginRight]  (cell= (- (or pr ph p 0) gho)))
        (bind-in! e [in .-style .-marginTop]    (cell= (- (or pt pv p 0) gvo)))
        (bind-in! e [in .-style .-marginBottom] (cell= (- (or pb pv p 0) gvo)))))))

(defn distribute [ctor]
  "set the position of the children to absolute so that they will stack on top
   of each other.

   this implementation requires the element to be explicitly sized." ;; todo: consider sizing to widest and tallest children if intrinsically sized.
  (fn [{:keys [d] :as attrs} elems]
    {:pre [(v/distributions? d)]}
    (swap-elems! elems #(bind-in! %1 [out .-style .-position] %2) (cell= (when (= d :pile) "absolute")))
    (ctor attrs elems)))

(defn align [ctor]
  "add the following attibutes for aligning the elem's children:

   :a align horizontal and vertical.  accepts one of the following keywords:
   - :beg align top & left (default)
   - :mid align center & middle
   - :end align bottom & right

   :ah align horizontal.  accepts one of the following keywonds:
  - :beg align left (default)
  - :mid align center
  - :end align right
  - :jst align justified

  :av align vertical. accepts one of the following keywords:
  - :beg align top (default)
  - :mid align middle
  - :end align bottom

  implementation notes. align sets the text-align and vertical-align attributes
  on the elem and proxies the vertical-align attributes to the outermost
  element of  each child.  it also sets the vertical height of the inner
  element to auto when :av is set. the vertical alignment is proxied to the
  outer elements of the children so that, in addition to aligning the lines of
  children within the elem, the children are also aligned in the same manner
  within their respective lines.

  when ah is set to jst, a space is inserted between the children so that the
  justification algo will see each elem as a separate word.

  this implementation conflates the alignment of text runs with the positioning
  of elems, attemping to treat them as uniformly as possible; this is somewhat
  necessary due to ui's reliance upon text-positioning attributes for its
  layout."
  (fn [{:keys [a ah av] :as attrs} elems]
    {:pre [(v/aligns? a) (v/alignhs? ah) (v/alignvs? av)]}
    (let [pv (cell= ({:beg "0%"  :mid "50%"   :end "100%"}               (or av a) "0%"))
          ah (cell= ({:beg :left :mid :center :end :right :jst :justify} (or ah a) (or ah a)))
          av (cell= ({:beg :top  :mid :middle :end :bottom}              (or av a) (or av a)))]
      (swap-elems! elems #(bind-in! %1 [out .-style .-verticalAlign] %2) (cell= (or av :top)))
      (with-let [e (ctor attrs (if (= @ah :justify) (interpose " " elems) elems))]
        (bind-in! e [in  .-style .-height]        (cell= (if (or (= av :middle) (= av :bottom)) :auto "100%"))) ;; height is 100% only when based on size of children
        (bind-in! e [mid .-style .-textAlign]     ah)
        (bind-in! e [mid .-style .-verticalAlign] av)
        (when (= (-> e in .-style .-position) "absolute")
          (bind-in! e [in .-style .-top]       pv)
          (bind-in! e [in .-style .-transform] (cell= (str "translateY(-" pv ")"))))))))

(defn fillable [ctor] ; _f_ill
  "set the background color an the inner element."
  (fn [{:keys [c o v] :as attrs} elems]
    {:pre [(v/colors? c) (v/opacities? o)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-style .-background] c)
      (bind-in! e [mid .-style .-opacity]    (cell= (when (ratio? o) (str (->dec o)))))
      (bind-in! e [out .-style .-visibility] (cell= (when (and (contains? attrs :v) (not v)) :hidden))))))

(defn pointable [ctor] ; _p_ointer
  "make the elem interact with the pointer.";; todo: linking user select to cursor
  (fn [{:keys [m me ms click drop] :as attrs} elems]
    {:pre [(v/cursors? m) (v/events? me) (v/callbacks? click drop)]}
    (with-let [e (ctor attrs elems)]
      (let [ms (cell= (if ms :text :none))]
        (bind-in! e [mid .-style .-cursor]           m)
        (bind-in! e [out .-style .-pointerEvents]    me)
        (bind-in! e [in  .-style .-userSelect]       ms)
        (bind-in! e [in  .-style .-mozUserSelect]    ms)
        (bind-in! e [in  .-style .-msUserSelect]     ms)
        (bind-in! e [in  .-style .-webkitUserSelect] ms)
        (when click (.addEventListener (mid e) "click" click))
        (when drop  (.addEventListener (mid e) "drop"  drop))))))

(defn transform [ctor]
  "apply a taransformation on the outer element."
  (fn [{:keys [x xx xy xz xb xs] :as attrs} elems]
    {:pre [(v/transforms? x) (v/origins? xx xy xz) (v/boxes? xb) (v/styles? xs)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [out .-style .-transform]       x)
      (bind-in! e [out .-style .-transformOrigin] (cell= (vstr (vector xx xy xz)))) ;; todo: remove vstr
      (bind-in! e [out .-style .-transformBox]    xb)
      (bind-in! e [out .-style .-transformStyle]  xs))))

(defn round [ctor]
  "set the radius on the middle element."
  (fn [{:keys [r rtl rtr rbl rbr] :as attrs} elems]
    {:pre [(v/lengths? r rtl rtr rbl rbr)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-style .-borderTopLeftRadius]     (or rtl r))
      (bind-in! e [mid .-style .-borderTopRightRadius]    (or rtr r))
      (bind-in! e [mid .-style .-borderBottomLeftRadius]  (or rbl r))
      (bind-in! e [mid .-style .-borderBottomRightRadius] (or rbr r)))))

(defn shadow [ctor] ;; d -> s
  "set the shadows on the middle element."
  (fn [{:keys [d] :as attrs} elems]
    {:pre [(v/shadows? d)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-style .-boxShadow] d))))

(defn border [ctor] ;_s_troke
  "set the border on the elem's middle element.

  this adds space between the edges of the container and its children."
  (fn [{:keys [b bh bv bl br bt bb bc bch bcv bcl bcr bct bcb] :as attrs} elems]
    {:pre [(v/lengths? b bh bv bl br bt bb) (v/colors? bc bch bcv bcl bcr bct bcb)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-style .-borderLeftStyle]   (cell= (when (or bl bh b) "solid")))
      (bind-in! e [mid .-style .-borderRightStyle]  (cell= (when (or br bh b) "solid")))
      (bind-in! e [mid .-style .-borderTopStyle]    (cell= (when (or bt bv b) "solid")))
      (bind-in! e [mid .-style .-borderBottomStyle] (cell= (when (or bb bv b) "solid")))
      (bind-in! e [mid .-style .-borderLeftWidth]   (or bl bh b))
      (bind-in! e [mid .-style .-borderRightWidth]  (or br bh b))
      (bind-in! e [mid .-style .-borderTopWidth]    (or bt bv b))
      (bind-in! e [mid .-style .-borderBottomWidth] (or bb bv b))
      (bind-in! e [mid .-style .-borderLeftColor]   (or bcl bch bc "transparent"))
      (bind-in! e [mid .-style .-borderRightColor]  (or bcr bch bc "transparent"))
      (bind-in! e [mid .-style .-borderTopColor]    (or bct bcv bc "transparent"))
      (bind-in! e [mid .-style .-borderBottomColor] (or bcb bcv bc "transparent")))))

(defn text [ctor]
  "specify the appearance of the text through the following properties, which are
  inherited through the dom-tree.  a nil value will return any attribute to its
  default.

  t size.  height of the font's capital letters, which may be expressed through
   one of the following attribute types or keywords.
   - <Pixel>
   - <Ratio>
   - <Ems>
   - <Points>
   - :xx-small
   - :x-small
   - :small
   - :medium
   - :large
   - :x-large
   - :xx-large
   - :larger
   - :smaller

  tf font. a reference to a font attribute constructed from a list of typeface
  sources that will deterine the width, weight, and slope of the text. since
  these characteristics are determined by the source, another a separate font
  must be constructed to stretch, bolden, or italicize the text.

  tw space. the width of the additional space to be added between the letters.
   - <Pixel>
   - <Ratio>

  th line height. set the minimum height of line boxes within each element
  to one of the following types:
  - <Pixel>
  - <Ratio>

  tc color. change the color of the text when set to one of the following
  attribute types:
  - <HSLA> (hsla 180 (r 1 1) (r 1 2))
  - <RGBA> (rgba 0xCCCCCC)

  td decoration. decorate text when set to one of the following keywords:
  - :underline
  - :overline
  - :line-through
  - :blink

  ts shadow. add a shadow to the text when set to the following type:
  - <Shadow>

  tx transform capitalization. capitalize text as specified by one of the
   following keywords:
   - :capitalize  uppercase first letter of each word
   - :uppercase  all characters to uppercase
   - :lowercase  all characters to lowercase

   tk kerning. indicate how text should be kerned when set to one of the following
   keywords, otherwise defaults to auto:
   - :none
   - :normal

   tr rendering. specify how the browser should optimize text. defaults to auto
   unless set to one of the following:
   - :optimize-speed
   - :optimize-legibility
   - :geometric-precision

   tm smooth. specify what kind of antialiasing should be used; defaults to auto
   unless explicitly set to one of the following:
   - :antialiased
   - :subpixel-antialiased

   ta adjust. typically used to tweak the text size (t) so that it is depends on
   the height of its lowercase rather than capital letters. it is expressed as a
   ratio of the base text size.
   - <Ratio> eg. (elem :t 14 :ta (r 1 2) ...) => 7 px"
  (fn [{:keys [t tw th tf tc td ts tx tk tr tm ta] :as attrs} elems]
    {:pre [(v/sizes? t) (v/spacings? tw th) (v/fonts? tf) (v/colors? tc)
           (v/decorations? td) (v/shadows? ts) (v/capitalizes? tx)
           (v/kernings? tk) (v/renderings? tr) (v/smoothings? tm)
           (v/adjusts? ta)]}
    (with-let [e (ctor attrs elems)]
      (when tf
        (inherit (out e)
          (bind-with! (fn [v] (swap! +fonts+ (partial apply assoc-in) (->font v))) tf)))
      (bind-in! e [in .-style .-fontSize]               t)
      (bind-in! e [in .-style .-letterSpacing]          tw)
      (bind-in! e [in .-style .-lineHeight]             th)
      (bind-in! e [in .-style .-fontFamily]             tf)
      (bind-in! e [in .-style .-color]                  tc)
      (bind-in! e [in .-style .-textDecoration]         td)
      (bind-in! e [in .-style .-textShadow]             ts)
      (bind-in! e [in .-style .-textTransform]          tx)
      (bind-in! e [in .-style .-fontKerning]            tk)
      (bind-in! e [in .-style .-textRendering]          (cell= (dash (name tr))))
      (bind-in! e [in .-style .-webkitFontSmoothing]    tm)
      (bind-in! e [in .-style .-moz-osx-font-smoothing] (case tm :antialiased :greyscale :none :unset nil))
      (bind-in! e [in .-style .-fontSmooth]             (case tm :antialiased :always    :none :never nil))
      (bind-in! e [in .-style .-fontSizeAdjust]         ta))))

(defn listable [ctor]
  (fn [{:keys [l lp li] :as attrs} elems]
    #_{:pre (list-types? l) (positions? lp) (images? li)}
    (with-let [e (ctor (dissoc attrs :l :lp :li) elems)]
      (bind-in! e [mid .-style .-display]             (cell= (if l "list-item" "table-cell")))
      (bind-in! e [mid .-style .-listStyleType]        l)
      #_(bind-in! e [out .-style .-listStylePosition] lp)
      #_(bind-in! e [out .-style .-listStyleImage]    (cell= (str "url(" li ")"))))))

(defn destyle [ctor]
  "neutralize the default styling of the inner element.

  this allows native components to be styled freely using attributes mapped to
  the middle element."
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (bind-in! e [in .-style .-width]           (r 1 1)) ;; display block should force to 100%
      (bind-in! e [in .-style .-height]          (r 1 1))
      (bind-in! e [in .-style .-outline]         :none)
      (bind-in! e [in .-style .-backgroundColor] :transparent)
      (bind-in! e [in .-style .-borderStyle]     :none)
      (bind-in! e [in .-style .-textAlign]       :inherit)))) ;; cursor: pointer, :width: 100%

;;; form middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:inherit +data+)
(def ^:inherit +error+)
(def ^:inherit +submit+)

(defn formable [ctor]
  "set up a form context"
  (fn [{:keys [change submit] :as attrs} elems]
    (when change (cell= (change (clean +data+)))) ;; init *data* to value of form fields on render
     (with-let [e (ctor attrs elems)]
       (bestow [+data+   (cell {})
                +error+  (atom nil)
                +submit+ (atom nil)] (out e)
         (.addEventListener (in e) "keypress" #(when (= (.-which %) 13) (submit (clean @+data+))))))))

(defn fieldable [ctor]
  "set the values common to all form fields."
  (fn [{:keys [key val req autofocus] :as attrs} elems]
    (with-let [e (ctor attrs elems)]
      (inherit (out e)
        (let [save #(when +data+ (swap! +data+ assoc (read-string (.-name (in e))) (not-empty (.-value (in e)))))]
          (.addEventListener (in e) "change" save)
          (.addEventListener (in e) "keyup"  (if-let [deb (:debounce attrs)] (debounce deb save) save))
          (bind-in! e [in .-name]     (cell= (pr-str key)))
          (bind-in! e [in .-value]    val)
          (bind-in! e [in .-required] (cell= (when req :required)))
          (bind-in! e [in .-autofocus] autofocus))))))

(defn file-field [ctor]
  (fn [{:keys [accept] :as attrs} elems]
    ;{:pre []} ;accept [".jpg" ".png" "audio/*" "video/*" "image/*" "application/ogg"]
    (with-let [e (ctor attrs elems)]
      (inherit (out e)
        (let [i (.appendChild (mid e) (.createElement js/document "input"))]
          (bind-in! i [.-style .-position] "absolute")
          (bind-in! i [.-style .-left]     "0")
          (bind-in! i [.-style .-width]    "100%")
          (bind-in! i [.-style .-top]      "0")
          (bind-in! i [.-style .-bottom]   "0")
          (bind-in! i [.-style .-opacity]  "0")
          (bind-in! i [.-type]             "file")
          (bind-in! (mid e) [.-tabIndex]   "0")
          (.addEventListener i "change" (fn [_] (when +data+ (swap! +data+ assoc (read-string (.-name i)) (when (not-empty (.-value i)) {:name (.-value i) :data (.-name (.item (.-files i) 0))})))
                                                (when-let [v (not-empty (.-value i))] (set! (.-innerHTML (in e)) (last (split v #"\\")))))))))));"

(defn pick-field [ctor]
  (fn [{:keys [selection] :as attrs} elems]
    (with-let [e (ctor attrs elems)]
      #_(bind-in! e [in .-name] key))))

(defn picks-field [ctor]
  (fn [{:keys [selections] :as attrs} elems]
    (with-let [e (ctor attrs elems)]
      #_(bind-in! e [in .-name] key))))

(defn item-field [ctor]
  (fn [{:keys [val] :as attrs} elems]
    (with-let [e (ctor attrs elems)]
      (inherit (out e)
        (bind-in! e [in .-value] (cell= (pr-str val)))
        (.addEventListener (mid e) "mousedown" (fn [] (when (= +state+ :on) nil #_(reset! +selected+ val))))))))

(defn items-field [ctor]
  (fn [{:keys [val] :as attrs} elems]
    (with-let [e (ctor attrs elems)]
      (inherit (out e)
        (bind-in! e [in .-value] (cell= (pr-str val)))
        (.addEventListener (mid e) "mousedown" (fn [] (if (= +state+ :on) nil #_(reset! +selected+ val))))))))

(defn line-field [ctor]
  (fn [{:keys [rows cols autocomplete autocapitalize content prompt charsize charmin charmax resizable] :as attrs} elems]
    {:pre [(v/autocompletes? autocomplete) (v/autocapitalizes? autocapitalize) (v/contents? content) (v/integers? charsize charmin charmax)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [in .-style .-padding] "0")
      (bind-in! e [in .-rows]            (cell= (if rows (str rows) "1")))
      (bind-in! e [in .-style .-height]  (cell= (if rows nil "100%")))
      (bind-in! e [in .-cols]            (cell= (when cols (str cols))))
      (bind-in! e [in .-style .-width]   (cell= (if cols nil "100%")))
      (bind-in! e [in .-style .-resize]  (cell= (or resizable :none)))
      (bind-in! e [in .-type]            content)
      (bind-in! e [in .-placeholder]     prompt)
      (bind-in! e [in .-autocomplete]    autocomplete)
      (bind-in! e [in .-autocapitalize]  autocapitalize)

      ;(bind-in! e [in .-size]           charsize)
      (bind-in! e [in .-minlength]       charmin)
      (bind-in! e [in .-maxlength]       charmax))))

(defn lines-field [ctor]
  (fn [{:keys [rows cols autocomplete autocapitalize content prompt charsize charmin charmax resizable] :as attrs} elems]
    {:pre [(v/autocompletes? autocomplete) (v/autocapitalizes? autocapitalize) (v/contents? content) (v/integers? charsize charmin charmax)]}
    (with-let [e (ctor attrs elems)]
      (bind-in! e [in .-style .-padding] "0")
      (bind-in! e [in .-rows]            (cell= (if rows (str rows) "2")))
      (bind-in! e [in .-style .-height]  (cell= (if rows nil "100%")))
      (bind-in! e [in .-cols]            (cell= (when cols (str cols))))
      (bind-in! e [in .-style .-width]   (cell= (if cols nil "100%")))
      (bind-in! e [in .-style .-resize]  (cell= (or resizable :none)))
      (bind-in! e [in .-type]            content)
      (bind-in! e [in .-placeholder]     prompt)
      (bind-in! e [in .-autocomplete]    autocomplete)
      (bind-in! e [in .-autocapitalize]  autocapitalize)

      ;(bind-in! e [in .-size]           charsize)
      (bind-in! e [in .-minlength]       charmin)
      (bind-in! e [in .-maxlength]       charmax))))

(defn send-field [ctor]
  (fn [{label :label submit' :submit :as attrs} elems]
    {:pre []} ;; todo: validate
    (with-let [e (ctor attrs elems)]
      (inherit (out e)
        (.addEventListener (mid e) "click" (bound-fn [_] (or submit' +submit+) +data+))
        (bind-in! e [in .-type]  "button")
        (bind-in! e [in .-value] label)))))

;;; middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn underlay [ctor element-ctor]
  (fn [{:keys [fit] :as attrs} elems]
    {:pre [(v/fits? fit)]}
    (with-let [e (ctor attrs elems)]
      (let [u (.insertBefore (mid e) (element-ctor) (in e))
            f (some #{fit} #{:cover :contain})]
        (bind-in! u [.-style .-display]      :block)
        (bind-in! u [.-style .-position]     (cell= (if f :absolute :relative)))
        (bind-in! u [.-style .-left]         (cell= (when f                 "50%")))
        (bind-in! u [.-style .-top]          (cell= (when f                 "50%")))
        (bind-in! u [.-style .-width]        (cell= (when (not= fit :cover) "100%")))
        (bind-in! u [.-style .-height]       (cell= (when (= fit :fill)     "100%")))
        (bind-in! u [.-style .-minWidth]     (cell= (when (= fit :cover)    "100%")))
        (bind-in! u [.-style .-minHeight]    (cell= (when (= fit :cover)    "100%")))
        (bind-in! u [.-style .-transform]    (cell= (when f                 "translate(-50%,-50%)")))
        (bind-in! e [mid .-style .-overflow] (cell= (when (= fit :cover) :hidden)))
        (bind-in! e [in  .-style .-pointerEvents] "none")
        (bind-in! e [in  .-style .-position]      :absolute)
        (bind-in! e [in  .-style .-top]           0)
        (bind-in! e [in  .-style .-width]         "100%")))))

(defn frameable [ctor]
  (fn [{:keys [allow-fullscreen policies type url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-firstChild .-allowFullscreen] allow-fullscreen)
      (bind-in! e [mid .-firstChild .-sandbox]         (cell= (if policies (apply str (interpose " " (mapv name policies))) "allow-same-origin allow-scripts")))
      (bind-in! e [mid .-firstChild .-type]            type)
      (bind-in! e [mid .-firstChild .-src]             url))))

(defn imageable [ctor]
  (fn [{:keys [url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-firstChild .-src] url))))

(defn objectable [ctor]
  (fn [{:keys [cross-origin type url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-firstChild .-crossOrigin] cross-origin)
      (bind-in! e [mid .-firstChild .-type]        type)
      (bind-in! e [mid .-firstChild .-data]        url))))

(defn videoable [ctor]
  (fn [{:keys [autoplay controls loop muted poster url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor attrs elems)]
      (bind-in! e [mid .-firstChild .-autoplay] autoplay)
      (bind-in! e [mid .-firstChild .-controls] (cell= (when controls "controls")))
      (bind-in! e [mid .-firstChild .-loop]     loop)
      (bind-in! e [mid .-firstChild .-muted]    muted)
      (bind-in! e [mid .-firstChild .-poster]   poster)
      (bind-in! e [mid .-firstChild .-src]      url))))

(defn svgable [ctor]
  (fn [{:keys [view-box preserve-aspect-ratio] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor attrs elems)]
      (bind-in! e [in .-style .-width] "100%")
      (cell= (.setAttribute (in e)     "viewBox"             (when view-box (apply str (interpose " " view-box)))))
      (cell= (.setAttribute (in e)     "preserveAspectRatio" (or preserve-aspect-ratio "none"))))))

(defn interactive [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (bestow [+state+   (cell nil)
               +pointer+ (cell {:over 0 :down 0 :up 0 :out 0})] (in e)
        (.addEventListener (mid e) "mouseover" #(swap! +pointer+ update :over inc))
        (.addEventListener (mid e) "mousedown" #(swap! +pointer+ update :down inc))
        (.addEventListener (mid e) "mouseup"   #(swap! +pointer+ update :up   inc))
        (.addEventListener (mid e) "mouseout"  #(swap! +pointer+ assoc  :out (inc (:out @+pointer+)) :up (:down @+pointer+)))))))

(defn selectable [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (inherit e
        (let [switch #(if (odd? (:down %)) :on :off)]
          (set-cell!= +state+ (switch +pointer+)))))))

(defn toggleable [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (let [switch  #(if (odd? (:down %)) "on" "off")
            mouse   #(cond (not= (:over %) (:out %)) "over"
                           (not= (:down %) (:up  %)) "down"
                           :else                     "out")]
        (set-cell!= +state+ (keyword (str (mouse +pointer+) "-" (switch +pointer+))))))))

(defn windowable [ctor]
  ;; todo: finish mousechanged
  (fn [{:keys [icon language metadata route position scripts styles title initiated mousechanged positionchanged statuschanged routechanged scroll] :as attrs} elems]
    (let [get-hash   #(if (= (get js/location.hash 0) "#") (subs js/location.hash 1) js/location.hash)
          set-hash!  #(if (blank? %) (.replaceState js/history #js{} js/document.title ".") (set! js/location.hash %))
          get-route  (comp hash->route get-hash)
          set-route! (comp set-hash! route->hash)
          get-agent  #(-> js/window .-navigator)
          get-refer  #(-> js/window .-document .-referrer)
          get-status #(-> js/window .-document .-visibilityState visibility->status)]
      (with-let [e (ctor attrs elems)]
        (bind-in! e [out .-lang] (or language "en"))
        (bind-in! e [out .-style .-width]     "100%")
        (bind-in! e [out .-style .-height]    "100%")
        (bind-in! e [mid .-style .-width]     "100%")
        (bind-in! e [mid .-style .-margin]    "0")
        (bind-in! e [mid .-style .-fontSize]  "100%")
        (bind-in! e [out .-style .-overflow] (cell= (when-not scroll "hidden")))
        (when initiated
          (initiated (get-route) (get-status) (get-agent) (get-refer)))
        (when routechanged
          (.addEventListener js/window "hashchange"
            #(when-not (= (route->hash @route) (get-hash)) (routechanged (get-route)))))
        (when statuschanged
          (.addEventListener js/window "visibilitychange"
            #(statuschanged (get-status))))
        (let [position (cell nil)]
          (.addEventListener js/window "scroll"
            #(let [[x y :as new-position] (vector (.-scrollX js/window) (.-scrollY js/window))]
              (when positionchanged
                (when-not (= new-position position)
                  (positionchanged x y))))))
        (cell= (set-route! route))
        (cell= (when scroll (.scrollTo js/window 0 0)))
        (.addEventListener js/document "DOMContentLoaded"
          #(cell= (.scroll js/window (first position) (second position))))
        (h/head
          (h/html-meta :charset "utf-8")
          (h/html-meta :http-equiv "X-UA-Compatible" :content "IE=edge")
          (h/html-meta :name "viewport" :content "width=device-width, initial-scale=1")
          (for [m (if (map? metadata) (mapv (fn [[k v]] {:name k :content v}) metadata) metadata)]
            (h/html-meta (into {} (for [[k v] m] [k (name v)]))))
          (when title
            (h/title title))
          (h/link :rel "icon" :href (or icon empty-icon-url))
          (h/for-tpl [s styles]  (h/link :rel "stylesheet" :href s))
          (bestow [+fonts+ (cell {})] (out e)
            (h/for-tpl [[id family] +fonts+]
              (h/style (cell= (font-face [id] (get-in family [:normal :400 :regular])))))))))))

;;; element primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def leaf (comp shadow round border size listable text fillable transform pointable))
(def node (comp distribute align positionable leaf))

;;; element primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def elem    (-> h/div      box                                     node            parse-args))
(def cmpt    (-> h/div      box interactive              toggleable node            parse-args))
(def canvas  (-> h/div      box (underlay h/canvas)                 node            parse-args))
(def svg     (-> s/svg      box                          svgable    leaf            parse-args))
(def frame   (-> h/div      box (underlay h/iframe)      frameable  node            parse-args))
(def image   (-> h/div      box (underlay h/img)         imageable  node            parse-args))
(def object  (-> h/div      box (underlay h/html-object) objectable node            parse-args))
(def video   (-> h/div      box (underlay h/video)       videoable  node            parse-args))
(def window  (->            doc                                     node windowable parse-args))

(def form    (-> h/form     box         formable                    node            parse-args))
(def line    (-> h/input    box destyle fieldable   line-field      node            parse-args))
(def lines   (-> h/textarea box destyle fieldable   lines-field     node            parse-args))
(def pick    (-> h/div      box destyle fieldable   pick-field      node            parse-args))
(def picks   (-> h/div      box destyle fieldable   picks-field     node            parse-args))
(def item    (-> h/option   box destyle interactive selectable  item-field node     parse-args))
(def file    (-> h/div      box         fieldable   file-field      node            parse-args))
(def files   (-> h/div      box         fieldable   file-field      node            parse-args))
(def submit  (-> h/input    box destyle             send-field      node            parse-args))

;;; utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn b
  "breakpoints."
  [x & xs]
  (with-let [v (cell nil)]
    (let [[o vs] (case x :h ["width" xs] :v ["height" xs] ["width" (cons x xs)])]
      (doseq [[min val max] (partition 3 2 (concat [0] vs [999999]))]
        (let [query (.matchMedia js/window (str "(min-" o ": " min "px) and (max-" o ": " max "px)"))
              value! #(when (.-matches %) (set-cell!= v val))]
          (value! query)
          (.addListener query #(value! %)))))))

(defn s
  "states"
  ;; todo: transition between states
  [& kvs]
  (cell= ((apply hash-map kvs) +state+)))

;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sans-serif        (font :generic :sans-serif [:normal :400 :regular]))
(def sans-serif-italic (font :generic :sans-serif [:normal :400 :italic]))
(def sans-serif-strong (font :generic :sans-serif [:normal :700 :regular]))

(def ^:dynamic *mdattrs*
  {:markdown         {:ft sans-serif :gv 10 :ms :all}
   :header           {:sh (r 1 1)}
   :header-1         {:t 32 :ft sans-serif-strong}
   :header-2         {:t 24 :ts sans-serif-italic}
   :header-3         {:t 20}
   :header-4         {:t 16}
   :header-5         {:t 14}
   :header-6         {:t 13}
   :bullet-list      {:sh (r 1 1) :pl 32}
   :number-list      {:sh (r 1 1) :pl 32}
   :bullet-list-item {:sh (r 1 1) :l :disc}
   :number-list-item {:sh (r 1 1) :l :decimal}
   :paragraph        {:sh (r 1 1)}
   :code-block       {:sh (r 1 1) :b 1}
   :inline-code      {:sh (r 1 1)}
   :image            {:sh (r 1 1)}
   :line-break       {:sh (r 1 1)}
   :link             {:mr :pointer :tc :blue}
   :italic           {:ft sans-serif-italic} ;; todo: what if bold and italic?  support font families in font registry?
   :strong           {:ft sans-serif-strong}})

(def ^:dynamic *mdelems*
  {:image image})

(defn keywordize-tag [tagstr env]
  (case tagstr
    "markdown"  :markdown
    "header"     :header
    "bulletlist" :bullet-list
    "numberlist" :number-list
    "listitem"   (keyword (-> env last name (str "-item")))
    "para"       :paragraph
    "code_block" :code-block
    "inlinecode" :inline-code
    "img"        :image
    "linebreak"  :line-break
    "link"       :link
    "link-ref"   :link-ref
    "em"         :italic
    "strong"     :strong))

(defn keywordize-ann [tag [k v]]
  (case k
    "href"  :href
    "level" (keyword (str (name tag) "-" v))))

(defn get-tag-attrs [tag]
  (with-let [attrs (*mdattrs* tag {})]
    (when (empty? attrs)
      (.warn js/console "No attributes specified for" tag "markdown tag."))))

(defn get-ann-attrs [k v]
  (case k
    :href {:click #(.open js/window v "blank")}
    (with-let [attrs (*mdattrs* k {})]
       (when (empty? attrs)
          (.warn js/console "No attributes specified for" k "markdown attribute with value" v ".")))))

(defn emit [[tagstr head :as node] env]
  (let [[tag ann :as node] (update (if (map? head) node (apply vector tagstr {} (subvec node 1))) 0 keywordize-tag env)
        elem  (*mdelems* tag elem)
        attrs (into (get-tag-attrs tag) (mapv #(get-ann-attrs (keywordize-ann tag %) (second %)) ann))
        elems (mapv #(if (vector? %) (emit % (conj env tag)) %) node)]
    (apply elem attrs elems)))

(defn parse [mdstr]
  (js->clj (.parse js/markdown mdstr)))

(defn markdown [mdstr & [mdattrs mdelems]]
  (binding [*mdattrs* (or mdattrs *mdattrs*)
            *mdelems* (or mdelems *mdelems*)]
    (cell= (when mdstr (emit (parse mdstr) [])))))

;;; todos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offset (use outer css margin to move out of current position)
;; baseline-shift
;; background, url (str "url(" v ") no-repeat 50% 50% / cover")
;; update, previously implemented on do multimethod, to form middleware
;; throw proper ui exceptions with stack traces and attribute kv information
;; consider utility of introducing rtl positioning
