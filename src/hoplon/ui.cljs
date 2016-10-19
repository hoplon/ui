(ns hoplon.ui
  (:refer-clojure :exclude [binding bound-fn])
  (:require
    [hoplon.core :as h]
    [clojure.string  :refer [blank? join split ends-with?]]
    [cljs.reader     :refer [read-string]]
    [javelin.core    :refer [cell cell?]]
    [hoplon.ui.attrs :refer [r ratio? calc? ->attr]]
    [hoplon.ui.elems :refer [box doc out mid in elem? markdown?]]
    [hoplon.ui.validation :as v]
    [hoplon.binding])
  (:require-macros
    [hoplon.core    :refer [with-timeout]]
    [hoplon.binding :refer [binding bound-fn]]
    [hoplon.ui      :refer [bind-in!]]
    [javelin.core   :refer [cell= with-let set-cell!=]]))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *exceptions* nil)
(def ^:dynamic *position*   nil)
(def ^:dynamic *clicks*     nil)
(def ^:dynamic *pointer*    nil)
(def ^:dynamic *state*      nil)

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

(defn clean [map] (into {} (filter second map)))

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

(defn throw-ui-exception [& msg]
  (when *exceptions*
    (swap! *exceptions* conj {:msg (apply str msg)})
    (throw (js/Error (apply str msg)))))

(defn vstr [vs]
  (join " " (map ->attr vs)))

(defn bind-cells [f] ;; todo: loop recur
  (fn [& vs]
    (let [watch (fn [i v] (if (cell? v) @(add-watch v i #(apply f (assoc (vec vs) i %4))) v))
          watch (fn [i v] (if (coll? v) (into (empty v) (map-indexed watch v)) (watch i v)))]
      (apply f (map-indexed watch vs)))))

(defn bind-with [f vs] ;;todo: consolidate with bind-cells
  (let [watch (fn [i v] (if (cell? v) @(add-watch v (gensym) #(f (assoc vs i %4))) v))]
    (f (map-indexed watch vs))))

(defn swap-elems! [e f & vs] ;; todo: factor out
  (cond (cell?       e) (cell= (apply swap-elems! e f vs))
        (sequential? e) (doseq [e e] (apply swap-elems! e f vs)) ;;todo: handled with IElemValue if (hoplon.ui/elem?)
        (elem?       e) (apply f e vs)
        (markdown?   e) identity
        (string?     e) identity
        (nil?        e) identity
        (fn?         e) identity
        :else       (throw-ui-exception "Invalid child of type " (type e) " with values " vs ".")))

(defn validate [validator]
  (fn [& vs]
    (doseq [v vs]
      (when-not (validator v)
        (throw-ui-exception "Error validating attribute value " v ".")))
    true))

(defn validate-cells [validator message] ;; todo: refactor to include attribute key
  (fn [& vs]
    (doseq [v vs :let [valid? (bind-cells validator)]]
      (when-not (valid? v)
        (throw-ui-exception message " " v ".")))
    true))

(def adjusts?         (validate-cells v/adjust?         "Error validating attribute of type adjust with value"))
(def aligns?          (validate-cells v/align?          "Error validating attribute of type align with value"))
(def alignhs?         (validate-cells v/alignh?         "Error validating attribute of type alingh with value"))
(def alignvs?         (validate-cells v/alignv?         "Error validating attribute of type alignv with value"))
(def colors?          (validate-cells v/color?          "Error validating attribute of type color with value"))
(def cursors?         (validate-cells v/cursor?         "Error validating attribute of type cursor with value"))
(def decorations?     (validate-cells v/decoration?     "Error validating attribute of type decoration with value"))
(def families?        (validate-cells v/family?         "Error validating attribute of type family with value"))
(def fits?            (validate-cells v/fit?            "Error validating attribute of type fit with value"))
(def kernings?        (validate-cells v/kerning?        "Error validating attribute of type kerning with value"))
(def lengths?         (validate-cells v/length?         "Error validating attribute of type length with value"))
(def opacities?       (validate-cells v/opacity?        "Error validating attribute of type opacity with value"))
(def overflows?       (validate-cells v/overflow?       "Error validating attribute of type overflow with value"))
(def renderings?      (validate-cells v/rendering?      "Error validating attribute of type rendering with value"))
(def shadows?         (validate-cells v/shadow?         "Error validating attribute of type shadow with value"))
(def sizes?           (validate-cells v/size?           "Error validating attribute of type size with value"))
(def smoothings?      (validate-cells v/smoothing?      "Error validating attribute of type smoothing with value"))
(def spacings?        (validate-cells v/spacing?        "Error validating attribute of type spacing with value"))
(def stretches?       (validate-cells v/stretch?        "Error validating attribute of type stetch with value"))
(def styles?          (validate-cells v/style?          "Error validating attribute of type style with value"))
(def syntheses?       (validate-cells v/synthesis?      "Error validating attribute of type sythesis with value"))
(def transforms?      (validate-cells v/transform?      "Error validating attribute of type transformation with value"))
(def capitalizes?     (validate-cells v/capitalize?     "Error validating attribute of type capitalize with value"))
(def origins?         (validate-cells v/origin?         "Error validating attribute of type transformation origin with value"))
(def boxes?           (validate-cells v/box?            "Error validating attribute of type transformation box with value"))
(def txstyles?        (validate-cells v/txstyle?        "Error validating attribute of type transformation style with value"))
(def weights?         (validate-cells v/weight?         "Error validating attribute of type weight with value"))

(def autocompletes?   (validate-cells v/autocomplete?   "Error validating attribute of type autocomplete with value"))
(def autocapitalizes? (validate-cells v/autocapitalize? "Error validating attribute of type autocapitalize with value"))
(def integers?        (validate-cells v/integer?        "Error validating attribute of type integer with value"))
(def contents?        (validate-cells v/content?        "Error validating attribute of type char with value"))

(def callbacks?       (validate-cells v/callback?       "Error validating attribute of type callback with value"))
(def docks?           (validate-cells v/dock?           "Error validating attribute of type dock with value"))
(def attrs?           (validate-cells empty?            "Unhandled attribute with value"))

;;; attribute middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn size [ctor]
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
    {:pre [(lengths? s sh sv sh- sh+)]}
    (with-let [e (ctor (dissoc attrs :s :sh :sv :sh- :sh+ :scroll) elems)]
      (let [rel? #(or (ratio? %) (calc? %))
            rel  #(cell= (if (rel? %) % %2))
            fix  #(cell= (if (rel? %) %2 %))]
        (bind-in! e [out .-style .-width]     (rel (or sh s) nil))
        (bind-in! e [out .-style .-minWidth]  (rel sh- nil))
        (bind-in! e [out .-style .-maxWidth]  (rel sh+ nil))
        (bind-in! e [out .-style .-height]    (rel (or sv s) nil))
        (bind-in! e [mid .-style .-width]     (fix (or sh s) nil))
        (bind-in! e [mid .-style .-minWidth]  (fix sh- nil))
        (bind-in! e [mid .-style .-maxWidth]  (fix sh+ nil))
        (bind-in! e [mid .-style .-height]    (fix (or sv s) nil))
        (bind-in! e [mid .-style .-maxHeight] (fix (or sv s) nil))
        (bind-in! e [in  .-style .-overflowY] (cell= (when (and scroll (or sv s)) :auto))))))) ;; default likely breaks 100% height where a sibling overflows

(defn align [ctor]
  "set the text-align and vertical-align attributes on the elem and proxy the
   vertical-align attribute to the outer element of each child.  set vertical
   height of the inner element to auto when the align vertical attribute is set.

  the vertical alignment is proxied to the outer elements of the children so
  that, in addition to aligning the lines of children within the elem, the
  children are also aligned in the same manner within their respective lines."
  (fn [{:keys [a ah av] :as attrs} elems]
    {:pre [(aligns? a) (alignhs? ah) (alignvs? av)]}
    (let [pv (cell= ({:beg "0%"  :mid "50%"   :end "100%"}               (or av a) "0%"))
          ah (cell= ({:beg :left :mid :center :end :right :jst :justify} (or ah a) (or ah a)))
          av (cell= ({:beg :top  :mid :middle :end :bottom}              (or av a) (or av a)))]
      (swap-elems! elems #(bind-in! %1 [out .-style .-verticalAlign] %2) (cell= (or av :top)))
      (with-let [e (ctor (dissoc attrs :a :ah :av) elems)]
        (bind-in! e [in  .-style .-height]        (cell= (if av :auto "100%"))) ;; height is 100% only when based on size of children
        (bind-in! e [mid .-style .-textAlign]     ah)
        (bind-in! e [mid .-style .-verticalAlign] av)
        (when (= (-> e in .-style .-position) "absolute")
          (bind-in! e [in .-style .-top]       pv)
          (bind-in! e [in .-style .-transform] (cell= (str "translateY(-" pv ")"))))))))

(defn pad [ctor]
  "set the padding on the elem's inner element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [p ph pv pl pr pt pb] :as attrs} elems]
    {:pre [(lengths? p ph pv pl pr pt pb)]}
    ;; todo: dissallow pct based paddings since tied to opposite dimension
    (with-let [e (ctor (dissoc attrs :p :ph :pv :pl :pr :pt :pb) elems)]
      (bind-in! e [mid .-style .-padding] (or pt pv p 0) (or pr ph p 0) (or pb pv p 0) (or pl ph p 0)))))

(defn gutter [ctor]
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
        ;; todo: gutter between text nodes
      (swap-elems! elems #(bind-in! % [out .-style .-padding] %2 %3 %4 %5) mv mh mv mh)
      (with-let [e (ctor (dissoc attrs :g :gh :gv) elems)]
        (bind-in! e [in .-style .-margin] pv ph)))))

(defn nudge [ctor]
  "bump the position of an elem relative to its normal position in the layout.
   useful as a final tweak in cases where the correctly calculated position of
   an element may appear off visually.

   implemented by setting the margins on the elem's outer element."
  (fn [{:keys [nh nv] :as attrs} elems]
    {:pre [(lengths? nh nv)]}
    (with-let [e (ctor (dissoc attrs :nh :nv) elems)]
      (bind-in! e [out .-style .-margin] (or nv 0) (or (cell= (- nh)) 0) (or (cell= (- nv)) 0) (or nh 0)))))

(defn dock [ctor]
  "fix the element to the window."
  (fn [{:keys [xl xr xt xb] :as attrs} elems]
    {:pre [(docks? xl xr xt xb)]} ;; todo: warn about pct w, pct h
    (with-let [e (ctor (dissoc attrs :xl :xr :xt :xb) elems)]
      (bind-in! e [out .-style .-position] (cell= (if (or xl xr xt xb) :fixed :initial)))
      (bind-in! e [out .-style .-zIndex]   (cell= (if (or xl xr xt xb) "9999" :initial)))
      (bind-in! e [out .-style .-left]     (cell= (or xl nil)))
      (bind-in! e [out .-style .-right]    (cell= (or xr nil)))
      (bind-in! e [out .-style .-top]      (cell= (or xt nil)))
      (bind-in! e [out .-style .-bottom]   (cell= (or xb nil))))))

(defn color [ctor]
  "set the background color an the inner element."
  (fn [{:keys [c o m v l] :as attrs} elems]
    {:pre [(colors? c) (opacities? o) (cursors? m)]}
    ;; todo: linking user select to cursor
    (with-let [e (ctor (dissoc attrs :c :o :m :v :l) elems)]
      (let [l (cell= (if l :text :none))]
        (bind-in! e [mid .-style .-backgroundColor]  c)
        (bind-in! e [mid .-style .-opacity]          o)
        (bind-in! e [mid .-style .-cursor]           m)
        (bind-in! e [out .-style .-visibility]       (cell= (when (and (contains? attrs :v) (not v)) :hidden)))
        (bind-in! e [in  .-style .-userSelect]       l)
        (bind-in! e [in  .-style .-mozUserSelect]    l)
        (bind-in! e [in  .-style .-msUserSelect]     l)
        (bind-in! e [in  .-style .-webkitUserSelect] l)))))

(defn transform [ctor]
  "apply a taransformation on the outer element."
  (fn [{:keys [x xx xy xz xb xs] :as attrs} elems]
    {:pre [(transforms? x) (origins? xx xy xz) (boxes? xb) (txstyles? xs)]}
    (with-let [e (ctor (dissoc attrs :x :xx :xy :xz :xb :xs) elems)]
      (bind-in! e [out .-style .-transform]       x)
      (bind-in! e [out .-style .-transformOrigin] xx xy xz)
      (bind-in! e [out .-style .-transformBox]    xb)
      (bind-in! e [out .-style .-transformStyle]  xs))))

(defn round [ctor]
  "set the radius on the middle element."
  (fn [{:keys [r rtl rtr rbl rbr] :as attrs} elems]
    {:pre [(lengths? r rtl rtr rbl rbr)]}
    (with-let [e (ctor (dissoc attrs :r :rtl :rtr :rbl :rbr) elems)]
      (bind-in! e [mid .-style .-borderRadius] (or rtl r) (or rtr r) (or rbr r) (or rbl r)))))

(defn shadow [ctor]
  "set the shadows on the middle element."
  (fn [{:keys [d] :as attrs} elems]
    {:pre [(shadows? d)]}
    (with-let [e (ctor (dissoc attrs :d) elems)]
      (bind-in! e [mid .-style .-boxShadow] d))))

(defn border [ctor]
  "set the border on the elem's middle element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [b bh bv bl br bt bb bc bch bcv bcl bcr bct bcb] :as attrs} elems]
    {:pre [(lengths? b bh bv bl br bt bb) (colors? bc bch bcv bcl bcr bct bcb)]}
    (with-let [e (ctor (dissoc attrs :b :bh :bv :bl :br :bt :bb :bw :bc :bch :bcv :bcl :bcr :bct :bcb) elems)]
      (bind-in! e [mid .-style .-borderWidth] (or bt bv b 0)  (or br bh b 0)  (or bb bv b 0)  (or bl bh b 0))
      (bind-in! e [mid .-style .-borderColor] (or bct bcv bc "transparent") (or bcr bch bc "transparent") (or bcb bcv bc "transparent") (or bcl bch bc "transparent"))
      (bind-in! e [mid .-style .-borderStyle] :solid))))

(defn fontable [ctor]
    "- f  font size
     - ft font weight
     - fw letter spacing
     - fh line height
     - ff font family
     - fc font color
     - fu text decoration
     - fi font style
     - fk font kerning
     - fr text rendering
     - fa font size adjust
     - fm font smoothing
     - fx font transform
     - fz font stretch
     - fy font synthesis
     - fx font capitalize"
  (fn [{:keys [f fw fh ft ff fc fu fi fk fa fs fx fy fr fm] :as attrs} elems]
    {:pre [(sizes? f) (spacings? fw fh) (weights? ft) (families? ff) (colors? fc) (decorations? fu) (styles? fi) (adjusts? fa) (stretches? fs) (syntheses? fy) (renderings? fr) (smoothings? fm) (capitalizes? fx)]}
    (with-let [e (ctor (dissoc attrs :f :fw :fh :ft :ff :fc :fu :fi :fk :fa :fs :fx :fy :fr :fm) elems)]
      (bind-in! e [in .-style .-fontSize]               f)
      (bind-in! e [in .-style .-letterSpacing]          fw)
      (bind-in! e [in .-style .-lineHeight]             fh)
      (bind-in! e [in .-style .-fontWeight]             ft)
      (bind-in! e [in .-style .-fontFamily]             ff)
      (bind-in! e [in .-style .-color]                  fc)
      (bind-in! e [in .-style .-textDecoration]         fu)
      (bind-in! e [in .-style .-fontStyle]              fi)
      (bind-in! e [in .-style .-fontKerning]            fk)
      (bind-in! e [in .-style .-textRendering]          fr)
      (bind-in! e [in .-style .-fontSizeAdjust]         fa)
      (bind-in! e [in .-style .-webkitFontSmoothing]    fm)
      (bind-in! e [in .-style .-moz-osx-font-smoothing] (case fm :antialiased :greyscale :none :unset :initial))
      (bind-in! e [in .-style .-fontSmooth]             (case fm :antialiased :always    :none :never :initial))
      (bind-in! e [in .-style .-fontStretch]            fs)
      (bind-in! e [in .-style .-textTransform]          fx)
      (bind-in! e [in .-style .-fontSynthesis]          fy))))

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

(def ^:dynamic *data*   nil)
(def ^:dynamic *error*  nil)
(def ^:dynamic *submit* nil)

(defn formable [ctor]
  "set up a form context"
  (fn [{:keys [change submit] :as attrs} elems]
    (when change (cell= (change (clean *data*)))) ;; init *data* to value of form fields on render
     (with-let [e (ctor (dissoc attrs :change :submit) elems)]
       (.addEventListener (in e) "keypress" (bound-fn [e] (when (= (.-which e) 13) (submit (clean @*data*))))))))

(defn fieldable [ctor]
  "set the values common to all form fields."
  (fn [{:keys [key val req autofocus] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :key :val :req :autofocus :debounce) elems)]
      (let [save (bound-fn  [_]  (when *data*  (swap! *data* assoc  (read-string  (.-name  (in e)))  (not-empty  (.-value  (in e))))))]
        (.addEventListener (in e) "change" save)
        (.addEventListener (in e) "keyup"  (if-let [deb (:debounce attrs)] (debounce deb save) save))
        (bind-in! e [in .-name]     (cell= (pr-str key)))
        (bind-in! e [in .-value]    val)
        (bind-in! e [in .-required] (cell= (when req :required)))
        (bind-in! e [in .-autofocus] autofocus)))))

(defn file-field [ctor]
  (fn [{:keys [accept] :as attrs} elems]
    ;{:pre []} ;accept [".jpg" ".png" "audio/*" "video/*" "image/*" "application/ogg"]
    (with-let [e (ctor (dissoc attrs :accept) elems)]
      (let [i (.appendChild (mid e) (.createElement js/document "input"))]
        (bind-in! i [.-style .-position] "absolute")
        (bind-in! i [.-style .-left]     "0")
        (bind-in! i [.-style .-width]    "100%")
        (bind-in! i [.-style .-top]      "0")
        (bind-in! i [.-style .-bottom]   "0")
        (bind-in! i [.-style .-opacity]  "0")
        (bind-in! i [.-type]             "file")
        (bind-in! (mid e) [.-tabIndex]   "0")
        (.addEventListener i "change" (bound-fn [_] (when *data* (swap! *data* assoc (read-string (.-name i)) (when (not-empty (.-value i)) {:name (.-value i) :data (.-name (.item (.-files i) 0))})))
                                                    (when-let [v (not-empty (.-value i))] (set! (.-innerHTML (in e)) (last (split v #"\\"))))))))));"

(defn pick-field [ctor]
  (fn [{:keys [selection] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :selection) elems)]
      #_(bind-in! e [in .-name] key))))

(defn picks-field [ctor]
  (fn [{:keys [selections] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :selections) elems)]
      #_(bind-in! e [in .-name] key))))

(defn item-field [ctor]
  (fn [{:keys [val] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :val) elems)]
      (bind-in! e [in .-value] (cell= (pr-str val)))
      (.addEventListener (mid e) "mousedown" (bound-fn [] (when (= *state* :on) nil #_(reset! *selected* val)))))))

(defn items-field [ctor]
  (fn [{:keys [val] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :val) elems)]
      (bind-in! e [in .-value] (cell= (pr-str val)))
      (.addEventListener (mid e) "mousedown" (bound-fn [] (if (= *state* :on) nil #_(reset! *selected* val)))))))

(defn line-field [ctor]
  (fn [{:keys [rows cols autocomplete autocapitalize content prompt charsize charmin charmax resizable] :as attrs} elems]
    {:pre [(autocompletes? autocomplete) (autocapitalizes? autocapitalize) (contents? content) (integers? charsize charmin charmax)]}
    (with-let [e (ctor (dissoc attrs :rows :cols :autocomplete :autocapitalize :content :prompt :charsize :charmin :charmax :resizeable) elems)]
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
    {:pre [(autocompletes? autocomplete) (autocapitalizes? autocapitalize) (contents? content) (integers? charsize charmin charmax)]}
    (with-let [e (ctor (dissoc attrs :rows :cols :autocomplete :autocapitalize :content :prompt :charsize :charmin :charmax :resizeable) elems)]
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
    (with-let [e (ctor (dissoc attrs :label :submit) elems)]
      (.addEventListener (mid e) "click" (bound-fn [_] (or submit' *submit*) *data*))
      (bind-in! e [in .-type]  "button")
      (bind-in! e [in .-value] label))))

;;; middlewares ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exceptional [ctor]
  "handle errors by highlighting the corresponding component"
  (fn [attrs elems]
    (binding [*exceptions* (atom [])]
      (with-let [e (ctor attrs elems)]
        (when (not-empty @*exceptions*)
          (doseq [{:keys [msg]} @*exceptions*]
            (.log js/console msg))
          (bind-in! e [out .-title] (join "\n" (mapv :msg @*exceptions*)))
          (bind-in! e [out .-style .-border] 3 :solid :red))))))

(defn assert-noattrs [ctor]
  (fn [attrs elems]
    {:pre [(attrs? attrs)]}
    (ctor attrs elems)))

(defn underlay [ctor element-ctor]
  (fn [{:keys [fit] :as attrs} elems]
    {:pre [(fits? fit)]}
    (with-let [e (ctor (dissoc attrs :fit) elems)]
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
        (bind-in! e [in  .-style .-position] :absolute)
        (bind-in! e [in  .-style .-top]      0)
        (bind-in! e [in  .-style .-width]    "100%")))))

(defn frameable [ctor]
  (fn [{:keys [allow-fullscreen sandbox type url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor (dissoc attrs :allow-fullscreen :sandbox :type :url) elems)]
      (bind-in! e [mid .-firstChild .-allowFullscreen] allow-fullscreen)
      (bind-in! e [mid .-firstChild .-sandbox]         sandbox)
      (bind-in! e [mid .-firstChild .-type]            type)
      (bind-in! e [mid .-firstChild .-src]             url))))

(defn imageable [ctor]
  (fn [{:keys [url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor (dissoc attrs :url) elems)]
      (bind-in! e [mid .-firstChild .-src] url))))

(defn objectable [ctor]
  (fn [{:keys [cross-origin type url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor (dissoc attrs :cross-origin :type :url) elems)]
      (bind-in! e [mid .-firstChild .-crossOrigin] cross-origin)
      (bind-in! e [mid .-firstChild .-type]        type)
      (bind-in! e [mid .-firstChild .-data]        url))))

(defn videoable [ctor]
  (fn [{:keys [autoplay controls url] :as attrs} elems]
    {:pre []} ;; todo
    (with-let [e (ctor (dissoc attrs :autoplay :controls :url) elems)]
      (bind-in! e [mid .-firstChild .-autoplay] autoplay)
      (bind-in! e [mid .-firstChild .-controls] (cell= (when controls "controls")))
      (bind-in! e [mid .-firstChild .-src]      url))))

(defn clickable [ctor] 
  (fn [{:keys [click] :as attrs} elems]
    {:pre [(callbacks? click)]}
    (with-let [e (ctor (dissoc attrs :click) elems)]
      (when click
        (.addEventListener (mid e) "click" click)))))

(defn parse-args [ctor]
  (fn [& args]
     (apply ctor (#'hoplon.core/parse-args args))))

(defn interactive [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (.addEventListener (mid e) "mouseover" (bound-fn [] (swap! *pointer* update :over inc)))
      (.addEventListener (mid e) "mousedown" (bound-fn [] (swap! *pointer* update :down inc)))
      (.addEventListener (mid e) "mouseup"   (bound-fn [] (swap! *pointer* update :up   inc)))
      (.addEventListener (mid e) "mouseout"  (bound-fn [] (swap! *pointer* assoc  :out (inc (:out @*pointer*)) :up (:down @*pointer*)))))))

(defn selectable [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (let [switch #(if (odd? (:down %)) :on :off)]
        (set-cell!= *state* (switch *pointer*))))))

(defn toggleable [ctor]
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (let [switch  #(if (odd? (:down %)) "on" "off")
            mouse   #(cond (not= (:over %) (:out %)) "over"
                           (not= (:down %) (:up  %)) "down"
                           :else                     "out")]
        (set-cell!= *state* (keyword (str (mouse *pointer*) "-" (switch *pointer*))))))))

(defn windowable [ctor]
  ;; todo: finish mousechanged
  (fn [{:keys [fonts icon language metadata route position scripts styles title initiated mousechanged positionchanged statuschanged routechanged scroll] :as attrs} elems]
    (let [get-hash   #(if (= (get js/location.hash 0) "#") (subs js/location.hash 1) js/location.hash)
          set-hash!  #(if (blank? %) (.replaceState js/history #js{} js/document.title ".") (set! js/location.hash %))
          get-route  (comp hash->route get-hash)
          set-route! (comp set-hash! route->hash)
          get-agent  #(-> js/window .-navigator)
          get-refer  #(-> js/window .-document .-referrer)
          get-status #(-> js/window .-document .-visibilityState visibility->status)]
        (with-let [e (ctor (dissoc attrs :fonts :icon :language :metadata :position :route :scripts :styles :title :initiated :mousechanged :positionchanged :statuschanged :routechanged :scroll) elems)]
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
          (.addEventListener js/window "scroll"
            (bound-fn []
              (let [[x y :as new-position] (vector (.-scrollX js/window) (.-scrollY js/window))]
                (reset! *position* new-position)
                (when positionchanged
                  (when-not (= new-position *position*)
                    (positionchanged x y))))))
          (cell= (set-route! route))
          (.addEventListener js/document "DOMContentLoaded"
            #(cell= (.scroll js/window (first position) (second position))))
          (h/head
            (h/html-meta :charset "utf-8")
            (h/html-meta :http-equiv "X-UA-Compatible" :content "IE=edge")
            (h/html-meta :name "viewport"    :content "width=device-width, initial-scale=1")
            (for [m (if (map? metadata) (mapv (fn [[k v]] {:name k :content v}) metadata) metadata)]
              (h/html-meta (into {} (for [[k v] m] [k (name v)]))))
            (when title
              (h/title title))
            (h/link :rel "icon" :href (or icon empty-icon-url))
            (h/for-tpl [f fonts]   (h/style f))
            (h/for-tpl [s styles]  (h/link :rel "stylesheet" :href s))
            (h/for-tpl [s scripts] (h/script :src s)))))))

;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare elem)

(def f
 {1 32
  2 24
  3 20
  4 16
  5 14
  6 13})

(defmulti  md (fn [tag ats elems] tag))
(defmethod md :default    [tag ats elems] (elem elems))
(defmethod md :markdown   [_ ats elems] elems)
(defmethod md :header     [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :bulletlist [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :numberlist [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :listitem   [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :para       [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :code_block [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :inlinecode [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :img        [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :linebreak  [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :link       [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :link_ref   [_ {:keys [level]} elems] (elem :sh (r 1 1) :f (f level 16) elems))
(defmethod md :em         [_ {:keys [level]} elems] (elem :fi :italic                 elems))
(defmethod md :strong     [_ {:keys [level]} elems] (elem :ft :bold                   elems))

(defn markdownable [ctor]
  (fn [{:keys [mdfn] :as attrs} elems]
    {:pre []} ;; todo: validate
    (binding [hoplon.ui.elems/*mdfn* (or mdfn md)]
      (ctor (dissoc attrs :mdfn) elems))))

;;; element primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def node (comp exceptional markdownable align shadow round border pad gutter nudge size dock fontable color transform clickable))

;;; element primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def elem    (-> h/div      box                                     node            parse-args))
(def cmpt*   (-> h/div      box interactive              toggleable node            parse-args))
(def canvas  (-> h/div      box (underlay h/canvas)                 node            parse-args))
(def frame   (-> h/div      box (underlay h/iframe)      frameable  node            parse-args))
(def image   (-> h/div      box (underlay h/img)         imageable  node            parse-args))
(def object  (-> h/div      box (underlay h/html-object) objectable node            parse-args))
(def video   (-> h/div      box (underlay h/video)       videoable  node            parse-args))
(def window* (->            doc                                     node windowable parse-args))

(def form*   (-> h/form     box         formable                    node            parse-args))
(def line    (-> h/input    box destyle fieldable   line-field      node            parse-args))
(def lines   (-> h/textarea box destyle fieldable   lines-field     node            parse-args))
(def pick    (-> h/div      box destyle fieldable   pick-field      node            parse-args))
(def picks   (-> h/div      box destyle fieldable   picks-field     node            parse-args))
(def item*   (-> h/option   box destyle interactive selectable  item-field node     parse-args))
(def file    (-> h/div      box         fieldable   file-field      node            parse-args))
(def files   (-> h/div      box         fieldable   file-field      node            parse-args))
(def write   (-> h/input    box destyle             send-field      node            parse-args))

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
  (cell= ((apply hash-map kvs) *state*)))

(defn font
  "font"
  [family style weight names urls & [ranges]]
  {:pre [(v/family? family) (v/style? style) (v/weight? weight)]}
  (let [name  #(str "local('" % "')")
        url   #(str "url('" % "') format('" (second (re-find #".+\.([^?]+)(\?|$)" %)) "')")
        src   (apply str (interpose "," (concat (map name names) (map url urls))))
        range (when ranges (apply str (interpose "," ranges)))
        props {"font-family"   family ;; ->elem
               "font-style"    (when style  (clojure.core/name style))
               "font-weight"   (when weight (clojure.core/name weight))
               "src"           src
               "unicode-range" range}]
    (str "@font-face{" (apply str (mapcat (fn [[k v]] (str k ":" v ";")) (clean props))) "}")))

;;; todos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offset (use outer css margin to move out of current position)
;; baseline-shift
;; background, url (str "url(" v ") no-repeat 50% 50% / cover")
;; update, previously implemented on do multimethod, to form middleware
;; throw proper ui exceptions with stack traces and attribute kv information
;; consider utility of introducing rtl positioning
