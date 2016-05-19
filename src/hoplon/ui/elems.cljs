(ns hoplon.ui.elems
  (:require
    [javelin.core    :refer [cell?]]
    [hoplon.core     :refer [do-watch]]
    [hoplon.ui.value :refer [IDOM ev rt px kw hx ratio? model? model]])
  (:require-macros
    [javelin.core    :refer [cell= with-let]]))

(declare elem?)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn throw-ui! [elem & msg]
  (let [error-msg (apply str "UI ERROR: " msg)]
    (when elem.style
      (set! (.. elem -title) error-msg)
      (set! (.. elem -style -border) "2px dotted red"))
    (println error-msg)))

(defn set-prop! [elem f k v] ;; todo: mult vs to reduce # of cells
  (let [e (f elem)]
    (cond (cell?    v) (do-watch v #(set-prop! elem f k %2))
          (integer? v) (aset e k (model (px v)))
          (model?   v) (aset e k (model v))
          (nil?     v) identity
          (string?  v) (aset e k v)
          :else        (throw-ui! elem "Property " k " has an invalid value of " v "."))))

(defn set-style! [elem f k v] ;; todo: mult vs to reduce # of cells
  (let [e (f elem)]
    (cond (cell?    v) (do-watch v #(set-style! elem f k %2))
          (integer? v) (aset e "style" k (model (px v)))
          (model?   v) (aset e "style" k (model v))
          (nil?     v) identity
          :else        (throw-ui! elem "Attribute " k " has an invalid value of " v "."))))

(defn swap-elems! [e f v]
  (cond (cell?   e) (do-watch e #(f @e %2))
        (vector? e) (doseq [e e] (swap-elems! e f v)) ;; loop?
        (elem?   e) (f e v)
        (string? e) identity
        :else       (throw-ui! e "Invalid child of type " (type e) " with value " v ".")))

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
      (set! (-> b m .-height)        "inherit")      ;; assume the height of the parent and proxy it to the inner div
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

;;; attributes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn align [ctor]
  "set the text-align and vertical-align attributes on the elem and proxy the
   vertical-align attribute to the outer element of each child.  set vertical
   height of the inner element to auto when the align vertical attribute is set.

  the vertical alignment is proxied to the outer elements of the children so
  that, in addition to aligning the lines of children within the elem, the
  children are also aligned in the same manner within their respective lines."
  (fn [{:keys [ah av] :as attrs} elems]
    (swap-elems! elems #(set-style! %1 out "vertical-align" %2) av)
    (with-let [e (ctor (dissoc attrs :ah :av) elems)]
      (set-style! e in  "height"         (cell= (if av :auto :inherit))) ;; initial instead?
      (set-style! e mid "text-align"     ah)
      (set-style! e mid "vertical-align" av))))

(defn color [ctor]
  "set the background color an the inner element."
  (fn [{:keys [c o m] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :c :o :m) elems)]
      (set-style! e mid "background-color" c)
      (set-style! e mid "opacity"          o)
      (set-style! e in  "cursor"           m))))

(defn overflow [ctor]
  "set the overflow style on the elem's middle element."
  (fn [{:keys [v vh vv] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :v :vh :vv) elems)]
      (set-style! e in "overflow-x" (cell= (or vh v)))
      (set-style! e in "overflow-y" (cell= (or vv v))))))

(defn pad [ctor]
  "set the padding on the elem's inner element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [p pl pr pt pb ph pv] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :p :pl :pr :pt :pb :ph :pv) elems)]
      (set-style! e in "padding-left"   (cell= (or pl ph p)))
      (set-style! e in "padding-right"  (cell= (or pr ph p)))
      (set-style! e in "padding-top"    (cell= (or pt pv p)))
      (set-style! e in "padding-bottom" (cell= (or pb pv p))))))

(defn round [ctor]
  "set the radius on the middle element."
  (fn [{:keys [r rtl rtr rbl rbr] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :r :rtl :rtr :rbl :rbr) elems)]
      (set-style! e mid "border-top-left-radius"     (cell= (or rtl r)))
      (set-style! e mid "border-top-right-radius"    (cell= (or rtr r)))
      (set-style! e mid "border-bottom-left-radius"  (cell= (or rbl r)))
      (set-style! e mid "border-bottom-right-radius" (cell= (or rbr r))))))

(defn size [ctor]
  "set the size on the outer element when it is expressed as a ratio, and on the
   inner element when it is a length.

   since ratios are expressed in terms of the parent, they include the margin
   (implemented as the padding between the inner and outer elements). fixed
   lengths are set on the middle, however, to exclude the margin so that when a
   margin is added, it will push out against the parent container instead of
   being subtracted from the size of the elem."
  (fn [{:keys [w w- w+ h h- h+] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :w :w- :w+ :h :h- :h+) elems)]
      (let [f #(if (ratio? %) out mid)]
        (set-style! e (f w)  "width"  w)
        (set-style! e (f w-) "width"  w-)
        (set-style! e (f w+) "width"  w+)
        (set-style! e (f h)  "height" h)
        (set-style! e (f h-) "height" h-)
        (set-style! e (f h+) "height" h+)))))

(defn space [ctor]
  "set the padding on the outer element of each child and a negative margin on
   the inner element of the elem itself equal to the padding.

   outer padding on the children creates an even gutter between them, while the
   negative inner margin on the elem itself offsets this padding to fencepost
   the children flush with the edges of the container."
  (fn [{:keys [g gh gv] :as attrs} elems]
    (let [mh (cell= (/ (or gh g) 2))
          mv (cell= (/ (or gv g) 2))
          ph (cell= (- mh))
          pv (cell= (- mv))]
      (swap-elems! elems #(set-style! % out "padding-left"   %2) mh)
      (swap-elems! elems #(set-style! % out "padding-right"  %2) mh)
      (swap-elems! elems #(set-style! % out "padding-top"    %2) mv)
      (swap-elems! elems #(set-style! % out "padding-bottom" %2) mv)
      (with-let [e (ctor (dissoc attrs :g :gh :gv) elems)]
        (set-style! e in "margin-left"   ph)
        (set-style! e in "margin-right"  ph)
        (set-style! e in "margin-top"    pv)
        (set-style! e in "margin-bottom" pv)))))

(defn stroke [ctor]
  "set the border on the elem's middle element.

   this adds space between the edges of the container and its children."
  (fn [{:keys [s sl sr st sb sh sv sc scl scr sct scb sch scv] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :s :sl :sr :st :sb :sh :sv :sw :sc :scl :scr :sct :scb :sch :scv) elems)]
      (set-style! e mid "border-left-width"   (cell= (or sl sh s)))
      (set-style! e mid "border-left-color"   (cell= (or scl sch sc)))
      (set-style! e mid "border-left-style"   (cell= (when (or sl sh s) :solid)))
      (set-style! e mid "border-right-width"  (cell= (or sr sh s)))
      (set-style! e mid "border-right-color"  (cell= (or scr sch sc)))
      (set-style! e mid "border-right-style"  (cell= (when (or sr sh s) :solid)))
      (set-style! e mid "border-top-width"    (cell= (or st sv s)))
      (set-style! e mid "border-top-color"    (cell= (or sct scv sc)))
      (set-style! e mid "border-top-style"    (cell= (when (or st sv s) :solid)))
      (set-style! e mid "border-bottom-width" (cell= (or sb sv s)))
      (set-style! e mid "border-bottom-color" (cell= (or scb scv sc)))
      (set-style! e mid "border-bottom-style" (cell= (when (or sb sv s) :solid))))))

(defn font [ctor]
  "set the font styles pertaining to the attribute"
  (fn [{:keys [f fw fh ft ff fc fu fi fm fk] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :f :fw :fh :ft :ff :fc :fu :fi :fm :fk) elems)]
      (set-style! e mid "font-size"       f)
      (set-style! e mid "letter-spacing"  fw)
      (set-style! e mid "line-height"     fh)
      (set-style! e mid "font-weight"     ft)
      (set-style! e mid "font-family"     ff)
      (set-style! e mid "color"           fc)
      (set-style! e mid "text-decoration" fu)
      (set-style! e mid "font-style"      fi)
      (set-style! e mid "font-smooth"     fm)
      (set-style! e mid "font-kerning"    fk))))

(defn destyle [ctor]
  "neutralize the default styling of the inner element.

  this allows native components to be styled freely using attributes mapped to
  the middle element."
  (fn [attrs elems]
    (with-let [e (ctor attrs elems)]
      (set-style! e in "background-color"  :transparent)
      (set-style! e in "border-style"      :none)
      (set-style! e in "text-align"        :inherit)))) ;; cursor: pointer, :width: 100%

(defn img [ctor]
  (fn [{:keys [url] :as attrs} elems]
    (with-let [e (ctor (dissoc attrs :url) elems)]
      (set-prop! e in "src" url))))

(defn parse-args [ctor]
  (fn [& args]
     (apply ctor (#'hoplon.core/parse-args args))))

;; todo:
;; shadow
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

(def elem   (-> (mkelem "div" "div" "div")    color font overflow size pad stroke round align space parse-args))
(def button (-> (mkelem "div" "div" "button") destyle color font overflow size pad stroke round align parse-args))
(def image  (-> (mkelem "div" "div" "img")    color font overflow size pad stroke round img parse-args))
