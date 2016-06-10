(ns hoplon.ui.elems
  (:require
    [clojure.string :refer [split-lines trim]]
    [hoplon.core    :refer [html body br]])
  (:require-macros
    [javelin.core   :refer [cell= with-let]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn split-nodes [text]
  (let [br  #(.createElement  js/document "br")
        txt #(.createTextNode js/document %)]
    (->> (split-lines text)
         (interpose br)
         (map #(if (fn? %) (%) (txt (trim %)))))))

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IBox
  (-out [e])
  (-mid [e])
  (-in  [e]))

(defprotocol IElem
  (-dom-element [e]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IElem
  (-dom-element [this]
    this))

(extend-type number
  IElem
  (-dom-element [this]
    this))

(extend-type string
  IElem
  (-dom-element [this]
    (with-let [frag (.createDocumentFragment js/document)]
      (doseq [line (split-nodes this)]
        (.appendChild frag line)))))

(extend-type PersistentVector
  IElem
  (-dom-element [this]
    (with-let [frag (.createDocumentFragment js/document)]
      (doseq [elem this]
        (.appendChild frag (-dom-element elem))))))

(extend-type javelin.core/Cell
  IElem
  (-dom-element [this]
    (cell= (-dom-element this))))

(deftype Elem [o m i]
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Elem: " (.-tagName o) " " (.-tagName m) " " (.-tagName i) ">"))
  IBox
  (-out [_] o)
  (-mid [_] m)
  (-in  [_] i)
  IElem
  (-dom-element [e] o)
  hoplon.core/ICustomElement
  (-append-child! [_ new-elem]
    (hoplon.core/append-child! i new-elem))
  (-insert-before! [_ new-elem old-elem]
    (hoplon.core/insert-before! i new-elem old-elem))
  (-remove-child! [_ old-elem]
    (hoplon.core/remove-child! i old-elem)))

(defn- box-tree [tags]
  "construct a linked list of dom elements from a vector of tags and ctors"
  (with-let [e (let [t (nth tags 0)] (if (fn? t) (t) (.createElement js/document t)))]
    (when-let [tags (not-empty (subvec tags 1))]
      (.appendChild e (box-tree tags)))))

(defn- doc-path [box-tree]
  (let [o box-tree
        m (.-lastChild  o)
        i (.-firstChild m)]
    (list o m i)))

(defn- box-path [box-tree]
  (if-let [e (.-lastChild box-tree)]
    (conj (box-path e) box-tree)
    (list box-tree)))

(defn box-model [box-tree]
  (with-let [[o m i] box-tree]
    (set! (.. o -style -boxSizing)     "border-box")   ;; include border and padding in dimensions, necessary to prevent gutter/margin attributes from breaking pct widths
    (set! (.. o -style -display)       "inline-table") ;; layout ltr ttb when width not 100% to support responsive design
    (set! (.. o -style -verticalAlign) "top")          ;; inline-block/table elems must be explititly told to align themselves to the top
    (set! (.. o -style -textAlign)     "initial")      ;; prevent inheritance of alignment from parent
    (set! (.. m -style -boxSizing)     "border-box")   ;; include border and padding in dimensions
    (set! (.. m -style -display)       "table-cell")   ;; cells in tables enable sane vertical alignment
    (set! (.. m -style -position)      "relative")     ;; support an absolutely positioned elements in the background such as svgs or image elements
    (set! (.. m -style -height)        "inherit")      ;; assume the height of the parent and proxy it to the inner div, :todo :min, max h?
    (set! (.. i -style -display)       "block")        ;; prevent white space from creeping in around inline elements
    (set! (.. i -style -position)      "relative")     ;; make positioned children adjust relative to container plus padding
    (set! (.. i -style -height)        "100%")         ;; display block fills the width, but needs to be told to fill the height (unless vertical alignment is set)
    (set! (.. i -style -cursor)        "inherit")))    ;; apply the mouse cursor set on the middle div to the inner div as well

(defn elem? [v] (instance? Elem v))

(defn ->element [v] (-dom-element v))

(defn box-with [path-fn & tags]
  "create an Elem by wrapping the model outside of the element constructor"
  (fn [_ elems]
    (let [[o m i] (-> tags box-tree path-fn box-model)]
      (with-let [e (Elem. o m i)]
        (hoplon.core/add-children! e (mapv -dom-element elems))))))

(def  doc        (box-with doc-path html body "div"))
(defn box [ctor] (box-with box-path "div" "div" ctor))

(defn out [e] (-out e))
(defn mid [e] (-mid e))
(defn in  [e] (-in  e))
