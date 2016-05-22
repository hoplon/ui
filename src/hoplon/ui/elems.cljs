(ns hoplon.ui.elems
  (:require
    [javelin.core    :refer [cell?]]
    [hoplon.ui.attrs :refer [rt hx ev bk ratio? hex? eval? break? ->attr]])
  (:require-macros
    [javelin.core    :refer [with-let]]))

(declare elem? ->elem)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bind-cells [f]
  (fn [& vs]
    (let [watch (fn [i v] (if (cell? v) @(add-watch v i #(apply f (assoc vs i %4))) v))]
      (apply f (map-indexed watch vs)))))

(defn- child-vec
  [this]
  (let [x (.-childNodes this)
        l (.-length x)]
    (loop [i 0 ret (transient [])]
      (or (and (= i l) (persistent! ret))
          (recur (inc i) (conj! ret (.item x i)))))))

(defn out* [e] e)
(defn mid* [e] (-> e .-firstChild))
(defn in*  [e] (-> e .-firstChild .-firstChild))

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IBox
  (-out [e])
  (-mid [e])
  (-in  [e]))

(defprotocol IElem
  "Serialize to DOM Element"
  (-toElem [e]))

(defprotocol IRender
  (-attach [e elems]))
#_(-detach [e elems])

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IElem
  (-toElem [this]
    nil))

(extend-type js/Number
  IElem
  (-toElem [this] ;; todo: construct elem with texnode at center
    (.createTextNode js/document (str this))))

(extend-type js/String
  IElem
  (-toElem [this] ;; todo: construct elem with textnode at center
    (.createTextNode js/document this)))

(deftype Elem [root]
  IBox
  (-out [_]
    (out* root))
  (-mid [_]
    (mid* root))
  (-in  [_]
    (in* root))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Elem: " (.-tagName (-out this)) " " (.-tagName (-mid this)) " "(.-tagName (-in this)) ">"))
  IElem
  (-toElem [_]
    root)
  IRender
  (-attach [this elems]
    (let [new  (remove nil? (flatten elems))
          new? (set new)]
      (loop [[x & xs] new
             [k & ks :as elems] (child-vec (-in this))]
        (when (or x k)
          (recur xs (cond (= x k) ks
                          (not k) (with-let [ks ks]
                                    (.appendChild (-in this) x))
                          (not x) (with-let [ks ks]
                                    (when-not (new? k)
                                      ;; todo: remove handlers
                                      (.removeChild (-in this) k)))
                          :else   (with-let [_ elems]
                                    (.insertBefore (-in this) x) k))))))))

(defn nest [tags]
  "construct a linked list of dom elements from a vector of html tags"
  (with-let [e (.createElement js/document (nth tags 0))]
    (when-let [tags (not-empty (subvec tags 1))]
      (.appendChild e (nest tags)))))

(defn elem-from [tags]
  (with-let [b (nest tags)]
    (set! (-> b out* .-style .-boxSizing)     "border-box")   ;; include border and padding in dimensions to prevent visual errors from shifting layout
    (set! (-> b out* .-style .-display)       "inline-table") ;; layout ltr ttb when width not 100% to support responsive design
    (set! (-> b out* .-style .-verticalAlign) "top")          ;; inline-block/table elems must be explititly told to align themselves to the top
    (set! (-> b out* .-style .-textAlign)     "initial")      ;; prevent inheritance of alignment from parent
    (set! (-> b mid* .-style .-boxSizing)     "border-box")   ;; include border and padding in dimensions
    (set! (-> b mid* .-style .-display)       "table-cell")   ;; cells in tables enable sane vertical alignment
    (set! (-> b mid* .-style .-position)      "relative")     ;; support an absolutely positioned svg skin in the background
    (set! (-> b mid* .-style .-height)        "inherit")      ;; assume the height of the parent and proxy it to the inner div
    (set! (-> b in*  .-style .-cursor)        "inherit")      ;; inherit mouse cursor, set in middleware?
    (set! (-> b in*  .-style .-display)       "block")        ;; prevent white space from creeping in around inline elements
    (set! (-> b in*  .-style .-position)      "relative")     ;; make positioned children adjust relative to container plus padding
    (set! (-> b in*  .-style .-height)        "inherit")))    ;; display block fills the width, but needs to be told to fill the height (unless vertical alignment is set)

(defn mkelem [& tags]
  (let [elem (elem-from tags)]
    (fn [_ elems]
      (with-let [e (Elem. (.cloneNode elem true))]
        (-attach e (mapv (bind-cells ->elem) elems))))))

#_(defn elem? [v] (satisfies? IElem v))
(defn elem? [v] (instance? Elem v)) ;; todo depend on interface instead, require strings, numbers to be Elems
(defn ->elem [v] (-toElem v))

(defn out [e] (-out e))
(defn mid [e] (-mid e))
(defn in  [e] (-in  e))
