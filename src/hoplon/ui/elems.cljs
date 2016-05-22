(ns hoplon.ui.elems
  (:require
    [clojure.string  :refer [join]]
    [javelin.core    :refer [cell?]]
    [hoplon.core     :refer [do-watch]]
    [hoplon.ui.attrs :refer [rt hx ev bk ratio? hex? eval? break? ->attr]])
  (:require-macros
    [javelin.core    :refer [cell= with-let]]))

(declare elem? ->elem)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *exceptions* nil)

(defn throw-ui-exception [& msg]
  (swap! *exceptions* conj {:msg (apply str msg)}))

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

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IBox
  (-out [e])
  (-mid [e])
  (-in  [e]))

(defprotocol IElem
  "Serialize to DOM Element"
  (-toElem [_]))

(defprotocol IContain ;; todo: factor out
  (-sync [e elems]))

(defprotocol IRender
  (-attach [e])
  (-detach [e]))

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
  (-out [_]     root)
  (-mid [_] (-> root .-firstChild))
  (-in  [_] (-> root .-firstChild .-firstChild))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Elem: " (.-tagName (-out this)) " " (.-tagName (-mid this)) " "(.-tagName (-in this)) ">"))
  IElem
  (-toElem [_] root)
  IContain
  (-sync [this elems]
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
          (throw-ui-exception "Attribute " k " with value " v " cannot be applied to element."))
        (-sync e (mapv (bind-cells ->elem) elems))))))


#_(defn elem? [v] (satisfies? IElem v))
(defn elem? [v] (instance? Elem v)) ;; todo depend on interface instead, require strings, numbers to be Elems
(defn ->elem [v] (-toElem v))

(defn out [e] (-out e))
(defn mid [e] (-mid e))
(defn in  [e] (-in  e))
