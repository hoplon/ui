(ns hoplon.ui.value
  (:require
    [clojure.string :refer [join]]
    [javelin.core   :refer [Cell]]))

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttrValue
  "Serialize to DOM Attr"
  (-toAttr [_]))

(defprotocol IElemValue
  "Serialize to DOM Element"
  (-toElem [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IAttrValue
  (-toAttr [this]
    "initial")
  IElemValue
  (-toElem [this]
    nil))

(extend-type js/Number
  IAttrValue
  (-toAttr [this]
    (str this "px"))
  IElemValue
  (-toElem [this]
    (.createTextNode js/document (str this))))

(extend-type js/String
  IAttrValue
  (-toAttr [this]
    this) ;; blank? "initial"
  IElemValue
  (-toElem [this]
    (.createTextNode js/document this)))

(extend-type function
  IAttrValue
  (-toAttr [this]
    "-")) ;;todo: capture symbol as macro

(extend-type Keyword
  IAttrValue
  (-toAttr [this]
    (name this)))

(deftype Hex [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w "0x" (.toString v 16)))
  IAttrValue
  (-toAttr [this]
    (if this (str "#" (.toString v 16)) "initial")))

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IAttrValue
  (-toAttr [_]
    (if n (str (* (/ n d) 100) "%") "initial")))

(deftype Pixels [v] ;; unit, not value
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IAttrValue
  (-toAttr [_]
    (if v (str v "px") "initial")))

(deftype Eval [vs]
  Object
  (toString [_]
    (apply pr-str (conj (mapv str vs) " evaluation")))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (.toString this)))
  IAttrValue
  (-toAttr [_]
    (let [vstrs (mapv -toAttr vs)]
      (if vs (str "calc(" (join (str " "(nth vstrs 0) " ") (subvec vstrs 1)) ")") "initial"))))

(deftype Break [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " breakpoints"))
  IAttrValue
  (-toAttr [_]
    v))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rt [n d]  (Ratio. n d))
(defn hx [v]    (Hex.   v))
(defn ev [& vs] (Eval.  vs))
(defn bk [& vs] (Break. vs))

(defn ratio? [v] (instance? Ratio v))
(defn hex?   [v] (instance? Hex   v))
(defn eval?  [v] (instance? Eval  v))
(defn break? [v] (instance? Break v))

(defn attr? [v] (satisfies? IAttrValue v))
(defn elem? [v] (satisfies? IElemValue v))

(defn ->attr [v] (-toAttr v))
(defn ->elem [v] (-toElem v))
