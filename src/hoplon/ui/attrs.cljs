(ns hoplon.ui.attrs
  (:require
    [clojure.string :refer [blank? join]]))

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttr
  "Serialize to DOM Attr"
  (-toAttr [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IAttr
  (-toAttr [this]
    "initial"))

(extend-type js/Number
  IAttr
  (-toAttr [this]
    (str this "px")))

(extend-type js/Boolean
  IAttr
  (-toAttr [this]
    (str this)))

(extend-type js/String
  IAttr
  (-toAttr [this]
    (if-not (blank? this) this "initial")))

(extend-type function
  IAttr
  (-toAttr [this]
    "-")) ;;todo: capture symbol via macro

(extend-type Keyword
  IAttr
  (-toAttr [this]
    (name this)))

(deftype Color [r g b a]
  Object
  (toString [_]
    (str "rgba(" r ", " g ", " b ", " a ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Color: " (.toString this) ">"))
  IAttr
  (-toAttr [this]
    (.toString this)))

(deftype Hex [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w "0x" (.toString v 16)))
  IAttr
  (-toAttr [this]
    (if this (str "#" (.toString v 16)) "initial")))

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IAttr
  (-toAttr [_]
    (if n (str (* (/ n d) 100) "%") "initial")))

(deftype Pixels [v] ;; unit, not value
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IAttr
  (-toAttr [_]
    (if v (str v "px") "initial")))

(deftype Eval [vs]
  Object
  (toString [_]
    (apply pr-str (conj (mapv str vs))))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (.toString this) " evaluation"))
  IAttr
  (-toAttr [_]
    (let [vstrs (mapv -toAttr vs)]
      (if vs (str "calc(" (join (str " "(nth vstrs 0) " ") (subvec vstrs 1)) ")") "initial"))))

(deftype Break [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " breakpoints"))
  IAttr
  (-toAttr [_]
    v))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn c  [r g b a] (Color. r g b a))
(defn rt [n d]     (Ratio. n d))
(defn hx [v]       (Hex.   v))
(defn ev [& vs]    (Eval.  vs))
(defn bk [& vs]    (Break. vs))

(defn color? [v] (instance? Color v))
(defn ratio? [v] (instance? Ratio v))
(defn hex?   [v] (instance? Hex   v))
(defn eval?  [v] (instance? Eval  v))
(defn break? [v] (instance? Break v))

(defn attr? [v] (satisfies? IAttr v))

(defn ->attr [v] (-toAttr v))
