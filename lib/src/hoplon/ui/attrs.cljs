(ns hoplon.ui.attrs
  (:refer-clojure
    :exclude [name load + - * /])
  (:require
    [clojure.string  :refer [blank? join]]
    [hoplon.ui.utils :refer [clean name]]))

(declare + - * /)

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttr
  "Serialize to DOM Attr"
  (-dom-attribute [_]))

(defprotocol ICSSAngle
  (-angle [_]))

(defprotocol ICSSColor
  (-color [_]))

(defprotocol IDecimal
  (-decimal [_]))

(defprotocol ISize
  (-extrinsic [_])
  (-explicit  [_]))

(defprotocol ICalc
  (-calc [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IAttr
  (-dom-attribute [this]
    "")
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [_]
    nil))

(extend-type number
  IAttr
  (-dom-attribute [this]
    (str this "px"))
  ICalc
  (-calc [this]
    this)
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(extend-type boolean
  IAttr
  (-dom-attribute [this]
    (str this)))

(extend-type string
  IAttr
  (-dom-attribute [this]
    (when-not (blank? this) this))
  ICalc
  (-calc [this]
    this)
  ISize
  (-extrinsic [this]
    this))

(extend-type PersistentVector
  IAttr
  (-dom-attribute [this]
    (join "," (map -dom-attribute this))))

(extend-type Keyword
  IAttr
  (-dom-attribute [this]
    (name this))
  ISize ;; handles auto, consider eliminating
  (-extrinsic [_]
    nil)
  (-explicit [_]
    nil))

;;; sizes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Ems [v]
  Object
  (toString [_]
   (str v "em"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (str "#<Size " this ">")))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ICalc
  (-calc [this]
    (.toString this))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Pixels [v]
  Object
  (toString [_]
    (str v "px"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (str "#<Size " this ">")))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ICalc
  (-calc [this]
    (.toString this))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Points [v]
  Object
  (toString [_]
    (str v "pt"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (str '"#<Size " this ">")))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ICalc
  (-calc [this]
    (.toString this))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Ratio [n d]
  Object
  (toString [_]
    (str (* (/ n d) 100) "%"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w (str "#<Size " this ">")))
  ICalc
  (-calc [this]
    (str n " / " d " * 100%"))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ISize
  (-extrinsic [this]
    this)
  (-explicit [_]
    nil)
  IDecimal
  (-decimal [this]
    (/ n d)))

;;; calculations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Calc [op vs]
  Object
  (toString [this]
    (str "calc(" (-calc this) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Calc: " this ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ICalc
  (-calc [_]
    (join (str " " op " ") (mapv -calc vs)))
  ISize
  (-extrinsic [this]
    (when (some -extrinsic vs)
      this))
  (-explicit [this]
    (when-not (some -extrinsic vs)
      this)))

(defn mkcalc [f op type-fn]
  (fn [& vs]
    (doseq [v vs]
      (assert (satisfies? ICalc v) (str "Calc function " op " was passed incalculable value " v ".")))
    (cond (every? number? vs) (apply f vs) (every? (comp not blank?) vs) (Calc. op (type-fn vs)))))

(deftype Transform [a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4]
  Object
  (toString [_]
    (if (and (= c1 0) (= d1 0) (= c2 0) (= d2 0) (= a3 0) (= b3 0) (= c3 1) (= d3 0) (= c4 0) (= d4 1))
      (str   "matrix(" (apply str (interpose ", " [a1 b1       a2 b2                   a4 b4      ])) ")")
      (str "matrix3d(" (apply str (interpose ", " [a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4])) ")")))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Transform " this ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn em [v]       (Ems.         v))
(defn pt [v]       (Points.      v))
(defn px [v]       (Pixels.      v))

(defn ratio?     [v] (instance? Ratio     v))
(defn ems?       [v] (instance? Ems       v))
(defn points?    [v] (instance? Points    v))
(defn calc?      [v] (instance? Calc      v))
(defn transform? [v] (instance? Transform v))

(defn attr?     [v] (satisfies? IAttr v))
(defn ->attr    [v] (-dom-attribute   v)) ;; remove
(defn ->dec     [v] (-decimal         v))
(defn extrinsic [v] (-extrinsic       v))
(defn explicit  [v] (-explicit        v))

(def + (mkcalc clojure.core/+ "+" (partial mapv ->attr)))
(def - (mkcalc clojure.core/- "-" (partial mapv ->attr)))
(def * (mkcalc clojure.core/* "*" #(update % [0] ->attr)))
(def / (mkcalc clojure.core// "/" #(update % [0] ->attr)))

(defn r
  "ratio"
  ([n]
   (r n 100))
  ([n d]
   (Ratio. n d)))

(defn t
  "transformation"
  ([a b c d tx ty]
   (t a b 0 0 c d 0 0 0 0 1 0 tx ty 0 1))
  ([a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4]
   (Transform. a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4)))

(defn scale
  ([x]
   (scale x x))
  ([x y]
   (t x 0 0 y 0 0)))

(defn skew
  ([x]
   (skew x x))
  ([x y]
   (t 1 x y 1 0 0)))

(defn translate
  ([x]
   (translate x x))
  ([x y]
   (t 1 0 0 1 x y)))


