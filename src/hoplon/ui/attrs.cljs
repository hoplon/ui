(ns hoplon.ui.attrs
  (:refer-clojure
    :exclude [+ - * /])
  (:require
    [clojure.string :refer [blank? join]]))

(declare + - * /)

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttr
  "Serialize to DOM Attr"
  (-dom-attribute [_]))

(defprotocol ISize
  (-extrinsic [_])
  (-explicit  [_]))

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
    (when-not (blank? this) this)))

(extend-type PersistentVector
  IAttr
  (-dom-attribute [this]
    (apply str (interpose ", " (map -dom-attribute this)))))

(extend-type Keyword
  IAttr
  (-dom-attribute [this]
    (name this)))

(deftype Calc [f fname vs]
  Object
  (toString [_]
    (str "calc(" (join (str " " fname " ") (mapv -dom-attribute vs)) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Calc: " (.toString this) ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this))
  ISize
  (-extrinsic [this]
    (when (some -extrinsic vs)
      this))
  (-explicit [this]
    (when-not (some -extrinsic vs)
      this)))

(deftype Color [r g b a]
  Object
  (toString [_]
    (str "rgba(" r ", " g ", " b ", " a ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Color: " (.toString this) ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype Ems [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " em"))
  IAttr
  (-dom-attribute [this]
    (str v  "em"))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Pixels [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IAttr
  (-dom-attribute [_]
    (if v (str v "px") "initial"))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Points [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pt"))
  IAttr
  (-dom-attribute [this]
    (str v  "pt"))
  ISize
  (-extrinsic [_]
    nil)
  (-explicit [this]
    this))

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IAttr
  (-dom-attribute [_]
    (if n (str (* (/ n d) 100) "%") "initial"))
  ISize
  (-extrinsic [this]
    this)
  (-explicit [_]
    nil))

(deftype Shadow [x y color blur spread inset]
  Object
  (toString [_]
    (apply str (interpose " " (map -dom-attribute [x y blur spread color]))))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Shadow: " (.toString this) ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype Transform [a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4]
  Object
  (toString [_]
    (str "matrix3d(" (apply str (interpose ", " [a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4])) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Transform: " (.toString this) ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

; (deftype Transition [v]
;   Object
;   (toString [_]
;     (apply str (interpose " " (map -dom-attribute [x y blur spread color]))))
;   IPrintWithWriter
;   (-pr-writer [this w _]
;     (write-all w "#<Shadow: " (.toString this) ">"))
;   IAttr
;   (-dom-attribute [this]
;     (.toString this)))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn em [v]       (Ems.         v))
(defn pt [v]       (Points.      v))
(defn px [v]       (Pixels.      v))
(defn d [x y color & [blur spread inset]] (Shadow. x y color blur spread inset))

(defn color?     [v] (instance? Color     v))
(defn ratio?     [v] (instance? Ratio     v))
(defn ems?       [v] (instance? Ems       v))
(defn points?    [v] (instance? Points    v))
(defn calc?      [v] (instance? Calc      v))
(defn shadow?    [v] (instance? Shadow    v))
(defn transform? [v] (instance? Transform v))

(defn attr? [v] (satisfies? IAttr v))
(defn ->attr [v] (-dom-attribute v))

(defn extrinsic [v] (-extrinsic v))
(defn explicit  [v] (-explicit  v))

(defn mkcalc [f fname]
  (fn [& vs]
    (if (every? number? vs) (apply f vs) (Calc. f fname vs))))

(def - (mkcalc clojure.core/- "-"))
(def + (mkcalc clojure.core/+ "+"))
(def * (mkcalc clojure.core/* "*"))
(def / (mkcalc clojure.core// "/"))

(defn c
  "color"
  ([hex]
   (c hex 1))
  ([hex a]
   (let [r (bit-and (bit-shift-right hex 16) 255)
         g (bit-and (bit-shift-right hex 8)  255)
         b (bit-and hex 255)]
     (c r g b a)))
  ([r g b]
   (c r g b 1))
  ([r g b a]
   (Color. r g b a)))

(defn r
  "ratio"
  [n d]
  (Ratio. n d))

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
