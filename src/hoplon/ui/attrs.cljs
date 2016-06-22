(ns hoplon.ui.attrs
  (:refer-clojure :exclude [+ - * /])
  (:require
    [clojure.string :refer [blank? join]]
    [javelin.core   :refer [cell]])
  (:require-macros
    [javelin.core   :refer [cell= with-let set-cell!=]]))

(declare + - * /)

(def ^:dynamic *state* nil)

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttr
  "Serialize to DOM Attr"
  (-dom-attribute [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  IAttr
  (-dom-attribute [this]
    ""))

(extend-type number
  IAttr
  (-dom-attribute [this]
    (str this "px")))

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
    (.toString this)))

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
    (str v  "em")))

(deftype Pixels [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IAttr
  (-dom-attribute [_]
    (if v (str v "px") "initial")))

(deftype Points [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pt"))
  IAttr
  (-dom-attribute [this]
    (str v  "pt")))

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IAttr
  (-dom-attribute [_]
    (if n (str (* (/ n d) 100) "%") "initial")))

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

(defn color?     [v] (instance? Color  v))
(defn ratio?     [v] (instance? Ratio  v))
(defn ems?       [v] (instance? Ems    v))
(defn points?    [v] (instance? Points v))
(defn calc?      [v] (instance? Calc  v))
(defn shadow?    [v] (instance? Shadow v))

(defn attr? [v] (satisfies? IAttr v))
(defn ->attr [v] (-dom-attribute v))

(defn mkcalc [f fname]
  (fn [& vs]
    (if (every? number? vs) (apply f vs) (Calc. f fname vs))))

(def - (mkcalc clojure.core/- "-"))
(def + (mkcalc clojure.core/+ "+"))
(def * (mkcalc clojure.core/* "*"))
(def / (mkcalc clojure.core// "/"))

(defn b
  "breakpoints."
  [orientation & vs]
  (with-let [v (cell nil)]
    (let [o (case orientation :w "width" :h "height")]
      (doseq [[min val max] (partition 3 2 (concat [0] vs [999999]))]
        (let [query (.matchMedia js/window (str "(min-" o ": " min "px) and (max-" o ": " max "px)"))
              value! #(when (.-matches %) (set-cell!= v val))]
          (value! query)
          (.addListener query #(value! %)))))))

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

(defn s
  "states"
  ;; todo: transition between states
  [& kvs]
  (cell :red #_((apply hash-map kvs) *state*)))
