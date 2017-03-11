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

(defprotocol IFont
  (-font [_]))

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

;;; colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype RGBA [r g b a]
  Object
  (toString [_]
    (str "rgba(" r ", " g ", " b ", " (if (satisfies? IDecimal a) (-decimal a) a) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Color " this ">"))
  IAssociative
  (-assoc [this key val]
    (let [v #(if (= key %1) val %2)]
      (RGBA. (v :r r) (v :g g) (v :b b) (v :a a))))
  ;ILookup
  ;(-lookup [this key]
  ;  (-lookup [this key nil]))
  ;(-lookup [this key not-found])
  IFn
  (-invoke [this & args]
    (apply assoc this args))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype HSLA [h s l a]
  Object
  (toString [_]
    (str "hsla(" h ", " s ", " l ", " (if (satisfies? IDecimal a) (-decimal a) a) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Color " this ">"))
  IAssociative
  (-assoc [this key val]
    (let [v #(if (= key %1) val %2)]
      (HSLA. (v :h h) (v :s s) (v :l l) (v :a a))))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype LinearGradient [angle colors]
  Object
  (toString [_]
    (str "linear-gradient(" angle "deg, " (join ", " colors) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Image " this ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype RadialGradient [shape extent x y color-stops]
  Object
  (toString [_]
    (str "radial-gradient(" (apply str shape  " " extent " at " x " " y ", " (join ", " color-stops)) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Image " this ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

(deftype Shadow [x y color blur spread inset]
  Object
  (toString [_]
    (join " " (map -dom-attribute [x y blur spread color])))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Shadow " this ">"))
  IAttr
  (-dom-attribute [this]
    (.toString this)))

;;; fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j

(deftype Font [path sources]
  Object
  (toString [_]
    (str [path sources]))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Font " this ">"))
  IFont
  (-font [_]
    [path sources])
  IAttr
  (-dom-attribute [this]
    (first path)))

;;; calculations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Calc [f fname vs]
  Object
  (toString [_]
    (str "calc(" (join (str " " fname " ") (mapv -dom-attribute vs)) ")"))
  IPrintWithWriter
  (-pr-writer [this w _]
    (write-all w "#<Calc: " this ">"))
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

(defn color?     [v] (or (instance? RGBA v) (instance? HSLA v)))
(defn font?      [v] (instance? Font      v))
(defn ratio?     [v] (instance? Ratio     v))
(defn ems?       [v] (instance? Ems       v))
(defn points?    [v] (instance? Points    v))
(defn calc?      [v] (instance? Calc      v))
(defn shadow?    [v] (instance? Shadow    v))
(defn transform? [v] (instance? Transform v))

(defn attr? [v] (satisfies? IAttr v))
(defn ->attr [v] (-dom-attribute  v))
(defn ->font [v] (-font           v))

(defn extrinsic [v] (-extrinsic v))
(defn explicit  [v] (-explicit  v))
,
(defn mkcalc [f fname]
  (fn [& vs]
    (if (every? number? vs) (apply f vs) (Calc. f fname vs))))

(def - (mkcalc clojure.core/- "-"))
(def + (mkcalc clojure.core/+ "+"))
(def * (mkcalc clojure.core/* "*"))
(def / (mkcalc clojure.core// "/"))

(defn rgb
  "construct a color based on the cubic coordinate system from a red, blue,
   green and an optional alphatransparency parameter. rgb values may be
   expressed as intergers ranging from 0 to 255, ratios, or as a single
   hexidecimal value.  the alpha must be a ratio."
  ([hex]
   (rgb hex 1))
  ([hex a]
   (let [r (bit-and (bit-shift-right hex 16) 255)
         g (bit-and (bit-shift-right hex 8)  255)
         b (bit-and hex 255)]
     (rgb r g b a)))
  ([r g b]
   (rgb r g b 1))
  ([r g b a]
   (RGBA. r g b a)))

(defn hsl
  "construct a color based on the cylindrical coordinate system from hue,
   saturation, lightness and an optional alphatransparancy parameter.  hsl
   values may be expressed as integers ranging from"
  ([h s l]
   (hsl h s l 1))
  ([h s l a]
   (HSLA. h s l a)))

(defn lgr [angle & color-stops]
  "linear gradient"
  (LinearGradient. angle color-stops))

#_(defn cgr [{:keys [p ph pv e]} & color-stops]
  "circular gradient"
  (let [stops (reduce #(if (diff type) (first? %) ) []  color-stops)]
    (RadialGradient. "circle" (name e) (or p ph) (or p pv) stops)))

#_(defn egr [{:keys [p pv ph e]} & color-stops]
  "eliptical gradient"
  (RadialGradient. "elipse" (name e) (or p ph) (or p pv) color-stops))

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

(defn font [& args]
  (let [[[width weight slope] sources] (if (odd? (count args)) [(last args) (drop-last args)] [nil args])]
    (Font. [(str (gensym "font-")) (or width :normal) (or weight :400) (or slope :regular)] (apply hash-map sources))))

