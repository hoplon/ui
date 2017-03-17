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

(defn color?     [v] (or (instance? RGBA v) (instance? HSLA v)))
(defn font?      [v] (instance? Font      v))
(defn ratio?     [v] (instance? Ratio     v))
(defn ems?       [v] (instance? Ems       v))
(defn points?    [v] (instance? Points    v))
(defn calc?      [v] (instance? Calc      v))
(defn shadow?    [v] (instance? Shadow    v))
(defn transform? [v] (instance? Transform v))

(defn attr?     [v] (satisfies? IAttr v))
(defn ->attr    [v] (-dom-attribute   v))
(defn ->font    [v] (-font            v))
(defn ->dec     [v] (-decimal         v))
(defn extrinsic [v] (-extrinsic       v))
(defn explicit  [v] (-explicit        v))
,
(defn mkcalc [f fname]
  (fn [& vs]
    (if (every? number? vs) (apply f vs) (Calc. f fname vs))))

(def - (mkcalc clojure.core/- "-"))
(def + (mkcalc clojure.core/+ "+"))
(def * (mkcalc clojure.core/* "*"))
(def / (mkcalc clojure.core// "/"))

(defn s
  "construct a shadow for use with elems and text."
  [x y color & [blur spread inset]]
  (Shadow. x y color blur spread inset))

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
  "construct a font from the sources provided that will be loaded lazily when
   required by a run of text.  this function accepts any number of souces as
   key value pairs and an optional path for selecting a particular font when
   any of these sources contain a typeface/family of multiple fonts. note that
   the width (stretch), weight (boldness), and slope (style) of a font are
   determined when it is constructed, typically like this:

     (def geometos      (font :system [\"Geometos\"]      :opentype \"geometos.ttf\"))
     (def lato-regular  (font :system [\"Lato Regular\"]  :opentype \"lato-regular.ttf\"))
     (def lato-italic   (font :system [\"Lato Italic\"]   :opentype \"lato-italic.ttf\"))
     (def lato-medium   (font :system [\"Lato Medium\"]   :opentype \"lato-medium.ttf\"))
     (def lato-semibold (font :system [\"Lato Semibold\"] :opentype \"lato-semibold.ttf\"))

  or like this if multiple fonts are included as part of a single typeface:

  (def tnr-bold (font :system [\"Times New Roman\"] :generic :serif [:normal :400 :700))

  sources.  these may be a the names of system fonts local to the user, urls
  to remote font files, or data uris containing embedded fonts. each source
  may be thought of as its own typeface that may contain more than one font
  within a data structure such as:

    {:normal   {:400 {:regular <font Lato Regular>}}
               {:700 {:regular <font Lato Bold>
                      :italic  <font Lato Italic>}}
     :extended {:900 {:oblique <font Lato Extended Heavy Oblique}}}

  where the specific font can be obtained by a path of the form [<width>
  <weight> <slope>] as described in more detail later on. the browser will
  lazily attempt to load each typeface in succession it finds one with a format
  it supports that also contains the glyhps to satisfy the path's constraints.
  the source arguments themselves are key, value pairs where the keys are the
  font format and they values eith vector of names or uri.  source attributes
  may be one of the following:
  - :system   [<String name> ...]
  -  <format> <String, uri>

    format. the browser utilizes any format key other than `:system` as a hint
    to determine whether it should (down)load the associated source, and will
    avoid doing so if it is unrecognized or unsupported.  note that these formats
    differ from the extensions.  in most cases, a combination of the woff and
    woff2 formats will suffice for iedge 11 and up.
    - :woff (.woff)
    - :woff2 (.woff2)
    - :truetype (.ttf)
    - :opentype (.ttf, .otf)
    - :embedded-opentype (.eot)
    - :svg (.svg, .svgz)

    generic typefaces. it's a good practice, particularly if relying upon local
    fonts, to specify one of the generic font faces as the final fallback. these
    font families are:
    - :serif
    - :sans-serif
    - :cursive
    - :fantasy
    - :monospace

  path. the path is a vector of font descriptors in the form [<width> <weight>
  <slope>], used to select the appropriate font from a source that contains
  multiple fonts (an entire typeface or font family).  if the path or any of its
  discriptors are nil or ommitted, the default will be selected.

    width. must be one of the following keywords:
     - :ultra-condensed
     - :extra-condensed
     - :condensed
     - :semi-condensed
     - :normal (default)
     - :semi-expanded
     - :expanded
     - :extra-expanded
     - :ultra-expanded

     weight. must be one of the following:
     - :100 thin
     - :200 extra/ultra light
     - :300 light
     - :400 normal (default)
     - :500 medium
     - :600 semi/demi bold
     - :700 bold
     - :800 extra/ultra bold
     - :900 black/heavy

     slope. must be one of the following:
     - :regular (default)
     - :oblique
     - :italic

   notes. when relying on local fonts, that there's no way to guarantee that the
   font installed on the user's os is the one intended for use based on the name
   alone (font names are global in the truest sense of the word). it is best to
   specify font uris only if this guarantee is necessary.

   there's currently no support for creating compound fonts from multiple
   typefaces. support for the variant descriptors is also limited.

   everything you ever wanted to know about the way the browser handles fonts
   can be found here: https://www.w3.org/TR/css-fonts-3/"
  (let [widths   #{:ultra-condensed :extra-condensed :condensed :semi-condensed :normal :semi-expanded :expanded :extra-expanded :ultra-expanded}
        weights  #{:100 :200 :300 :400 :500 :600 :700 :800 :900}
        slopes   #{:regular :italic :oblique}
        formats  #{:system :generic :embedded-opentype :opentype :svg :truetype :woff :woff2}
        generics #{:serif :sans-serif :monospace :cursive :fantasy}
        [[width weight slope] sources] (if (odd? (count args)) [(last args) (drop-last args)] [nil args])
         sources (apply sorted-map sources)]
    (when width  (assert (widths  width)  (str "Error validating font width with value "  width)))
    (when weight (assert (weights weight) (str "Error validating font weight with value " weight)))
    (when slope  (assert (slopes  slope)  (str "Error validating font slope with value "  slope)))
    (doseq [[format value] sources]
      (assert (formats format) (str "Error validating font format with value " format))
      (when (= format :generic)
        (assert (generics value) (str "Error validating generic font value " value))))
    (Font. [(str (gensym "font-")) (or width :normal) (or weight :400) (or slope :regular)] sources)))
