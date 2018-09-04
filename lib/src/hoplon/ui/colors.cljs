(ns hoplon.ui.colors
  "colors are represented as javascript numbers using the red, blue, green, and
   alphatransparent cubic coordinates. each dimension is encoded into its own
   byte so that the color can be treated as an unsigned 32-bit integer of the
   form RRGGBBAA. a few examples follow:

   0xFFFFFFFF white
   0xFFFFFF   light blue instead of white
   0xFF       black instead of dark blue
   0          invisible (default)"
  (:require
    [hoplon.ui.utils :refer [pad]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def abs   js/Math.abs)
(def atan2 js/Math.atan2)
(def pow   js/Math.pow)
(def sqrt  js/Math.sqrt)
(def floor js/Math.floor)

(defn Uint32 [int]
  (unsigned-bit-shift-right int 0))

(defn byte-conj [int byte]
  (Uint32 (bit-or (bit-shift-left int 8) byte)))

(def byte-get
  (let [masks [0x000000FF 0x0000FF00 0x00FF0000 0xFF000000]]
    (fn [int idx]
      (unsigned-bit-shift-right (bit-and int (masks idx)) (* idx 8)))))

(def byte-set
  (let [masks [0xFFFFFF00 0xFFFF00FF 0xFF00FFFF 0x00FFFFFF]]
    (fn [int idx byte]
      (Uint32 (bit-or (bit-and int (masks idx)) (bit-shift-left byte (* idx 8)))))))

(defn rgb-points [h' c x] ;; points not 255 like min, max
  (let [lookup [[c x 0] [x c 0] [0 c x] [0 x c] [x 0 c] [c 0 x]]]
    (lookup (mod (floor h') 6))))

(defn color-with [f points a]
  (->> (conj (mapv f points) (or a 1))
       (reduce #(byte-conj % (* 0xFF %2)) 0)))

(defn min-dist
  [color]
  (loop [i 1 d 0xFF]
    (if (> i 3) d
      (let [d' (byte-get color i)]
        (recur (inc i) (if (< d' d) d' d))))))

(defn max-dist
  [color]
  (loop [i 1 d 0x00]
    (if (> i 3) d
      (let [d' (byte-get color i)]
        (recur (inc i) (if (> d' d) d' d))))))

(defn sum-dist
  [color]
  (loop [i 1 d 0x00]
    (if (> i 3) d
      (let [d' (byte-get color i)]
        (recur (inc i) (+ d d'))))))

;;; getters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hue
  "the hue is the proportion of the distance around the edge of the hexigonal
   projection measured in degrees."
  [color]
  (let [r (/ (byte-get color 3) 0xFF)
        g (/ (byte-get color 2) 0xFF)
        b (/ (byte-get color 1) 0xFF)
        m (max r g b)
        n (min r g b)
        c (- m n)]
    (-> (cond
          (= c 0) 0
          (= m r) (mod (/ (- g b) c) 6)
          (= m g) (+ (/ (- b r) c) 2)
          (= m b) (+ (/ (- r g) c) 4))
        (* 60)
        (js/Math.round))))

(defn chroma
  "the chroma is the distance from the origin of a circularly-warped hexigonal
   projection of the tilted rgb cube to a projection of the given color's point
   in the same rgb space into the same circle. it is universal to all of the
   cylindrical models."
  [color]
  (let [r (/ (byte-get color 3) 0xFF)
        g (/ (byte-get color 2) 0xFF)
        b (/ (byte-get color 1) 0xFF)
        m (max r g b)
        n (min r g b)]
    (- m n)))

(def luma
  (let [weight [nil 0.114 0.587 0.299]]
    (fn [color]
      (loop [i 1 v 0]
        (if (> i 3) (/ v 0xFF)
          (recur (inc i) (+ (* (byte-get color i) (weight i)) v)))))))

(defn alpha
  "the degree to which the color should be composited with its backround to
   create the appearance of transparency."
  [color]
  (/ (byte-get color 0) 0xFF))

(defn value
  "lightness in the hsv (hexicone) model"
  [color]
  (/ (max-dist color) 255))

(defn saturation-hsv
  [color]
  (let [v (value color)]
   (if (= 0 v) 0 (/ (chroma color) v))))

(defn lightness
  "lightness in the hsl (bihexicone) model"
  [color]
  (/ (+ (/ (min-dist color) 255) (/ (max-dist color) 255)) 2))

(defn saturation-hsl
  [color]
  (let [l (lightness color)]
    (if (= 1 l) 0 (/ (chroma color) (- 1 (abs (- (* 2 l) 1)))))))

(defn intensity
  [color]
  (/ (sum-dist color) 255 3))

(defn saturation-hsi
  "does not attempt to fill"
  [color]
  (let [i (intensity color)]
    (if (= 0 i) 0 (- 1 (/ (min-dist color) 255 i)))))

;;; constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- cubic ---------------------------------------------------------------------;

(defn hex
  "construct a color from a string of cubic coordinates where leading zeros are
   preserved to resolve ambigutity between the traditional 3, 6, and 8 nibble
   representations.

   0 color. a string in one of the following formats:
   - <String RGB>      expands to 3-byte by duplicating each nibble
   - <String RRGGBB>   expands to 4-byte by appending the default alpha \"FF\"
   - <String RRGGBBAA> same as 0xRRGGBBAA

   this function is provided for convenience and completeness of the api.  in
   practice, it's usually better to declare cubic colors directly using clojure
   hexidecimals (0xRRGGBBAA)."
  [color]
  (case (count color)
    3 (hex (reduce #(str % %2 %2) "" color))
    6 (hex (str color "FF"))
    8 (js/parseInt color 16)
      (throw (ex-info (str "invalid number of digits in color " color ".") {}))))

(defn ->hex [color] (pad (.toString color 16) 8))

(defn rgb
  "construct a color from the following cubic coordinates.

   0 red <Number Byte>. an integer between 0 and 255 (0x00 - 0xFF)

   1 blue <Number Byte>. an integer between 0 and 255 (0x00 - 0xFF)

   2 green <Number Byte>. an integer between 0 and 255 (0x00 - 0xFF)

   3 alpha <Number Float Percent>. a floating point number between 0.0 and 1.0
   that defaults to 1 if none is specified"
  [r g b & [a]]
  (reduce byte-conj (conj (vector r g b) (floor (* (or a 1) 0xFF)))))

(defn ->rgb [color] (mapv (partial byte-get color) [3 2 1 0]))

;-- prism ---------------------------------------------------------------------;

(defn hcl
  "construct a color from the following coordinates of the luma/chroma/hue
   model:

   0 hue <Number Angle>. the angle to the the color on the cylindrical rainbow
   where the top of the circle is red. positive numbers change the color in a
   clockwise direction, while negative numbers reverse direction. degrees beyond
   -359 to 359 merely continue around the cylinder in the appropriate direction.

   1 chroma <Number Percentage>. the tone, or amount of grey, from 0 to 1.

   2 lumina <Number Percentage>. the tint, or amount of white, from 0 to 1.
   computed by taking the average of the rgb values.

   3 alpha <Number Percentage>. the opacity, which determines how the color
   should composite with its background, as a number between 0 and 1."
  [h c l & [a]]
  (let [h' (/ h 60)
        z  (- 1 (abs (- (mod h' 2) 1)))
        x  (* c z)
        [r g b :as rgb] (rgb-points h' c x)
        m  (- l (+ (* 0.3 r) (* 0 0.59 g) (* 0 0.11 b)))]
    (color-with (partial + m) rgb a)))

(defn ->hcl [color] ((juxt hue chroma luma alpha) color))

;-- cylindrical ---------------------------------------------------------------;

(defn hsv
  "construct a color from the following coordinates of the double hexcone model:

   0 hue <Number/Angle>. the angle to the the color on the cylindrical rainbow
   where the top of the circle is red. positive numbers change the color in a
   clockwise direction, while negative numbers reverse direction. degrees beyond
   -359 to 359 merely continue around the cylinder in the appropriate direction.

   1 saturation <Number/Percentage>. the tone, or amount of grey, from 0 to 1.

   2 value <Number/Percentage>. the tint, or amount of white, from 0 to 1.
   computed by taking the maximum of r, g, and b.

   3 alpha <Number/Percentage>. the opacity, which determines how the color
   should composite with its background, as a number between 0 and 1.

   hsv is sometimes called hsb, where the b stands for brightness."
  [h s v & [a]]
  (let [h' (/ h 60)
        c  (* s v)
        x  (* c (- 1 (abs (- (mod h' 2) 1))))
        m  (- v c)]
   (color-with (partial + m) (rgb-points h' c x) a)))

(defn ->hsv [color] ((juxt hue saturation-hsv value alpha) color))

(defn hsl
  "construct a color from the following coordinates of the hexcone model:

   0 hue <Number Angle>. the angle to the the color on the cylindrical rainbow
   where the top of the circle is red. positive numbers change the color in a
   clockwise direction, while negative numbers reverse direction. degrees beyond
   -359 to 359 merely continue around the cylinder in the appropriate direction.

   1 saturation <Number/Percentage>. the tone, or amount of grey, from 0 to 1.

   2 luminocity <Number/Percentage>. computed by averaging the min and max rgb
   values.

   3 alpha <Number/Percentage>. the opacity, which determines how the color
   should composite with its background, as a number between 0 and 1.

   hsv is sometimes called hsv, where the v stands for value."
  [h s l & [a]]
  (let [h' (/ h 60)
        c  (* (- 1 (abs (- (* 2 l) 1))) s)
        x  (* c (- 1 (abs (- (mod h' 2) 1))))
        m  (- l (/ c 2))]
    (color-with (partial + m) (rgb-points h' c x) a)))

(defn ->hsl [color] ((juxt hue saturation-hsl lightness alpha) color))

(defn hsi
  "construct a color from the following coordinates of the cylindrical system:

   0 hue <Number/Angle>. the angle to the the color on the cylindrical rainbow
   where the top of the circle is red. positive numbers change the color in a
   clockwise direction, while negative numbers reverse direction. degrees beyond
   -359 to 359 merely continue around the cylinder in the appropriate direction.

   1 saturation <Number/Percentage>. the tone, or amount of grey, from 0 to 1.

   2 intensity <Number/Percentage>. the tint, or amount of white, from 0 to 1.
   computed by taking the average of the rfgb values.

   3 alpha <Number/Percentage>. the opacity, which determines how the color
   should composite with its background, as a number between 0 and 1."
  [h s i & [a]]
  (let [h' (/ h 60)
        z  (- 1 (abs (- (mod h' 2) 1))) ;; part of x
        c  (/ (* 3 i s) (+ 1 z)) ;unique
        x  (* c z)
        m  (* i (- 1 s))]
    (color-with (partial + m) (rgb-points h' c x) a)))

(defn ->hsi [color] ((juxt hue saturation-hsi intensity alpha) color))

;;; type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type number
  ILookup
  (-lookup [color k]
    (case k
      :r  (byte-get       color 3)
      :g  (byte-get       color 2)
      :b  (byte-get       color 1)
      :a  (alpha          color)
      :h  (hue            color)
      :c  (chroma         color)
      :y  (luma           color)
      :s  (saturation-hsl color)
      :l  (lightness      color)
      :sv (saturation-hsv color)
      :v  (value          color)
      :si (saturation-hsi color)
      :i  (intensity      color)))
  IAssociative
  (-assoc [color k v]
    (case k
      :r  (byte-set color 3 v)
      :g  (byte-set color 2 v)
      :b  (byte-set color 1 v)
      :a  (byte-set color 0 (* v 0xFF))
      ; :h  (hcl v           (chroma color)         (value color)) ;todo
      :c  (hcl (hue color) v                      (luma  color)     (alpha color))
      :y  (hcl (hue color) (chroma v)             v                 (alpha color))
      :h  (hsl v           (saturation-hsl color) (lightness color) (alpha color))
      :s  (hsl (hue color) v                      (lightness v)     (alpha color))
      :l  (hsl (hue color) (saturation-hsl v)     v                 (alpha color))
      :sv (hsv (hue color) v                      (value color)     (alpha color))
      :v  (hsv (hue color) (saturation-hsv color) v                 (alpha color))
      :si (hsi (hue color) v                      (intensity v)     (alpha color))
      :i  (hsi (hue color) (saturation-hsi v)     v                 (alpha color))))
  IFn
  (-invoke [color k]
    (-lookup color k)))

(defn color
  "construct a color from any three coordinates, and an optional alpha, that
   make sense together."
  [& coords]
  (apply assoc coords))

;;; schemes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn scheme
  "construct a color scheme that includes the supplied color and additional
   color for each hue interval supplied.

   0 color <Number/Color>. the base color and starting point for the scheme.

   n intervals ...<Number/Angle>. the intervals, in degrees from the hue of the
   base color and then the previously generated color."
  [color & intervals]
  (reduce #(conj % (update (peek %) :h (partial + %2))) [color] intervals))

(defn complementary
  "the corresponding color that will cancel out the argument"
  [color] (scheme color 180))

(defn split-complementary
  [color] (scheme color 165 195))

(defn triadic
  [color] (scheme color 120 120))

(defn quadradic
  [color] (scheme color 90 90 90 90))

(defn analogic
  [color] (scheme color 15 15))

(defn tetradic
  [color] (scheme color 30 60 30 60))

(defn tint ;todo
  "mix white with the color"
  [color] (+ color 0x112B))

(defn shade ;todo
  "add black to the color"
  [color] (+ color 0x112B))

(defn tone
  [color] (+ color 0x112B))
