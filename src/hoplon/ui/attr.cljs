(ns hoplon.ui.attr
  (:require-macros
    [javelin.core :refer [with-let]]))

;; todo: support additional attributes

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IAttribute
  (-to-dom [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Percent [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v "%"))
  IValue
  (-to-dom [_]
    (str v "%")))

(deftype Pixels [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IValue
  (-to-dom [_]
    (str v "px")))

(extend-type Keyword
  IValue
  (-to-dom [this]
    (str this)))

;;; constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn kw [v] (keyword  v))
(defn pc [v] (Percent. v))
(defn px [v] (Pixels.  v))

;;; validators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->px    [v]  (if (number? v) (str v "px") v))

(defn namekw [v & kws]
  (if (some #{v} (apply concat kws))
      (name v)
      (throw (js/Error. (str "Keyword value " v " invalid.")))))

(defn string [v]
  (if (or (= (subs v (- (count v) 1) (count v)) "%")
          (= (subs v (- (count v) 2) (count v)) "em"))
      v
      (throw (js/Error. (str "String value " v " invalid.")))))

;; todo: rgb colors
(defn ->color [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v colors globals)
    (number?  v) (str "#" (.toString v 16))
    :else        (throw (js/Error. (str "Color value " v " invalid.")))))

(defn ->len [v]
  (cond
    (nil?     v) "auto"
    (list?    v) (let [[op x y] v] (str "calc(" (->len x) " " op " " (->len y) ")"))
    (integer? v) (str    v "px")
    (number?  v) (str (* v 100) "%")
    (keyword? v) (namekw v lengths globals)
    (string?  v) (string v)
    :else        (throw (js/Error. (str "Length value " v " invalid.")))))

(defn ->cursor [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v cursors globals)
    :else        (throw (js/Error. (str "Cursor value " v " invalid.")))))

(defn ->decoration [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v decorations globals)
    :else        (throw (js/Error. (str "Decoration value " v " invalid.")))))

(defn ->style [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v styles globals)
    :else        (throw (js/Error. (str "Style value " v " invalid.")))))

(defn ->weight [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v weights globals)
    (integer? v)  v
    :else        (throw (js/Error. (str "Weight value " v " invalid.")))))

(defn ->halign [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v haligns globals)
    :else        (throw (js/Error. (str "Align value " v " invalid.")))))

(defn ->overflow [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v overflows globals)
    :else        (throw (js/Error. (str "Overflow value " v " invalid.")))))

(defn ->valign [v]
  (cond
    (nil?     v) "initial"
    (integer? v) (str    v "px")
    (number?  v) (str    v "%")
    (keyword? v) (namekw v valigns globals)
    (string?  v) (string  v)
    :else        (throw (js/Error. (str "Vertical align value " v " invalid.")))))

(defn ->font-family [v]
  (cond
    (nil?     v) "initial"
    (keyword? v) (namekw v globals)
    (string?  v) v
    :else        (throw (js/Error. (str "Font family value " v " invalid.")))))

(def globals [:initial :inherit])

(def lengths [:auto])

(def colors [:transparent :antiquewhite :aqua :aquamarine :azure :beige :bisque :black
             :blanchedalmond :blue :blueviolet :brown :burlywood :cadetblue :chartreuse
             :chocolate :coral :cornflowerblue :cornsilk :crimson :darkblue :darkcyan
             :darkgoldenrod :darkgray :darkgreen :darkgrey :darkkhaki :darkmagenta
             :darkolivegreen :darkorange :darkorchid :darkred :darksalmon :darkseagreen
             :darkslateblue :darkslategray :darkslategrey :darkturquoise :darkviolet
             :deeppink :deepskyblue :dimgray :dimgrey :dodgerblue :firebrick :floralwhite
             :forestgreen :fuchsia :gainsboro :ghostwhite :gold :goldenrod :gray :green
             :greenyellow :grey :honeydew :hotpink :indianred :indigo :ivory :khaki
             :lavender :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcoral
             :lightcyan :lightgoldenrodyellow :lightgray :lightgreen :lightgrey :lightpink
             :lightsalmon :lightseagreen :lightskyblue :lightslategray :lightslategrey
             :lightsteelblue :lightyellow :lime :limegreen :linen :maroon :mediumaquamarine
             :mediumblue :mediumorchid :mediumpurple :mediumseagreen :mediumslateblue
             :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue :mintcream
             :mistyrose :moccasin :navajowhite :navy :oldlace :olive :olivedrab :orangered
             :orchid :palegoldenrod :palegreen :paleturquoise :palevioletred :papayawhip
             :peachpuff :peru :pink :plum :powderblue :purple :rebeccapurple :red
             :rosybrown :royalblue :saddlebrown :salmon :sandybrown :seagreen :seashell
             :sienna :silver :skyblue :slateblue :slategray :slategrey :snow :springgreen
             :steelblue :tan :teal :thistle :tomato :turquoise :violet :wheat :white
             :whitesmoke :yellow :yellowgreen])

(def cursors [:alias :all-scroll :auto :cell :context-menu :col-resize :copy
              :crosshair :default :e-resize :ew-resize :grab :grabbing :help :move :n-resize
              :ne-resize :nesw-resize :ns-resize :nw-resize :nwse-resize :no-drop :none
              :not-allowed :pointer :progress :row-resize :s-resize :se-resize :sw-resize
              :text :vertical-text :w-resize :wait :zoom-in :zoom-out])

(def decorations [:none :underline :overline :line-through])
(def styles      [:normal :italic :oblique])
(def weights     [:normal :bold :bolder :lighter :number])
(def kernings    [:auto :normal :none])

(def haligns [:left :right :center :justify])
(def valigns [:top :middle :bottom :baseline :sub :super :text-top :text-bottom])

(def floats    [:left :right])
(def overflows [:visible :hidden :scroll :auto])
