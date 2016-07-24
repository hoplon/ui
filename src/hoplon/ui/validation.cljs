(ns hoplon.ui.validation
  (:require
    [hoplon.ui.attrs :as a]))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def globals     [:initial :inherit])
(def adjusts     [:none])
(def aligns      [:beg :mid :end])
(def haligns     [:jst])
(def valigns     [:baseline :sub :super :text-top :text-bottom])
(def colors      [:transparent :antiquewhite :aqua :aquamarine :azure :beige
                  :bisque :black :blanchedalmond :blue :blueviolet :brown
                  :burlywood :cadetblue :chartreuse :chocolate :coral
                  :cornflowerblue :cornsilk :crimson :darkblue :darkcyan
                  :darkgoldenrod :darkgray :darkgreen :darkgrey :darkkhaki
                  :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred
                  :darksalmon :darkseagreen :darkslateblue :darkslategray
                  :darkslategrey :darkturquoise :darkviolet :deeppink
                  :deepskyblue :dimgray :dimgrey :dodgerblue :firebrick
                  :floralwhite :forestgreen :fuchsia :gainsboro :ghostwhite
                  :gold :goldenrod :gray :green :greenyellow :grey :honeydew
                  :hotpink :indianred :indigo :ivory :khaki :lavender
                  :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcoral
                  :lightcyan :lightgoldenrodyellow :lightgray :lightgreen
                  :lightgrey :lightpink :lightsalmon :lightseagreen
                  :lightskyblue :lightslategray :lightslategrey :lightsteelblue
                  :lightyellow :lime :limegreen :linen :maroon :mediumaquamarine
                  :mediumblue :mediumorchid :mediumpurple :mediumseagreen
                  :mediumslateblue :mediumspringgreen :mediumturquoise
                  :mediumvioletred :midnightblue :mintcream :mistyrose :moccasin
                  :navajowhite :navy :oldlace :olive :olivedrab :orange :orangered
                  :orchid :palegoldenrod :palegreen :paleturquoise
                  :palevioletred :papayawhip :peachpuff :peru :pink :plum
                  :powderblue :purple :rebeccapurple :red :rosybrown :royalblue
                  :saddlebrown :salmon :sandybrown :seagreen :seashell :sienna
                  :silver :skyblue :slateblue :slategray :slategrey :snow
                  :springgreen :steelblue :tan :teal :thistle :tomato :turquoise
                  :violet :wheat :white :whitesmoke :yellow :yellowgreen])
(def cursors     [:alias :all-scroll :auto :cell :context-menu :col-resize :copy
                  :crosshair :default :e-resize :ew-resize :grab :grabbing :help
                  :move :n-resize :ne-resize :nesw-resize :ns-resize :nw-resize
                  :nwse-resize :no-drop :none :not-allowed :pointer :progress
                  :row-resize :s-resize :se-resize :sw-resize :text
                  :vertical-text :w-resize :wait :zoom-in :zoom-out])
(def decorations [:none :underline :overline :line-through])
(def capitalizes [:none :uppercase :lowercase :capitalize])
(def families    [:serif :sans-serif :monospace :cursive :fantasy])
(def kernings    [:auto :normal :none])
(def lengths     [:auto])
(def renderings  [:auto :optimizeSpeed :optimizeLegibility :geometricPrecision]) ;; todo: dash instead of camelcase
(def sizes       [:xx-small :x-small :small :medium :large :x-large :xx-large :larger :smaller])
(def smoothings  [:none :antialiased :subpixel-antialiased])
(def spacings    [:normal])
(def stretches   [:ultra-condensed :extra-condensed :condensed :semi-condensed :normal :semi-expanded :expanded :extra-expanded :ultra-expanded])
(def styles      [:normal :italic :oblique])
(def syntheses   [:none :weight :style :weight-style])
(def boxes       [:border :fill :view])
(def origins     [:left :right :top :bottom :center])
(def txstyles    [:preserve-3d :flat])
(def overflows   [:visible :hidden :scroll :auto])
(def weights     [:normal :bold :bolder :lighter :100 :200 :300 :400 :500 :600 :700 :800 :900])

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in? [v & kwvecs] (some #{v} (apply concat kwvecs)))

;;; validation fns ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adjust? [v]
  (cond (keyword? v) (in? v adjusts globals)
        (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
        (nil?     v) :initial
        :else        false))

(defn align? [v]
  (cond (keyword? v) (in? v aligns lengths globals)
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn alignh? [v]
  (cond (keyword? v) (in? v aligns haligns lengths globals)
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn alignv? [v]
  (cond (keyword? v) (in? v aligns valigns lengths globals)
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn callback? [v]
  (cond (fn? v)   v
        (nil?     v) true
        :else        false))

(defn color? [v]
  (cond (keyword? v) (in? v colors globals)
        (a/color? v) v
        (nil?     v) :initial
        :else        false))

(defn cursor? [v]
  (cond (keyword? v) (in? v cursors globals)
        (nil?     v) :inital
        :else        false))

(defn dock? [v]
  (cond (keyword?  v) (in? v lengths globals)
        (a/calc?   v) v
        (a/ratio?  v) v
        (a/ems?    v) v
        (a/points? v) v
        (number?   v) v
        (true?     v) v
        (false?    v) true
        (nil?      v) :initial
        :else         false))

(defn family? [v]
  (cond (vector?  v) (every? identity v)
        (keyword? v) (in? v families globals)
        (string?  v) v
        (nil?     v) :initial
        :else        false))

(defn kerning? [v]
  (cond (keyword? v) (in? v kernings globals)
        (nil?     v) :initial
        :else        false))

(defn length? [v]
  (cond (keyword?  v) (in? v lengths globals)
        (a/calc?   v) v
        (a/ratio?  v) v
        (a/ems?    v) v
        (a/points? v) v
        (number?   v) v
        (nil?      v) :initial
        :else         false))

(defn opacity? [v]
  (cond (keyword? v) (in? v globals)
        (number?  v) (and (>= v 0) (<= v 1)) ;; todo: make a ratio
        (nil?     v) :initial
        :else        false))

(defn overflow? [v]
  (cond (keyword? v) (in? v overflows globals)
        (nil?     v) :initial
        :else        false))

(defn rendering? [v]
  (cond (keyword? v) (in? v renderings globals)
        (nil?     v) :initial
        :else        false))

(defn size? [v] ;; todo: support other units
  (cond (keyword?  v) (in? v sizes globals)
        (a/ratio?  v) v
        (number?   v) v
        (a/ems?    v) v
        (a/points? v) v
        (nil?      v) :initial
        :else         false))

(defn shadow? [v]
  (cond (vector?   v) (every? a/shadow? v)
        (keyword?  v) (in? v globals)
        (a/shadow? v) v
        (nil?      v) :initial
        :else         false))

(defn smoothing? [v]
  (cond (keyword? v) (in? v smoothings globals)
        (nil?     v) :initial
        :else        false))

(defn spacing? [v]
  (cond (keyword? v) (in? v spacings globals)
        (a/ratio? v) v
        (number?  v) v
        (nil?     v) :initial
        :else        false))

(defn stretch? [v]
  (cond (keyword? v) (in? v stretches globals)
        (nil?     v) :initial
        :else        false))

(defn style? [v]
  (cond (keyword? v) (in? v styles globals)
        (nil?     v) :initial
        :else        false))

(defn synthesis? [v]
  (cond (keyword? v) (in? v syntheses globals)
        (nil?     v) :initial
        :else        false))

(defn transform? [v]
  (cond (keyword?     v) (in? v globals)
        (a/transform? v) v
        (nil?         v) :initial
        :else            false))

(defn origin? [v]
  (cond (keyword?     v) (in? v origins globals)
        ;(a/ratio? v)      :initial
        (nil?         v) :initial
        :else            false))

(defn box? [v]
  (cond (keyword? v) (in? v boxes globals)
        (nil?     v) :initial
        :else        false))

(defn txstyle? [v]
  (cond (keyword? v) (in? v txstyles globals)
        (nil?     v) :initial
        :else        false))

(defn capitalize? [v]
  (cond (keyword? v) (in? v capitalizes globals)
        (nil?     v) :initial
        :else        false))

(defn decoration? [v]
  (cond (keyword? v) (in? v decorations globals)
        (nil?     v) :initial
        :else        false))

(defn weight? [v]
  (cond (keyword? v) (in? v weights globals)
        (nil?     v) :initial
        :else        false))
