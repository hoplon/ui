(ns hoplon.ui.value
  (:require
    [clojure.string :refer [join]]))

;; todo: support additional units

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IValue
  (-dom [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IValue
  (-dom [_]
    (if n (str (* (/ n d) 100) "%") "initial")))

(deftype Pixels [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IValue
  (-dom [_]
    (if v (str v "px") "initial")))

(deftype Evaluation [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " evaluation"))
  IValue
  (-dom [_]
    (if v (str "calc(" (join (nth v 0) (subvec v 1)) ")") "initial")))

(deftype Breakpoints [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " ratio"))
  IValue
  (-dom [_]
    (if v (str v "%") "intiial")))

(extend-type Keyword
  IValue
  (-dom [this]
    (name this)))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dom [v] (-dom v))

(defn ratio?       [v]   (instance?  Ratio      v))
(defn calculation? [v]   (instance?  Evaluation v))
(defn pixels?      [v]   (instance?  Pixels     v))
(defn value?       [v]   (satisfies? IValue     v))

(defn rt [n d] (Ratio. n d))
(defn px [v]   (Pixels.  v))
(defn kw [v]   (keyword  v))

(defn ev [& args] (Evaluation.  args))
(defn bp [& args] (Breakpoints. args))
