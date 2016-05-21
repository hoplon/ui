(ns hoplon.ui.value
  (:require
    [clojure.string :refer [join]]
    [javelin.core   :refer [Cell]]))

;; todo: support additional units

;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IDOM
  (-model [_]))

;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Hexidecimal [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w "0x" (.toString v 16)))
  IDOM
  (-model [this]
    (str "#" (.toString v 16))))

(deftype Ratio [n d]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w n "/" d))
  IDOM
  (-model [_]
    (if n (str (* (/ n d) 100) "%") "initial")))

(deftype Pixels [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " pixels"))
  IDOM
  (-model [_]
    (if v (str v "px") "initial")))

(deftype Evaluation [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " evaluation"))
  IDOM
  (-model [_]
    (if v (str "calc(" (join (nth v 0) (subvec v 1)) ")") "initial")))

(deftype Breakpoints [v]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (write-all w v " ratio"))
  IDOM
  (-model [_]
    (if v (str v "%") "intiial")))

(extend-type Keyword
  IDOM
  (-model [this]
    (name this)))

; (extend-type Cell
;   IDOM
;   (-model [this]
;     (deref this))) ;; fix

; (extend-type js/Text
;   IDOM
;   (-model [this]
;     this))

;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn model [v] (-model v))

(defn ratio?       [v]   (instance?  Ratio      v))
(defn calculation? [v]   (instance?  Evaluation v))
(defn pixels?      [v]   (instance?  Pixels     v))
(defn model?       [v]   (satisfies? IDOM       v))
(defn hex?         [v]   (instance? Hexidecimal v))

(defn hx [v]   (Hexidecimal. v))
(defn rt [n d] (Ratio. n d))
(defn px [v]   (Pixels.  v))
(defn kw [v]   (keyword  v))

(defn ev [& args] (Evaluation.  args))
(defn bp [& args] (Breakpoints. args))
