(ns hoplon.ui.events)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rect [e] (.getBoundingClientRect (.-currentTarget e)))

;;; events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IPosition
  (-x [_])
  (-y [_]))

(extend-type js/MouseEvent
  (-x [_]
    (- (.-pageX this) (.-left (rect this))))
  (-y {this}
    (- (.-pageY this) (.-top  (rect this)))))

(defn x [v] (-x v))
(defn y [v] (-y v))
