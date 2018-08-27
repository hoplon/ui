(ns hoplon.ui.effects)

;;; gradients ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lgr
  "construct a linear gradient along a geometric vector that originates at the
   edge of the area to which the gradient is applied and transitions between
   the color stops in the direction of the orientation.
   
   orientation. the number of degrees to rotate the gradient vector about the
   center of the area from its default twelve o'clock orientation. positive
   numbers rotate the gradient vector infinitely clockwise, while negative
   numbers move it in a counterclockwise direction.
   - <Number/Angle> -720, -360, 0, 360, and 720 are all the same
   - <Keyword> :rtl :ltr :ttb :btt

   color-stops ...<Number/Color> | [<Number/Color> <Number/Position>]. a sequence of
   colors and/or colors paired with positions. colors will be distributed evenly
   along the gradient (geometric) vector, while the unadulterated colors taken
   from each pair will be focused at the specified distance from the vector's
   origin and direction." 
  ; todo: gradients need to return functions with a reference to the container
  ; to facilitate positions based the parenting container size:
  ; (lgr 90 0xFFFFFFFF [0x00000000 #(/ (:bh %) 2)])
  [orientation & color-stops]
  {:gradient "linear" :orientation orientation :color-stops color-stops})

(defn rgr
  "construct a repeating linear gradient from an angle, colors, and color-stops.
   the angle my be expressed as a number in degrees or as a percentage in turns.
   the remaining arguments are colors optionally paired with stops as either
   lengths or percentages that specify where the transitions should occur."
  [angle & colors]
  {:gradient "repeating-linear" :angle angle :colors colors})

(defn cgr 
  "circular gradient"
  [{:keys [p ph pv e]} & colors]
  {:gradient "circle" :e (name e) :ph (or p ph) :pv (or p pv) :colors colors})

(defn egr 
  "eliptical gradient"
  [{:keys [p pv ph e]} & colors]
  {:gradient "elipse" :e (name e) :ph (or p ph) :pv (or p pv) :colors colors})

;;; shadows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sdw
  "construct a shadow."
  [x y color & [blur spread inset]]
  {:shadow "box" :x x :y y :color color :blur blur :spread spread :inset inset})
