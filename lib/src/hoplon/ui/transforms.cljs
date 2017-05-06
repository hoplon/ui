(ns hoplon.ui.transforms)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pow [x e] (js/Math.pow x e))

;;; formulas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- linear --------------------------------------------------------------------;

(defn linear [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a linear
   equation in point-slope form."
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (/ (* c (- x d1)) d) r1))))

;-- quadratic-in -----------------------------------------------------------------;

(defn quadratic-in [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a
   quadratic equation in point-slope form."
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* c (pow (/ (- x d1) d) 2)) r1))))

;-- quadratic -----------------------------------------------------------------;


(defn quadratic-out [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a
   quadratic equation in point-slope form."
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* (- c)  (/ (- x d1) d) (- (/ (- x d1) d) 2)) r1))))
