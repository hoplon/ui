(ns hoplon.ui.transforms)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pow [x e] (js/Math.pow x e))
(defn sin [x] (js/Math.sin x))
(defn cos [x] (js/Math.cos x))
(def pi js/Math.PI)

;;; formulas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- linear --------------------------------------------------------------------;

(defn linear
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a linear
   equation in point-slope form."
  [[d1 d2] [r1 r2]]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (/ (* c (- x d1)) d) r1))))

;-- quadratic --------------------------------------------------------------;

(defn quadratic-in [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a positive quadratic
   function"
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* c (pow (/ (- x d1) d) 2)) r1))))

(defn quadratic-out [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a negative quadratic
   function"
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* (- c)  (/ (- x d1) d) (- (/ (- x d1) d) 2)) r1))))

(defn quadratic-in-out [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
  value in that domain to the corresponding range using a negative quadratic function"
    (let [c (- r2 r1)
          d (- d2 d1)]
      (fn [x]
        (if (< x (+ d1 (/ d 2)))
            (+ (* (/ c 2) (pow (/ (- x d1) (/ d 2))2)) r1)
            (+ (* (- (/ c 2)) (- (* (- (/ (- x d1) (/ d 2)) 1) (- (- (/ (- x d1) (/ d 2)) 1) 2)) 1)) r1)))))

;-- cubic ---------------------------------------------------------------------;

(defn cubic-in [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a 3rd degree polynomial
   function"
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* c (pow (/ (- x d1) d) 3)) r1))))

(defn cubic-out [[d1 d2] [r1 r2]]
 "given a domain and a range, return a function that will translate any given
  value in that domain to the corresponding range using a 3rd degree polynomial
  function"
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (* c (+ (pow (- (/ (- x d1) d) 1) 3) 1)) r1))))

;-- cubic-in-out -------------------------------------------------------------;
;;; need if loops for this one ;;;
;(defn cubic-in-out [[d1 d2] [r1 r2]]
;  "given a domain and a range, return a function that will translate any given
;  value in that domain to the corresponding range using a negative quadratic function"
;    (let [c (- r2 r1)
;          d (- d2 d1)]
;        ))
;

;-- quartic-in --------------------------------------------------------------;

(defn quartic-in [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a 4th degree polynomial
   function"
   (let [c (- r2 r1)
         d (- d2 d1)]
     (fn [x] (+ (* c (pow (/ (- x d1) d) 4)) r1))))

;-- quartic-out --------------------------------------------------------------;

(defn quartic-out [[d1 d2] [r1 r2]]
 "given a domain and a range, return a function that will translate any given
  value in that domain to the corresponding range using a 4th degree polynomial
  function"
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (* (- c) (- (pow (- (/ (- x d1) d) 1) 4) 1)) r1))))

    ;-- quartic-in-out -------------------------------------------------------------;
    ;;; need if loops for this one ;;;
    ;(defn cubic-in-out [[d1 d2] [r1 r2]]
    ;  "given a domain and a range, return a function that will translate any given
    ;  value in that domain to the corresponding range using a negative quadratic function"
    ;    (let [c (- r2 r1)
    ;          d (- d2 d1)]
    ;        ))
    ;


;-- quintic-in --------------------------------------------------------------;

(defn quintic-in [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a 5th degree polynomial
   function"
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (* c (pow (/ (- x d1) d) 5)) r1))))

;-- quintic-out --------------------------------------------------------------;

(defn quintic-out [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a 5th degree polynomial
   function"
  (let [c (- r2 r1)
        d (- d2 d1)]
    (fn [x] (+ (* c (+ (pow (- (/ (- x d1) d) 1) 5) 1)) r1))))


    ;-- quintic-in-out -------------------------------------------------------------;
    ;;; need if loops for this one ;;;
    ;(defn cubic-in-out [[d1 d2] [r1 r2]]
    ;  "given a domain and a range, return a function that will translate any given
    ;  value in that domain to the corresponding range using a negative quadratic function"
    ;    (let [c (- r2 r1)
    ;          d (- d2 d1)]
    ;        ))
    ;

;-- sine-in -------------------------------------------------------------------;

(defn sine-in [[d1 d2][r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range using a sine function"
   (let [c (- r2 r1)
         d (- d2 d1)]
      (fn [x] pi)))
