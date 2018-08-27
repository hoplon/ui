(ns hoplon.ui.interpolators)

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pi  js/Math.PI)
(def pow js/Math.pow)
(def sin js/Math.sin)
(def cos js/Math.cos)

;;; transformations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- linear --------------------------------------------------------------------;

(defn linear
  "given a domain and a range, translate a value from that domain to the
   corresponding range. implemented by calculating the slope between (d1, r1)
   and (d2, r2) and substituting it into a linear equation in point-slope form."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (/ (* c (- x d1)) d) r1)))

;-- quadratic -----------------------------------------------------------------;

(defn quadratic-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a positive quadratic function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (pow (/ (- x d1) d) 2)) r1)))

(defn quadratic-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a negative quadratic function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* (- c)  (/ (- x d1) d) (- (/ (- x d1) d) 2)) r1)))

(defn quadratic
  "given a domain and a range, translate a value from that domain to the
   corresponding range using the quadratic functions."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (if (< x (+ d1 (/ d 2)))
      (+ (* (/ c 2) (pow (/ (- x d1) (/ d 2))2)) r1)
      (+ (* (- (/ c 2)) (- (* (- (/ (- x d1) (/ d 2)) 1) (- (- (/ (- x d1) (/ d 2)) 1) 2)) 1)) r1))))

;-- cubic ---------------------------------------------------------------------;

(defn cubic-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 3rd degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (pow (/ (- x d1) d) 3)) r1)))

(defn cubic-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 3rd degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (+ (pow (- (/ (- x d1) d) 1) 3) 1)) r1)))

(defn cubic
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 3rd degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (if (< x (+ d1 (/ d 2)))
      (+ (* (/ c 2) (pow (/ (- x d1) (/ d 2)) 3)) r1)
      (+ (* (/ c 2) (+ (pow (- (/ (- x d1) (/ d 2)) 2) 3) 2)) r1))))

;-- quartic -------------------------------------------------------------------;

(defn quartic-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 4th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (pow (/ (- x d1) d) 4)) r1)))

(defn quartic-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 4th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* (- c) (- (pow (- (/ (- x d1) d) 1) 4) 1)) r1)))

(defn quartic
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 4th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (if (< x (+ d1 (/ d 2)))
      (+ (* (/ c 2) (pow (/ (- x d1) (/ d 2)) 4)) r1)
      (+ (* (- (/ c 2)) (- (pow (- (/ (- x d1) (/ d 2)) 2) 4) 2)) r1))))

;-- quintic -------------------------------------------------------------------;

(defn quintic-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 5th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (pow (/ (- x d1) d) 5)) r1)))

(defn quintic-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 5th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (+ (pow (- (/ (- x d1) d) 1) 5) 1)) r1)))

(defn quintic
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a 5th degree polynomial function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (if (< x (+ d1 (/ d 2)))
      (+ (* (/ c 2) (pow (/ (- x d1) (/ d 2)) 5)) r1)
      (+ (* (/ c 2) (+ (pow (- (/ (- x d1) (/ d 2)) 2) 5) 2)) r1))))

;-- sine ----------------------------------------------------------------------;

(defn sine-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a sine function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
     (+ (* (- c) (cos (* (/ (- x d1) d) (/ pi 2)))) c r1)))

(defn sine-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a sine function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (sin (* (/ (- x d1) d) (/ pi 2)))) r1)))

(defn sine
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a sine function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* (/ (- c) 2) (- (cos (* (/ (- x d1) d) pi)) 1)) r1)))

;-- exponential ---------------------------------------------------------------;

(defn exp-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using an exponential function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* c (pow 2 (* 10 (- (/ (- x d1) d) 1)))) r1)))

(defn exp-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using an exponential function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
   (+ (* c (+ (- (pow 2 (* (- 10) (/ (- x d1) d)))) 1)) r1)))

(defn exp
  "given a domain and a range, translate a value from that domain to the
   corresponding range using an exponential function."
 [d1 d2 r1 r2 x]
 (let [c (- r2 r1)
       d (- d2 d1)]
   (if (< x (+ d1 (/ d 2)))
      (+ (* (/ c 2) (pow 2 (* 10 (- (/ (- x d1) (/ d 2))1)))) r1)
      (+ (* (/ c 2) (+ (- (pow 2 (* (- 10) (- (/ (- x d1) (/ d 2)) 1))))2)) r1))))

;-- circular ------------------------------------------------------------------;

(defn circ-in
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a circular function."
  [d1 d2 r1 r2 x]
  (let [c (- r2 r1)
        d (- d2 d1)]
    (+ (* (- c) (- (pow (- 1 (pow (/ (- x d1) d) 2)) 0.5) 1)) r1)))

(defn circ-out
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a circular function."
   [d1 d2 r1 r2 x]
   (let [c (- r2 r1)
         d (- d2 d1)]
     (if (< x d1)
       r1
       (+ (* c (pow (- 1 (pow (- (/ (- x d1) d) 1) 2)) 0.5)) r1))))

(defn circ
  "given a domain and a range, translate a value from that domain to the
   corresponding range using a circular function."
 [d1 d2 r1 r2 x]
 (let [c (- r2 r1)
       d (- d2 d1)]
    (if (< x d1)
      r1
      (if (< x (+ d1 (/ d 2)))
        (+ (* (/ (- c) 2) (- (pow (- 1 (pow (/ (- x d1) (/ d 2)) 2)) 0.5)1)) r1)
        (+ (* (/ c 2) (+ (pow (- 1 (pow (- (/ (- x d1) (/ d 2)) 2) 2)) 0.5) 1)) r1)))))
