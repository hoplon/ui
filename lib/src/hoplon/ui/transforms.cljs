(ns hoplon.ui.transforms)

(defn linear [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a linear
   equation in point-slope form."
  (let [m (/ (- r2 r1) (- d2 d1))]
    (fn [x] (+ (* m (- x d1)) r1))))
