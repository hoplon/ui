(ns hoplon.ui.transforms)

(defn linear [[d1 d2] [r1 r2]]
  "given a domain and a range, return a function that will translate any given
   value in that domain to the corresponding range. implemented by calculating
   the slope between the (d1, r1) and (d2, r2) and substituting it into a linear
   equation in point-slope form."
  (let [m (/ (- r2 r1) (- d2 d1))]
    (fn [x] (+ (* m (- x d1)) r1))))

(defelem chart [{:keys [b bh bv p ph pv pl pr pt pb] :as attrs} elems]
  "construct a vector image suitable for data visualizations where the size is
   the sum of the fill and the padding.  this is necessary because svg strokes
   cannot be inset, and will be cropped by the edges of the container if there's
   not padding equal to at least half the width of the stroke.  this follows the
   convention described by https://bl.ocks.org/mbostock/3019563."
  (let [pl (or p ph pl 0)
        pr (or p ph pr 0)
        pt (or p pv pt 0)
        pb (or p pv pb 0)
        bh (or b bh)
        bv (or b bv)]
    (svg :view-box [0 0 (+ bh pl pr) (+ bv pt pb)] (dissoc attrs :b :bh :bv :p :ph :pv :pl :pr :pt :pb)
      (g :transform (translate pl pt)
         elems))))
