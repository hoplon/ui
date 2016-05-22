(ns hoplon.ui.elems)

(defmacro set-in! [elem path & values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro set-in* [elem path values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro bind-in! [elem path & values]
  `(bind-with (fn [vs#] (set-in* ~elem ~path vs#)) (vector ~@values)))
