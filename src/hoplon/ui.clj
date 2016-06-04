(ns hoplon.ui
  (:require
    [javelin.core :refer [cell=]]))

(defmacro set-in! [elem path & values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro set-in* [elem path values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro bind-in! [elem path & values]
  `(bind-with (fn [vs#] (set-in* ~elem ~path vs#)) (vector ~@values)))

(defmacro form [& args]
  `(binding [*data*   (atom nil)
             *error*  (atom nil)
             *submit* (atom nil)]
    (form* ~@args)))

(defmacro window [& args]
  `(binding [*scroll* (javelin.core/cell nil)]
     (window* ~@args)))
