(ns hoplon.ui
  (:refer-clojure
    :exclude [comp])
  (:require
    [javelin.core :refer [cell=]]))

(defmacro set-in! [elem path & values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro set-in* [elem path values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro bind-in! [elem path & values]
  `(bind-with (fn [vs#] (set-in* ~elem ~path vs#)) (vector ~@values)))

(defmacro comp [& args]
  `(binding [hoplon.ui/*pointer* (javelin.core/cell {:over 0 :down 0 :up 0 :out 0})
             hoplon.ui/*state*   (javelin.core/cell nil)]
    (comp* ~@args)))

(defmacro form [& args]
  `(binding [*data*   (atom nil)
             *error*  (atom nil)
             *submit* (atom nil)]
    (form* ~@args)))

(defmacro window [& args]
  `(binding [*position* (javelin.core/cell nil)]
    (window* ~@args)))
