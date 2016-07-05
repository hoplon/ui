(ns hoplon.ui
  (:require
    [javelin.core :refer [cell=]]))

(defmacro set-in! [elem path & values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro set-in* [elem path values]
  `(set! ~(reduce #(list %2 %1) elem path) (vstr ~values)))

(defmacro bind-in! [elem path & values]
  `(bind-with (fn [vs#] (set-in* ~elem ~path vs#)) (vector ~@values)))

(defmacro button [& args]
  `(binding [hoplon.ui.attrs/*state* (javelin.core/cell :up)]
    (button* ~@args)))

(defmacro toggle [& args]
  `(binding [hoplon.ui.attrs/*state* (javelin.core/cell :up)]
    (toggle* ~@args)))

(defmacro form [& args]
  `(binding [*data*   (atom nil)
             *error*  (atom nil)
             *submit* (atom nil)]
    (form* ~@args)))

(defmacro window [& args]
  `(binding [*scroll* (javelin.core/cell nil)]
    (window* ~@args)))
