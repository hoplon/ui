(ns hoplon.ui
  (:require
    [javelin.core :refer [cell=]]))

(defmacro form [& args]
  `(binding [*data*   (atom nil)
             *error*  (atom nil)
             *submit* (atom nil)]
    (form* ~@args)))
