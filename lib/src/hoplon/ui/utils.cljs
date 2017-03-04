(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean [map]    (into {} (filter second map)))
(defn name  [& args] (try (apply clojure.core/name args) (catch js/Error _)))
(defn dash  [str]    (join "-" (map lower-case (split str #"(?=[A-Z])"))))
