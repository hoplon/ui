(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean [map]    (into {} (remove (comp empty? second) map)))
(defn name  [& args] (try (apply clojure.core/name args) (catch js/Error _)))
(defn dash  [str]    (join "-" (map lower-case (split str #"(?=[A-Z])"))))

;;; dom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bestow [node key val]
  (let [bind (fn [e] (.preventDefault e) (reset! (.-detail e) val))]
    (.addEventListener node key bind true)))

(defn inherit [node key callback]
  (let [*ref (atom nil)
        conf #js{:detail *ref :cancelable true}]
    (hoplon.core/with-dom node
      (if (.dispatchEvent node (js/CustomEvent. key conf))
          (throw (js/ReferenceError. (str "Unable to resolve inherited symbol " key ".")))
          (callback @*ref)))))
