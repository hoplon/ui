(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]
    [hoplon.core    :refer [with-dom]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean [map]    (into {} (remove (comp empty? second) map)))
(defn name  [& args] (try (apply clojure.core/name args) (catch js/Error _)))
(defn dash  [str]    (join "-" (map lower-case (split str #"(?=[A-Z])"))))

;;; dom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bestow* [element redefn]
  "when a bubbling inheritance event is detected on the dom element, wrap the
   closure attached to that event and reattach it so that the initiating inherit
   function may evaluate it to construct the appropriate bindings."
  (let [add-redefs! #(reset! (.-detail %) (redefn @(.-detail %)))]
    (.addEventListener element ::inherit add-redefs!)))

(defn inherit* [element func]
  "when the element is added to the dom, rebind all inherited vars to the values
   of their nearest ancestors then evaluate the function before resetting vars
   back to their original values."
  (let [*fn (atom func)
        cnf #js{:bubbles true :detail *fn}]
    (with-dom element
      (.dispatchEvent element (js/CustomEvent. ::inherit cnf))
      (@*fn))))
