(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]
    [hoplon.core    :refer [with-dom]]
    [javelin.core   :refer [with-let]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clean [map]    (into {} (remove (comp empty? second) map)))
(defn name  [& args] (try (apply clojure.core/name args) (catch js/Error _)))
(defn dash  [str]    (join "-" (map lower-case (split str #"(?=[A-Z])"))))

(defn debounce [ms f]
  (let [id (atom nil)]
    (fn [& args]
      (js/clearTimeout @id)
      (reset! id (js/setTimeout #(apply f args) ms)))))

(defn FileList->vec [fs]
  (vec (.call js/Array.prototype.slice fs)))

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

(defn copy! [text]
  (let [foc (.-activeElement js/document)
        tgt (with-let [e (.createElement js/document "textarea")]
              (set! (.-value e) text))]
    (.appendChild (.-body js/document) tgt)
    (.focus tgt)
    (.setSelectionRange tgt 0 (.. tgt -value -length))
    (.execCommand js/document "copy")
    (.focus foc)
    (.removeChild (.-body js/document) tgt)))
