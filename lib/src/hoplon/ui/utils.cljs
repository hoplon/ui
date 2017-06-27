(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]
    [hoplon.core    :refer [with-dom]]
    [javelin.core   :refer [with-let]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prv [len i] (mod (dec i) len))
(defn nxt [len i] (mod (inc i) len))

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

;;; dom elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rect   [e] (when (instance? js/Element e) (.getBoundingClientRect e)))

(defn area  [e] (when-let [r (rect e)] [(.-left r) (.-top r) (.-width r) (.-height r)]))
(defn pos   [e] (subvec (area e) 0 2))
(defn size  [e] (subvec (area e) 2 4))
(defn x     [e] (if-let [r (rect e)] (.-x      r) (nth e 0)))
(defn y     [e] (if-let [r (rect e)] (.-y      r) (nth e 1)))
(defn w     [e] (if-let [r (rect e)] (.-width  r) (nth e 0)))
(defn h     [e] (if-let [r (rect e)] (.-height r) (nth e 1)))

(defn frame [e] (when-let [r (rect e)] [(.-left r) (.-right r) (.-top r) (.-bottom r)]))
(defn lt    [e] (when-let [r (rect e)] [(.-left  r) (.-top    r)]))
(defn rt    [e] (when-let [r (rect e)] [(.-right r) (.-top    r)]))
(defn lb    [e] (when-let [r (rect e)] [(.-left  r) (.-bottom r)]))
(defn rb    [e] (when-let [r (rect e)] [(.-right r) (.-bottom r)]))

;;; mouse events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn current [e] (.-currentTarget e))
(defn target  [e] (.-target        e))

; mouse position relative to the parent
(defn point-x [e] (- (.-clientX e) (x (current e))))
(defn point-y [e] (- (.-clientY e) (y (current e))))
(defn point   [e] [(point-x e) (point-y e)])

;;; stats ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clamp [n min max] (if (< n min) min (if (> n max) max n)))

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
