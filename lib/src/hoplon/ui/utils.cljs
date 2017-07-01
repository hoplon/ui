(ns hoplon.ui.utils
  (:refer-clojure
    :exclude [name])
  (:require
    [clojure.string :refer [join lower-case split]]
    [hoplon.core    :refer [when-dom add-initfn! with-dom]]
    [javelin.core   :refer [with-let]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prv [len i] (mod (dec i) len))
(defn nxt [len i] (mod (inc i) len))

(defn clean [map]    (into {} (remove (comp empty? second) map)))
(defn name  [& args] (try (apply clojure.core/name args) (catch js/Error _)))
(defn dash  [str]    (join "-" (map lower-case (split str #"(?=[A-Z])"))))

(defn xssoc
  "like assoc, but dissocs keys with nil values"
  [m & kvs]
  (into (empty m) (remove (comp nil? second) (apply assoc m kvs))))

(defn xssoc-in
  "like assoc-in, but dissocs last key if value is nil"
  [m path v]
  (if v (assoc-in m path v)
        (condp = (count path)
          0 m
          1 (dissoc m (first path))
          (update-in m (butlast path) dissoc (last path)))))

(defn debounce [ms f]
  (let [id (atom nil)]
    (fn [& args]
      (js/clearTimeout @id)
      (reset! id (js/setTimeout #(apply f args) ms)))))

(defn FileList->vec [fs]
  (vec (.call js/Array.prototype.slice fs)))

;;; fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn font-face [[id width weight slope] sources]
  (let [loc    #(str "local('" % "')")
        url    (fn [[f u]] (str "url('" u "') format('" (name f) "')"))
        src    (join "," (into (mapv loc (:system sources)) (mapv url (dissoc sources :system))))
        props  {"font-family"   id
                "font-stretch"  (name width)
                "font-weight"   (name weight)
                "font-style"    (name slope)
                "src"           src}]
    (str "@font-face{" (apply str (mapcat (fn [[k v]] (str k ":" v ";")) (clean props))) "}")))

;;; elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- measurements --------------------------------------------------------------;

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

;-- selection -----------------------------------------------------------------;

(defn index [element]
  (loop [i 0 e element]
    (if-let [e (.-previousSibling e)]
      (recur (inc i) e) i)))

(defn path
  "returns the relative path between two elems"
  [root node]
  (->> (iterate #(.-parentNode %) node)
       (take-while (partial not= root))
       (reduce #(if (.-elem %2) (cons %2 %) %) [])
       (map index)
       (rest)))

(defn get-elem [view path & [not-found]]
  (get-in view (vec (mapcat #(vector 2 %) path)) not-found))

(defn set-elem [view path elem]
  (if (seq path) (assoc-in view (vec (mapcat #(vector 2 %) path)) elem) elem))

(defn descend? [root & nodes]
  (every? #(.contains root %) nodes))

(defn when-ready
  "evaluate the function only after the dom is completely loaded"
  [element func]
  (when-dom element
    #(if (= js/document.readyState "complete") (func) (add-initfn! func))))

;-- inheritance ---------------------------------------------------------------;

(defn bestow*
  "when a bubbling inheritance event is detected on the dom element, wrap the
   closure attached to that event and reattach it so that the initiating inherit
   function may evaluate it to construct the appropriate bindings."
  [element func]
  (let [add-redefs! #(reset! (.-detail %) (func @(.-detail %)))]
    (.addEventListener element ::inherit add-redefs!)))

(defn inherit*
  "when the element is added to the dom, rebind all inherited vars to the values
   of their nearest ancestors then evaluate the function before resetting vars
   back to their original values."
  [element func]
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


;;; events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- elems ---------------------------------------------------------------------;

(defn current [e] (.-currentTarget e))
(defn target  [e] (.-target        e))

;-- mouse ---------------------------------------------------------------------;

(defn mouse-x [event & [ref-point]]
  (case ref-point
    :screen (.-screenX   event)
    :window (.-clientX   event)
    :page   (.-pageX     event)
    :target (.-offsetX   event)
    :mouse  (.-movementX event)
            (- (.-clientX event) (x (current event)))))

(defn mouse-y [event & [ref-point]]
  (case ref-point
    :screen (.-screenY   event)
    :window (.-clientY   event)
    :page   (.-pageY     event)
    :target (.-offsetY   event)
    :mouse  (.-movementY event)
            (- (.-clientY event) (y (current event)))))

(defn mouse
  "the position of the mouse, when given a js/MouseEvent and an optional
   reference point.

   ref-point. the upper-left corner from which the distance to the pointer will be
   calculated.  may be one of the following:
   - :screen  expressed in device (hardware) pixels
   - :window  does not change when scrolled.
   - :page    same as the window, but changes when scrolled.
   - :current the elem that caught the event (default)
   - :target  the elem that was clicked
   - :mouse   the last mouse moved event"
  [event & [ref-point]]
  (vector (mouse-x event ref-point) (mouse-y event ref-point)))

;;; stats ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clamp [n min max] (if (< n min) min (if (> n max) max n)))
