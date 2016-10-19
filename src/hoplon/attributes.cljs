(ns hoplon.attributes
  (:require [hoplon.core :as h :refer [-do!]]
            [javelin.core :as j :refer [cell?]]))

(extend-type Keyword
  hoplon.core/ICustomAttribute
  (-do! [this elem value]
    (prn :this this :elem elem :value value)
    (cond (cell? value) (h/do-watch value #(h/do! elem this %2))
          (fn? value)   (h/on! elem this value)
          :else         (h/do! elem this value))))

(defn make-debug-attr [k]
  (reify hoplon.core/ICustomAttribute
    (-do! [this e v]
      (.log js/console (str "setting: " k " on " e))
      (cond (fn? v)   (.on (js/jQuery e) k v)
            (cell? v) (h/do-watch v #(-do! this e %2))
            :else     (.log js/console "setting:" k v)))))

(def --layout (make-debug-attr :layout))
