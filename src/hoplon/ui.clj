(ns hoplon.ui
  (:require
    [javelin.core :refer [cell=]]))

; (defn change-type [v]
;   (cond
;     (int?   v) `(px ~v)
;     (nil?   v) `(px ~v)
;     (list?  v) (if (some #{'+ '- '/ '*} (first v))
;                  `(ev ~@v)
;                  v) ;; and the list starts with -+/* with ratio inside
;     (ratio? v) `(pc ~(int (* v 100)))
;     :else       v))
;
; (defn- type-body
;   [old-args]
;   (loop [[old-arg & old-args] new-args (transient [])]
;     (if-not old-arg
;       [(persistent! new-args)]
;       (cond (map? arg)     (recur old-args (conj! new-args old-arg (reduce-kv #(assoc! %1 %2 (value-type %3))) {} arg))
;             (keyword? arg) (recur old-args (conj! new-args old-arg (value-type (first new-args))))
;             :else          (recur old-args (conj! new-args old-arg))))))
;
; (defn- type-body [forms]
;  (map #(if (list? %) (change -type forms))))
;
; (defmacro defelem
;   "Define a custom element."
;   [name & forms]
;   `(hoplon.core/defelem ~name ~@forms)
;   (let [[_ name [_ & [[bind & body]]]] (macroexpand-1 `(defn ~name ~@forms))]
;     `(def ~name (elem ~bind ~@(type-body body)))))

(defmacro form [& args]
  `(binding [*data*   (atom nil)
             *error*  (atom nil)
             *submit* (atom nil)]
    (form* ~@args)))
