(ns hoplon.ui)

(defmacro set-in! [elem path value]
  `(set! ~(reduce #(list %2 %1) elem path) (hoplon.ui.attrs/->attr ~value)))

(defmacro bind-in! [elem path value]
  `(bind-with (fn [v#] (set-in! ~elem ~path v#)) ~value))

(defmacro cmpt [& args]
  `(hoplon.binding/binding [hoplon.ui/*pointer* (javelin.core/cell {:over 0 :down 0 :up 0 :out 0})
                            hoplon.ui/*state*   (javelin.core/cell nil)]
    (cmpt* ~@args)))

(defmacro item [& args]
  `(hoplon.binding/binding [hoplon.ui/*pointer* (javelin.core/cell {:over 0 :down 0 :up 0 :out 0})
                            hoplon.ui/*state*   (javelin.core/cell nil)]
    (item* ~@args)))

(defmacro form [& args]
  `(hoplon.binding/binding [*data*   (javelin.core/cell {})
                            *error*  (atom nil)
                            *submit* (atom nil)]
    (form* ~@args)))

(defmacro window [& args]
  `(hoplon.binding/binding [*position* (javelin.core/cell nil)]
     (window* ~@args)))
