(ns hoplon.ui.utils)

(defmacro bestow
  "rebind the vars associated with any descendant elements through the inherit
   inherit macro when they are inserted into the dom below, for example:

    (let [menu (hoplon.core/div ...)
      (bestow [+selections+ (atom #{)] menu
        (doseq [selection +selections+]
          (println selection))))

    (let [item (hoplon.core/div j..)
      (inherit item
        (item :click #(swap! +selections+ conj @%))))"
  [bindings element & body]
  (let [vars (take-nth 2 bindings)
        syms (repeatedly (count bindings) gensym)
        vals (take-nth 2 (rest bindings))
        lets (vec (interleave syms vals))
        defs (vec (interleave vars syms))]
   `(let ~lets
      (with-redefs ~defs
        (bestow* ~element
          (fn [redef#]
            (fn []
              (with-redefs ~defs
               (redef#)))))
        ~@body))))

(defmacro inherit
  "create a context where any inherited vars will be rebound to the values set
   by the bestow macro through the nearest ancestor element. the semantic is
   similar to that of a dynamic var, but differs in that the stack is replaced
   by the path to the root of the dom tree and that the binding occurs when the
   associated element is inserted into the dom."
  [element & body]
  `(inherit* ~element (fn [] ~@body)))

(defmacro with-ready
  "evaluate the body only after the element has been inserted into the dom and
   the dom is fully loaded. similar to with-dom, but evaluates only after the
   layout is complete.  useful in scenarios where an element needs to obtain
   layout information from the dom after it is added. see the popout elem."
  [element & body]
  `(when-ready ~element (fn [] ~@body)))
