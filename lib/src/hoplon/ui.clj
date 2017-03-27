(ns hoplon.ui
  (:require
    [clojure.java.io :as io]))

(defmacro set-in! [elem path value]
  `(set! ~(reduce #(list %2 %1) elem path) (hoplon.ui.attrs/->attr ~value)))

(defmacro bind-in! [elem path value]
  `(bind-with! (fn [v#] (set-in! ~elem ~path v#)) ~value))

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

#_(defn slurp-bytes [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream (clojure.java.io/resource x)) out)
    (.toByteArray out)))

#_(defmacro data-uri [path mime-type & [binary]]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (let [encode (.encodeToString (java.util.Base64/getUrlEncoder) (.toByteArray stream))
          stream (-> (io/resource path) (io/input-stream) (io/copy out))
          data  (if binary (str "base64," ))])
    (.toByteArray out)
    (str "data:" mime-type "; " (when binary "base64," base64,) )))
