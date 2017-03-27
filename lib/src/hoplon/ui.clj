(ns hoplon.ui
  (:require
    [clojure.java.io :as io]))

(defmacro set-in! [elem path value]
  `(set! ~(reduce #(list %2 %1) elem path) (hoplon.ui.attrs/->attr ~value)))

(defmacro bind-in! [elem path value]
  `(bind-with! (fn [v#] (set-in! ~elem ~path v#)) ~value))

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
