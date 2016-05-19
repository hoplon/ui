(ns hoplon.ui
  (:refer-clojure :exclude [range time])
  (:require
    [hoplon.core     :as h]
    [hoplon.ui.elems :as e]
    [clojure.string  :refer [split]]
    [javelin.core    :refer [cell]]
    [hoplon.ui.value :refer [model ev rt px kw hx]])
  (:require-macros
    [hoplon.core  :refer [defelem for-tpl]]
    [javelin.core :refer [cell=]]))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn route->hash [[path & [qmap]]]
  "transforms a urlstate of the form [[\"foo\" \"bar\"] {:baz \"barf\"}]
   to hash string in the form \"foo/bar&baz=barf\""
  (let [pair (fn [[k v]] (str (name k) "=" (pr-str v)))
        pstr (when path (apply str "/" (interpose "/" (map name path))))
        qstr (when qmap (apply str "?" (interpose "&" (map pair qmap))))]
    (str "#" pstr qstr)))

(defn hash->route [hash]
  "transforms a hash string to a urlstate of the form
   [[\"foo\" \"bar\"] {:baz \"barf\"}]"
  (let [[rstr qstr] (split (subs hash 2) #"\?")
        pair        #(let [[k v] (split % #"=")] [(keyword k) (cljs.reader/read-string v)])
        qmap        (->> (split qstr #"&") (map pair) (when (not-empty qstr)) (into {}))
        path        (->> (split rstr #"/") (remove empty?) (mapv keyword))]
    (vec (remove empty? [path qmap]))))

(def visibility->status
  "maps the visibility string to a status keyword"
  {"visible"   :foreground
   "hidden"    :background
   "prerender" :background
   "unloaded"  :terminated})

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-icon-url  "data:;base64,iVBORw0KGgo=")
(def empty-image-url "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==")

(def ^:dynamic *data*   nil)
(def ^:dynamic *error*  nil)
(def ^:dynamic *submit* nil)

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def +sm+ 736)
(def +lg+ 1069)

(def +default-button+      {:color 0xffffff :s-color 0xcccccc :font-color 0x333})
(def +default-button-down+ {:color 0xd4d4d4 :s-color 0x8c8c8c :font-color 0x333})
(def +primary-button+      {:color 0x337ab7 :s-color 0x2e6da4 :font-color 0xfff})
(def +primary-button-down+ {:color 0x286090 :s-color 0x204d74 :font-color 0xfff})
(def +success-button+      {:color 0x5cb85c :s-color 0x4cae4c :font-color 0xfff})
(def +success-button-down+ {:color 0x449d44 :s-color 0x398439 :font-color 0xfff})
(def +warning-button+      {:color 0xf0ad4e :s-color 0xeea236 :font-color 0xfff})
(def +warning-button-down+ {:color 0xd58512 :s-color 0x985f0d :font-color 0xfff})
(def +info-button+         {:color 0x5bc0de :s-color 0x46b8da :font-color 0xfff})
(def +info-button-down+    {:color 0x31b0d5 :s-color 0x269abc :font-color 0xfff})
(def +danger-button+       {:color 0xd9534f :s-color 0xd43f3a :font-color 0xfff})
(def +danger-button-down+  {:color 0xac2925 :s-color 0x761c19 :font-color 0xfff})

(def +success+ {:color 0xdff0d8 :s-color 0xd6e9c6 :font-color 0x3c763d})
(def +warning+ {:color 0xfcf8e3 :s-color 0xfaebcc :font-color 0x8a6d3b})
(def +danger+  {:color 0xf2dede :s-color 0xebccd1 :font-color 0xa94442})
(def +info+    {:color 0xd9edf7 :s-color 0xBCE8F1 :font-color 0x31708f})

(def palettes
  {:hoplon.ui/success +success+
   :hoplon.ui/warning +warning+
   :hoplon.ui/danger  +danger+
   :hoplon.ui/info    +info+})

(def +f+  21)
(def p 18)

(def +sc+ 0xCCC)

(def bgcolor :grey)

;;; elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem elem [attrs elems]
  (e/elem attrs elems))

(defelem button [attrs elems]
  (e/button :ph 14 :pv 10 :r 5 :c (hx 0xFFF) :s 1 :sc (hx 0xCCC) :ah :center :f p :fc (hx 0x333)
    attrs elems))

(defelem image [attrs elems]
  (e/image attrs elems))

;; todo: window rel=noopener
;; todo: finish mousechanged

(defelem window [{:keys [alert metadata route scroll scripts styles initiated mousechanged scrollchanged statuschanged routechanged] :as attrs} elems]
  (let [get-agent  #(-> js/window .-navigator)
        get-hash   #(-> js/window .-location .-hash)
        get-route  #(-> js/window .-location .-hash hash->route)
        get-status #(-> js/window .-document .-visibilityState visibility->status)]
    (when initiated
      (initiated (get-route) (get-status) (get-agent)))
    (when routechanged
      (.addEventListener js/window "hashchange"
        #(when-not (= (route->hash @route) (get-hash)) (routechanged (get-route)))))
    (when statuschanged
      (.addEventListener js/window "visibilitychange"
        #(statuschanged (get-status))))
    (when scrollchanged
       (.addEventListener js/window "scroll"
         #(let [[x y :as new-scroll] (vector (.-scrollX js/window) (.-scrollY js/window))]
            (when-not (= new-scroll scroll)
              (scrollchanged x y)))))
    (cell= (set! js/location.hash (route->hash route)))
    (.addEventListener js/document "DOMContentLoaded"
      #(cell= (.scroll js/window (first scroll) (second scroll)))))
  (h/html :lang (:lang attrs "en") :css/height "100%"
    (h/head
      (h/html-meta :charset "utf-8")
      (h/html-meta :http-equiv "X-UA-Compatible" :content "IE=edge")
      (h/html-meta :name "viewport"    :content "width=device-width, initial-scale=1")
      (for [m (if (map? metadata) (map (fn [[k v]] {:name k :content v}) metadata) metadata)]
        (h/html-meta (into {} (for [[k v] m] [k (name v)]))))
      (h/title (:title attrs))
      (h/link :rel "icon" :href (or (:icon attrs) empty-icon-url))
      (for-tpl [s styles]  (h/link :rel "stylesheet" :href s))
      (for-tpl [s scripts] (h/script :src s)))
    (h/body :h (rt 1 1) :w (rt 1 1) :css/margin 0 :css/font-family "Helvetica Neue, Helvetica, Arial, sans-serif" ;; font-size 100%, neutralize body dflts
      (dissoc attrs :alert :scroll :metadata :title :icon :route :lang :styles :scripts :initiated :mousechanged :scrollchanged :statuschanged :routechanged)
      (map model elems)))) ;; todo: implement body as elem

;;; element components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem alert [{:keys [alert] :as attrs} _]
  (let [detail  (cell false)
        palette #(palettes (keyword "hoplon.ui" (name (or (:severity %) "info"))))]
    (elem
      :ph         15
      :pv         10
      :r          4
      :s          2
      :toggle     alert
      :color      (cell= (:color      (palette alert)))
      :s-color    (cell= (:s-color    (palette alert)))
      :font-color (cell= (:font-color (palette alert)))
      (dissoc attrs :alert)
      (cell= (:message alert)))))
