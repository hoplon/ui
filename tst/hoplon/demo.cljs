(ns+ hoplon.demo
  (:page
    "index.html")
  (:refer-clojure
    :exclude [-])
  (:require
    [javelin.core    :refer [defc defc= cell= cell]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [window elem frame image object video form line lines pick picks file files item s b]]
    [hoplon.ui.elems :refer [markdown]]
    [hoplon.ui.attrs :refer [- c r d]]))

;;; model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc db {:session {:state :home}})

;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= state (-> db :session :state))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn swap-state! [state]
  (swap! db assoc-in [:session :state] state))

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- sizes ---- ----------------------------------------------------------------;

(def gutter 6)

;-- colors --------------------------------------------------------------------;

(def black  (c 0x1F1F1F))
(def orange (c 0xE73624))
(def blue   (c 0x009BFF))
(def yellow (c 0xF5841F))

;-- fonts ---------------------------------------------------------------------;

(def menu-font {:f 28 :ff "opensans" :ft :800 :fc :white :fw 1})

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(window
  :title "Hoplon UI"
  (elem :sh (r 1 1) :ah :mid :c orange :bt 8 :bc yellow
    (elem :sh (>sm md) :p 50 :gv 50 :a :mid
      (image :s 400 :m :pointer :click #(swap-state! :home) :url "http://hoplon.io/images/logos/hoplon-logo.png")
      (elem :sh (>sm (- (r 1 1) (+ 400 10))) :p 10 :g 10 :ah :end
        (elem :sh (>sm :auto) :ah :mid :m :pointer menu-font "layout")
        (elem :sh (>sm :auto) :ah :mid :m :pointer menu-font "fonts")
        (elem :sh (>sm :auto) :ah :mid :m :pointer menu-font "layout")
        (elem :sh (>sm :auto) :ah :mid :m :pointer menu-font "source"))))
  (elem :sh (r 1 1) :ah :mid
    (elem :sh (>sm md) :p 50 :g 50 :a :mid
      (image :a :mid :b 2 :bc :black :url "http://placehold.it/200x100"
        (elem "content"))
      (image :s 200 :a :mid :b 2 :bc :black :fit :fill :url "http://placehold.it/200x100"
        (elem "filled"))
      (image :s 200 :p 20 :b 2 :bc :black :fit :cover :url "http://placehold.it/200x100"
        (elem "covered"))
      (image :s 200 :a :mid :b 2 :bc :black :fit :contain :url "http://placehold.it/200x100"
        (elem "contained"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :fill :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "filled"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :cover :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "covered"))
      (video :s 200 :a :mid :b 2 :bc :black :fit :contain :autoplay :true :url "http://www.sample-videos.com/video/mp4/720/big_buck_bunny_720p_1mb.mp4"
        (elem "contained")))))
