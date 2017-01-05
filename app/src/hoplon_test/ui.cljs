(ns ^{:hoplon/page "index.html"} hoplon-test.ui
  (:refer-clojure
    :exclude [- test])
  (:require
    [javelin.core    :refer [defc defc= cell= cell]]
    [hoplon.core     :refer [defelem for-tpl when-tpl case-tpl]]
    [hoplon.ui       :refer [window elem image form line lines pick picks file files item write s b]]
    #_[hoplon.ui.elems :refer [markdown]]
    [hoplon.ui.attrs :refer [- rgb hsl lgr r d]]))

(defc things ["a" "b" "c"])

(def metadata
  [{:property "og:url"                   :content "http://www.mysite.com/"}
   {:name     "image_src"                :content "http://www.mysite.com/images/logo-fb.png"}
   {:property "og:image"                 :content "http://www.mysite.com/images/logo-180.png"}
   {:property "og:image:width"           :content "180"}
   {:property "og:image:height"          :content "110"}
   {:property "og:image"                 :content "http://www.mysite.com/images/logo-250.png"}
   {:property "og:image:width"           :content "250"}
   {:property "og:image:height"          :content "250"}
   {:content  "text/html; charset=UTF-8" :http-equiv "content-type"}])

;;; styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-- breakpoints ---------------------------------------------------------------;

(def sm 760)
(def md 1240)
(def lg 1480)

(defn >sm [& bks] (apply b (r 1 1) sm bks))

;-- dimensions ----------------------------------------------------------------;

(def gutter 6)

;-- colors --------------------------------------------------------------------;

(def border-grey (rgb 0xCCCCCC))
(def fill-grey   (rgb 0xEEEEEE))
(def font-grey   (rgb 0x888888))

(def fail-color  (rgb 0xd43f3a))
(def pass-color  (rgb 0x4cae4c))

;-- fonts ---------------------------------------------------------------------;

(def title-styles {:f 24})

;-- forms ---------------------------------------------------------------------;

(def text-styles {:ph 14 :pv 10 :r 5 :b 1 :bc border-grey :f 21})

;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defelem suite [{:keys [title pass code] :as attrs} elems]
  (elem :s (r 1 1) :p gutter :g gutter
    (elem :sh (r 1 1) :ph gutter
      (elem :sh (r 1 2) :f 21 :fc font-grey
        title)
      (elem :sh (r 1 2) :ah :end
        (when-tpl code
          (elem :p gutter :c fill-grey :b 1 :bc border-grey :r 3
            code))))
    (dissoc attrs :code :pass :title) elems))

(defelem box [attrs elems]
  (elem :s 40 :a :mid :c (rgb 0xFFFFFF) :b 2 :bc border-grey
    attrs elems))

(defelem test [{:keys [title pass] :as attrs} elems]
  (elem :sh 200
    (elem :sv 26 :ph gutter :gh gutter :av :mid
      (elem :s 6 :r 6 :c (cell= (if pass pass-color fail-color)))
      (elem :sh 170 :f 10 :fc font-grey :v :hidden title))
    (elem :s 200                                          ; sizing
            :p gutter :g gutter                             ; spacing
            :c fill-grey :b 2 :bc border-grey               ; coloring
            :d (d 3 3 (rgb 0xAAAAAA) 4)                       ; shaDow
            (dissoc attrs :pass :title)
      elems)))

(defelem text-test [{:keys [title pass] :as attrs} elems]
  (elem :sh 400
    (elem :sv 26 :ph gutter :gh gutter :av :mid
      (elem :s 6 :r 6 :c (cell= (if pass pass-color fail-color)))
      (elem :sh 170 :f 10 :fc font-grey :v :hidden title))
    (elem :s (r 1 1)                                          ; sizing
            :p gutter :g gutter                             ; spacing
            :c fill-grey :b 2 :bc border-grey               ; coloring
            :d (d 3 3 (rgb 0xAAAAAA) 4)                       ; shaDow
            (dissoc attrs :pass :title)
      elems)))

#_(hoplon.core/html
  (hoplon.core/head
    (hoplon.core/title "hello"))
  (hoplon.core/body :css/margin 0
    (elem :sh (r 1 1) :sv 200 :p 20 :b 1 :bc :grey
      (elem :sh (r 1 3) :sv (r 1 1) :a (r 1 2) :b 1 :bc :grey
        (elem :s 40 :a (r 1 2) :b 1 :bc :grey
          "A"))
      (elem :sh (r 1 3) :sv (r 1 1) :a (r 1 2) :b 1 :bc :grey
        "B")
      (elem :sh (r 1 3) :sv (r 1 1) :a (r 1 2) :b 1 :bc :grey
        "C"))
    (elem :sh (r 1 1) :sv 200 :ah (r 2 2) :av (r 0 1) :b 1 :bc :grey
      "hello dude")))

(window
  :title     "Hoplon UI"
  :route     [["tests"] {:foo "bar" :baz "barf"}]
  :metadata  metadata
  :scroll    true
  (elem :sh (r 1 1) :sv 50 :p 6 :a :mid :b 2 :bc border-grey
    #_(image :url "hoplon-logo.png")
    (elem :sh+ (r 1 1) :pl 6 :f 21 "Hoplon UI Live Reference & Functional Tests"))
  (suite :title "alignments" :code ":a :av :ah [:beg :bid :end :jst]" :pass false
    (test :ah :beg :av :beg :title "box aligns horizontal left & vertical top" :pass true
      (box "a"))
    (test :ah :mid :av :beg :title "box aligns horizontal center & vertical top" :pass true
      (box "a"))
    (test :ah :end :av :beg :title "box aligns horizontal right & vertical top" :pass true
      (box "a"))
    (test :ah :beg :av :mid :title "box aligns horizontal left & vertical middle" :pass true
      (box "a"))
    (test :ah :mid :av :mid :title "box aligns horizontal center & vertical middle" :pass true
      (box "a"))
    (test :ah :end :av :mid :title "box aligns horizontal right & vertical middle" :pass true
      (box "a"))
    (test :ah :beg :av :end :title "box aligns horizontal left & vertical bottom" :pass true
      (box "a"))
    (test :ah :mid :av :end :title "box aligns horizontal center & vertical bottom" :pass true
      (box "a"))
    (test :ah :end :av :end :title "box aligns horizontal right & vertical bottom" :pass true
      (box "a"))
    (test :ah :beg :av :beg :title "boxes align horizontal left & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :mid :av :beg :title "boxes align horizontal center & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :end :av :beg :title "boxes align horizontal right & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :beg :av :mid :title "boxes align horizontal left & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :mid :av :mid :title "boxes align horizontal center & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :end :av :mid :title "boxes align horizontal right & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :beg :av :end :title "boxes align horizontal left & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :mid :av :end :title "boxes align horizontal center & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :end :av :end :title "boxes align horizontal right & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c"))
    (test :ah :beg :av :beg :title "boxes align horizontal left & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :mid :av :beg :title "boxes align horizontal center & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :end :av :beg :title "boxes align horizontal right & vertical top" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :beg :av :mid :title "boxes align horizontal left & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :mid :av :mid :title "boxes align horizontal center & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :end :av :mid :title "boxes align horizontal right & vertical middle" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :beg :av :end :title "boxes align horizontal left & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :mid :av :end :title "boxes align horizontal center & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :ah :end :av :end :title "boxes align horizontal right & vertical bottom" :pass true
      (box :sv 60 "a")
      (box :sv 40 "b")
      (box :sv 20 "c")
      (box :sv 40 "d")
      (box :sv 60 "e")
      (box :sv 60 "f")
      (box :sv 40 "g")
      (box :sv 20 "h")
      (box :sv 40 "i")
      (box :sv 40 "j"))
    (test :a :mid :title "box aligns horizontal center and vertical top inside image" :pass true
      (image :s 100 :ah :mid :av :beg :url "http://hoplon.github.io/assets/images/logos/hoplon-logo.png"
        (box :s 40 "a")))
    (test :a :mid :title "box aligns horizontal center and vertical middle inside image" :pass true
      (image :s 100 :a :mid :url "http://hoplon.github.io/assets/images/logos/hoplon-logo.png"
        (box :s 40 "a")))
    (test :a :mid :title "box aligns horizontal center and vertical bottom inside image" :pass true
      (image :s 100 :ah :mid :av :end :url "http://hoplon.github.io/assets/images/logos/hoplon-logo.png"
        (box :s 40 "a")))
    #_(text-test
      (markdown
        "#header one\n##header two\n###header three\n####header four\n* bullet one\n* bullet two\nsome *italic text* and **bold text**\n")))
  #_(elem :sh (r 1 1) :ah :mid
    (form :sh (b (r 1 1) md 1200) :g 10 :submit #(prn "submitting data: " %)
      (elem title-styles :sh (r 1 1) "forms")
      (elem :sh (r 1 1) "form elem ctors are based on the type of input solicited from the user.  they may prompt the user to enter text, choose from a list of items, upload a file,  ")
      (line text-styles :sh (>sm 3 8) :key :first-name  "first name")
      (line text-styles :sh (>sm 1 8) :key :middle-name "mi")
      (line text-styles :sh (>sm 1 2) :key :last-name   "last name")
      (line text-styles :sh (>sm 1 1) :key :address     "street address")
      (line text-styles :sh (>sm 1 2) :key :city        "city")
      (line text-styles :sh (>sm 1 4) :key :state       "state")
      (line text-styles :sh (>sm 1 4) :key :zip         "zip")
      (line text-styles :sh (>sm 1 3) :key :photo       "profile photo")
      (pick text-styles :sh (>sm 1 3) :key :gender :selection :male
        (item :sh (r 1 1) :c (s :on :grey :off :white) :m :pointer :val :male   "male")
        (item :sh (r 1 1) :c (s :on :grey :off :white) :m :pointer :val :female "female"))
      (picks text-styles :sh (>sm 1 3) :key :meals :selections #{:hamburger}
        (item :sh (r 1 1) :c (s :on :blue :off :white) :m :pointer :val :fries        "fries")
        (item :sh (r 1 1) :c (s :on :blue :off :white) :m :pointer :val :hamburger    "hamburger")
        (item :sh (r 1 1) :c (s :on :blue :off :white) :m :pointer :val :cheezeburger "cheezeburger"))
      (picks text-styles :sh (>sm 1 3) :key :meals :selections #{:hamburger}
        (for-tpl [[val label] [[:fries "fries"] [:hamburger "hamburger"] [:cheezeburger "cheeseburger"]]]
          (item :val val label)))
      (lines text-styles :sh (r 1 1) :key :description "description")
      (file text-styles :sh (>sm 1 3) :key :photo       "profile photo")
      (write text-styles :sh (r 1 1) :ah :mid :m :pointer :label "submit transaction")))
  (test :title "background color is rgb" :c (rgb 50 50 50 (r 1 2))
    (str (rgb 50 50 50 (r 1 2))))
  (test :title "background color is hsl" :c (hsl 50 (r 7 10) (r 1 2))
    (str (hsl 50 (r 7 10) (r 1 2) (r 3 4))))
  (test :title "background color is linear gradient" :c (lgr 45 (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2)))
    (str (lgr 45 (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2)))))
  #_(test :title "background color is radial gradient" :c (cgr (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2)))
    (str (cgr (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2)))))
  #_(test :title "background color is radial gradient" :c (egr (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2)))
    (str (egr (hsl 50 (r 7 10) (r 1 2)) (hsl 100 (r 7 10) (r 1 2))))))

  ; (test :ah :mid  :av :mid :title "box in cell aligns horizontal center & vertical center" :pass false
  ;   (rgbell (box "a")))
  ; (test :ah :mid  :av :mid :title "boxes in formula cell align horizontal center & vertical center" :pass false
  ;   (for-tpl [thing things]
  ;     (box thing)))
  ; (suite :title "layouts"
  ;   (test :a :mid :title "elem % sizes to column width set by sibling" :pass false
  ;     (elem
  ;       (box :sh(r 1 1) "a")
  ;       (box "b")))
  ;   (test :a :mid :title "fonts below size 16 adjust vertical position of siblings" :pass false
  ;       ;; caused by
  ;     (elem
  ;       (elem :f 10 "a")
  ;       (box "b")))
  ;   (test :a :mid :title "elem of fixed width remains constant when gutter is applied to parent." :pass false
  ;       ;; solution: apply lengths to middle, but percentages to outer
  ;     (elem :g 50
  ;       (box "a")))))
  ; (test :a :mid :title "elem sizes to width of attribute in cell" :pass false
  ;   (elem
  ;     (box :sh(rgbell 80) "a")))
  ; (suite :title "controls"
  ;   (button :sh300 :sv 150 "Click Me" :click #(swap! things conj "d"))
  ;   (button :sh150 :sv 75 :fc :white "Click Me")
  ;   (button :sh300 :sv 200 "Click Me")
  ;   (button :sh300 :sv 200 "Click Me")
  ;   #_(elem :sv 25 :sh[(r 1 1) sm (- (r 1 1) 220)] :ah :end :g 6)))
