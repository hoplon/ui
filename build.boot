(set-env!
  :source-paths #{"lib/src"}
  :dependencies '[[org.clojure/clojure                      "1.8.0"          :scope "provided"]
                  [org.clojure/clojurescript                "1.9.293"        :scope "provided"]
                  [adzerk/env                               "0.4.0"          :scope "test"]
                  [adzerk/boot-cljs                         "1.7.228-2"      :scope "test"]
                  [adzerk/boot-test                         "1.1.2"          :scope "test"]
                  [adzerk/boot-reload                       "0.4.13"         :scope "test"]
                  [adzerk/bootlaces                         "0.1.13"         :scope "test"]
                  [org.seleniumhq.selenium/selenium-support "3.0.1"          :scope "test"]
                  [tailrecursion/boot-static                "0.1.0"          :scope "test"]
                  [hoplon/hoplon                            "6.0.0-alpha17"]
                  [hoplon/javelin                           "3.9.0"]
                  [cljsjs/markdown                          "0.6.0-beta1-0"]])

(require
  '[adzerk.bootlaces          :refer :all]
  '[adzerk.boot-test          :as    t]
  '[adzerk.boot-env           :refer [init]]
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-static :refer [serve]])

(ns-unmap 'boot.user 'test)

(def +version+ "0.1.0-SNAPSHOT")

#_(bootlaces! +version+) ;; https://github.com/adzerk-oss/bootlaces/issues/9

(deftask develop []
  "Continuously rebuild and reinstall the library."
  (set-env! :source-paths #{"lib/src"})
  (comp (watch) (speak) (build-jar)))

(deftask deploy []
  "Deploy the library snapshot to clojars"
  (set-env! :source-paths #{"lib/src"})
  (comp (speak) (build-jar) (push-snapshot)))

(deftask demo
  [o optimizations OPM   kw   "Optimizations to pass the cljs compiler."
   v no-validate         bool "Elide assertions used to validate attibutes."]
  "Serve the test app locally"
  (set-env! :source-paths #{"lib/src" "app/src"} :resource-paths #{"app/rsc"})
  (let [o (or optimizations :none)
        c {:elide-asserts no-validate}]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o :compiler-options c) (serve))))

(deftask connect
  "Launch Sauce Connect Proxy"
  [u username USER str "Username"
   k access-key PASS str "Access Key"]
  (with-pass-thru _
    (let [u (or username   (System/getProperty "SAUCE_LABS_USERNAME"))
          k (or access-key (System/getProperty "SAUCE_LABS_ACCESS_KEY"))]
      (boot.util/info "Starting Sauce Connect proxy...\n")
      (prn sh (sh "sc" "-u" u "-k" k)))))

(deftask test-local
  "Continuously rebuild the test suite during development.

  To simulate a production environment, the tests should be built with advanced
  optimizations and without validations"
  [n namespaces NS       #{sym}   "Namespaces containing unit tests."
   o optimizations OPM   kw       "Optimizations to pass the cljs compiler."
   v no-validate         bool     "Elide assertions used to validate attibutes."]
  (let [o (or optimizations :none)
        c {:elide-asserts no-validate}]
    (set-env! :source-paths #{"lib/src" "app/src"} :resource-paths #{"tst/src" "app/rsc"})
    (comp (init) (connect) (watch) (speak) (hoplon) (cljs :optimizations o :compiler-options c) (serve) (t/test :namespaces namespaces))))

(deftask test
  "Continuously rebuild the test suite during development.

  To simulate a production environment, the tests should be built with advanced
  optimizations and without validations"
  [n namespaces NS       #{sym}   "Namespaces containing unit tests."
   o optimizations OPM   kw       "Optimizations to pass the cljs compiler."
   v no-validate         bool     "Elide assertions used to validate attibutes."]
  (let [o (or optimizations :none)
        c {:elide-asserts no-validate}]
    (set-env! :source-paths #{"lib/src" "app/src"} :resource-paths #{"tst/src" "app/rsc"})
    (comp (hoplon) (cljs :optimizations o :compiler-options c) (serve) (t/test :namespaces namespaces))))

(task-options!
  init   {:file            "cnf/local.env"}
  pom    {:project         'hoplon/ui
          :version         +version+
          :description     "a cohesive layer of composable abstractions over the dom."
          :url             "https://github.com/hoplon/ui"
          :scm             {:url "https://github.com/hoplon/ui"}
          :license         {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
  serve  {:port            5000}
  test   {:namespaces     '#{hoplon-test.ui}})

