(set-env!
  :source-paths #{"src"}
  :test-paths   #{"tst"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "provided"]
                  [org.clojure/clojurescript "1.8.51"         :scope "provided"]
                  [adzerk/boot-cljs          "1.7.228-2"      :scope "test"]
                  [adzerk/boot-reload        "0.4.13"         :scope "test"]
                  [adzerk/bootlaces          "0.1.13"         :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [hoplon/hoplon             "6.0.0-alpha17"]
                  [cljsjs/markdown           "0.6.0-beta1-0"]]
  :repositories  [["clojars"       "https://clojars.org/repo/"]
                  ["maven-central" "https://repo1.maven.org/maven2/"]])

(require
  '[adzerk.bootlaces          :refer :all]
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-static :refer [serve]])

(ns-unmap 'boot.user 'test)

(def +version+ "0.1.0-SNAPSHOT")

(bootlaces! +version+)

(deftask develop []
  "Continuously rebuild and reinstall the library."
  (comp (watch) (speak) (build-jar)))

(deftask deploy []
  "Deploy the library snapshot to clojars"
  (comp (speak) (build-jar) (push-snapshot)))

(deftask test
  "Continuously rebuild the visual test suite during development.

  To simulate a production environment, the tests should be built with advanced
  optimizations and without validations"
  [e elide-asserts     bool "Exclude validations from build."
   o optimizations OPM kw   "Optimizations to pass the cljs compiler."]
  (let [o (or optimizations :none)]
    (as-> (get-env) $
        (clojure.set/union (:source-paths $) (:test-paths $))
        (set-env! :source-paths $))
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o :compiler-options {:elide-asserts elide-asserts}) (serve))))

(task-options!
  pom    {:project     'hoplon/ui
          :version     +version+
          :description "a cohesive layer of composable abstractions over the dom."
          :url         "https://github.com/hoplon/ui"
          :scm         {:url "https://github.com/hoplon/ui"}
          :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
  serve  {:port        5000})
