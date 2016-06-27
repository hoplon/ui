(set-env!
  :source-paths #{"src"}
  :test-paths   #{"tst"}
  :target-path  "tgt"
  :dependencies '[[org.clojure/clojure       "1.8.0"     :scope "provided"]
                  [org.clojure/clojurescript "1.9.36"    :scope "provided"]
                  [org.clojure/tools.nrepl   "0.2.12"    :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-1" :scope "test"]
                  [adzerk/boot-cljs-repl     "0.3.0"     :scope "test"]
                  [adzerk/boot-reload        "0.4.10"    :scope "test"]
                  [adzerk/bootlaces          "0.1.13"    :scope "test"]
                  [com.cemerick/piggieback   "0.2.1"     :scope "test"]
                  [hoplon/boot-hoplon        "0.1.13"    :scope "test"]
                  [tailrecursion/boot-jetty  "0.1.3"     :scope "test"]
                  [weasel                    "0.7.0"     :scope "test"]
                  [hoplon/hoplon             "6.0.0-alpha16"]]
  :repositories  [["clojars"       "https://clojars.org/repo/"]
                  ["maven-central" "https://repo1.maven.org/maven2/"]])

(require
  '[adzerk.bootlaces         :refer :all]
  '[adzerk.boot-cljs         :refer [cljs]]
  '[adzerk.boot-cljs-repl    :refer [cljs-repl start-repl]]
  '[adzerk.boot-reload       :refer [reload]]
  '[hoplon.boot-hoplon       :refer [hoplon]]
  '[tailrecursion.boot-jetty :refer [serve]])

(def +version+ "0.0.1-SNAPSHOT")

(bootlaces! +version+)

(deftask develop []
  (comp (watch) (speak) (build-jar)))

(deftask deploy []
  (comp (speak) (build-jar) (push-snapshot)))

(deftask test []
  (as-> (get-env) $
        (clojure.set/union (:source-paths $) (:test-paths $))
        (set-env! :source-paths $))
  (comp (watch) (speak) (hoplon) (cljs-repl) (reload) (cljs :optimizations :none) (serve)))

(task-options!
  pom    {:project     'hoplon/ui
          :version     +version+
          :description "a cohesive layer of composable abstractions over the dom."
          :url         "https://github.com/hoplon/ui"
          :scm         {:url "https://github.com/hoplon/ui"}
          :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
  serve  {:port        5000})
