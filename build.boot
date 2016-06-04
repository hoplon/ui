(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :test-paths   #{"tst"}
  :target-path  "tgt"
  :dependencies '[[org.clojure/clojure       "1.7.0"     :scope "provided"]
                  [org.clojure/clojurescript "1.8.51"    :scope "provided"]
                  [org.clojure/tools.nrepl   "0.2.12"    :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-1" :scope "test"]
                  [adzerk/boot-cljs-repl     "0.3.0"     :scope "test"]
                  [adzerk/boot-reload        "0.4.7"     :scope "test"]
                  [adzerk/bootlaces          "0.1.13"    :scope "test"]
                  [com.cemerick/piggieback   "0.2.1"     :scope "test"]
                  [hoplon/boot-hoplon        "0.1.13"    :scope "test"]
                  [tailrecursion/boot-jetty  "0.1.3"     :scope "test"]
                  [weasel                    "0.7.0"     :scope "test"]
                  [hoplon                    "6.0.0-alpha15"]]
  :repositories  [["clojars"       "https://clojars.org/repo/"]
                  ["maven-central" "https://repo1.maven.org/maven2/"]])

(require
  '[adzerk.bootlaces         :refer :all]
  '[adzerk.boot-cljs         :refer [cljs]]
  '[adzerk.boot-cljs-repl    :refer [cljs-repl start-repl]]
  #_'[adzerk.boot-reload       :refer [reload]]
  '[hoplon.boot-hoplon       :refer [hoplon]]
  '[tailrecursion.boot-jetty :refer [serve]])

(def +version+ "0.0.1-SNAPSHOT")

(bootlaces! +version+)

(deftask build []
  (comp (speak) (hoplon :manifest true) (build-jar)))

(deftask develop []
  (comp (watch) (speak) (hoplon :manifest true) (pom) (jar) (install)))

(deftask test []
  (as-> (get-env) $
        (clojure.set/union (:source-paths $) (:test-paths $))
        (set-env! :source-paths $))
  (comp (watch) (speak) (hoplon) (cljs-repl) #_(reload) (cljs :optimizations :none) (serve))) ;; advanced :compiler-options {:elide-asserts true}

(task-options!
  pom    {:project     'hoplon/ui
          :version     +version+
          :description "a high level interface to hoplon."
          :url         "https://github.com/hoplon/ui"
          :scm         {:url "https://github.com/hoplon/ui"}
          :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
  serve {:port        5000})
