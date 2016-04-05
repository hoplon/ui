(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"tst" "src"}
  :target-path  "tgt"
  :dependencies '[[javax.servlet/javax.servlet-api "3.1.0"     :scope "provided"]
                  [org.clojure/clojurescript       "1.7.228"   :scope "provided"]
                  [adzerk/boot-cljs                "1.7.228-1" :scope "test"]
                  [adzerk/boot-reload              "0.4.7"     :scope "test"]
                  [adzerk/bootlaces                "0.1.13"    :scope "test"]
                  [hoplon/boot-hoplon              "0.1.13"    :scope "test"]
                  [tailrecursion/boot-jetty        "0.1.3"     :scope "test"]
                  [hoplon                          "6.0.0-alpha13"]
                  [cljsjs/enquire                  "2.1.2-0"]]
  :repositories  [["clojars"       "https://clojars.org/repo/"]
                  ["maven-central" "https://repo1.maven.org/maven2/"]])

(require
  '[adzerk.bootlaces         :refer :all]
  '[adzerk.boot-cljs         :refer [cljs]]
  '[adzerk.boot-reload       :refer [reload]]
  '[hoplon.boot-hoplon       :refer [hoplon]]
  '[tailrecursion.boot-jetty :refer [serve]])

(def +version+ "0.0.1-SNAPSHOT")

(bootlaces! +version+)

(deftask build []
  (comp (speak) (hoplon :manifest true) (build-jar)))

(deftask develop []
  (comp (watch) (speak) (hoplon :manifest true) (pom) (jar) (install)))

(deftask run []
  (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations :none) (serve)))

(task-options!
  pom    {:project     'hoplon/ui
          :version     +version+
          :description "a high level interface to hoplon."
          :url         "https://github.com/hoplon/ui"
          :scm         {:url "https://github.com/hoplon/ui"}
          :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}}
  serve {:port        3010})
