(ns hoplon-test.ui
  (:import
    [java.net                    URL]
    [org.openqa.selenium         By Dimension Point]
    [org.openqa.selenium.remote  RemoteWebDriver DesiredCapabilities])
  (:require
    [adzerk.env :as env]
    [clojure.test :refer [deftest is use-fixtures]]))

;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(env/def SAUCE_LABS_USERNAME   :required
         SAUCE_LABS_ACCESS_KEY :required
         TRAVIS_JOB_NUMBER     nil)

(def url (str "https://" SAUCE_LABS_USERNAME ":" SAUCE_LABS_ACCESS_KEY "@ondemand.saucelabs.com:443/wd/hub"))

;;; browser fixtures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def browsers
  {:chrome  (DesiredCapabilities/chrome)
   :firefox (DesiredCapabilities/firefox)
   :ie      (DesiredCapabilities/internetExplorer)
   :edge    (DesiredCapabilities/edge)
   :safari  (DesiredCapabilities/safari)})

(def oss
  {:linux   "Linux"
   :osx     "OS X"
   :windows "Windows"})

(defn capability [bs-key bs-ver os-key os-ver]
  (let [bs-key (if (= bs-key :iedge) (if (< (read-string bs-ver) 12) :ie :edge) bs-key)]
    (doto (browsers bs-key)
      (.setCapability "version"           bs-ver)
      (.setCapability "platform"          (str (oss os-key) " " os-ver))
      (.setCapability "tunnel-identifier" TRAVIS_JOB_NUMBER))))

(defn driver [& args]
  (boot.util/info "testing with %s \n" args)
  (RemoteWebDriver. (URL. url) (apply capability args)))

(def ^:dynamic *driver*)

(defn mkfixture [args]
  (fn [test]
    (let [driver (apply driver args)]
      (doto (.window (.manage driver))
        (.setPosition (Point. 0 0))
        (.setSize (Dimension. 1024 768)))
      (try
          (.get driver "http://localhost:5000")
          (binding [*driver* driver]
            (test))
          (finally (.quit driver))))))

(def supported-browsers
  ;; https://wiki.saucelabs.com/display/DOCS/Platform+Configurator
  [:chrome  "26.0"     :windows "7"
   :firefox "4.0"      :windows "7"
   :iedge   "11.0"     :windows "7"
   :iedge   "14.14393" :windows "10"
   :safari  "7.0"      :osx     "10.9"])

(apply use-fixtures :each (map mkfixture (partition 4 supported-browsers)))

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn size [e] (let [s (.getSize e)] (vector (.getWidth s) (.getHeight s))))

;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest title
  (let [t (.getTitle *driver*)]
    (is (= "Hoplon UI" t) "title is Hoplon UI")))
