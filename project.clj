(defproject fhofherr/clj-result "0.1.0-SNAPSHOT"
  :description "Wrapper for results and errors"
  :url "https://github.com/fhofherr/clj-result"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :plugins [[lein-codox "0.10.3"]
            [lein-eftest "0.4.3"]
            [lein-cljfmt "0.5.7"]]
  :codox {:namespaces [#"^fhofherr\.clj-result\."]
          :metadata {:doc/format :markdown}}
  :aliases {"test" ["eftest"]}
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.11"
                                   :exclusions [org.clojure/clojure]]
                                  [eftest "0.4.3"]]}})
