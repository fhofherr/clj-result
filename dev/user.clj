(ns user
  (:require [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [eftest.runner :as t]))

(defn- do-test
  []
  (-> "test"
      t/find-tests
      t/run-tests))

(defn run-tests
  []
  (refresh :after 'user/do-test))
