(ns fhofherr.clj-result-test
  (:require [clojure.test :refer :all]
            [fhofherr.clj-result :as result]))

(deftest error-implments-result
  (let [value "Ooops!"
        err (result/error value)]
    (is (true? (result/error? err)))
    (is (false? (result/success? err)))
    (is (= value (result/value err)))))

(deftest success-implments-result
  (let [value "Yeah!"
        success (result/success value)]
    (is (false? (result/error? success)))
    (is (true? (result/success? success)))
    (is (= value (result/value success)))))
