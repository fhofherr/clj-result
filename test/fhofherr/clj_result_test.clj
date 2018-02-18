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

(deftest m-bind

  (testing "apply function to success"
    (let [init-success (result/success 1)
          f (fn [v] (result/success (+ 1 v)))
          res (result/m-bind init-success f)]
      (is (result/success? res))
      (is (= 2 (result/value res)))))

  (testing "left identity for success"
    (let [f (fn [v] (result/success (+ 1 v)))]
      (is (= (result/m-bind (result/success 1) f)
            (f 1)))))

  (testing "right identity for success"
    (let [s (result/success 1)
          f result/success]
      (is (= s (result/m-bind s f)))))

  (testing "associativity"
    (let [s (result/success 1)
          f (fn [v] (result/success (+ 1 v)))
          g (fn [v] (result/success (+ 10 v)))]
      (is (= (result/m-bind (result/m-bind s f)
                            g)
             (result/m-bind s
                            (fn [v] (result/m-bind (f v) g)))))))

  (testing "do not apply function to error"
    (let [init-error (result/error 1)
          f (fn [v] (result/success (+ 1 v)))
          res (result/m-bind init-error f)]
      (is (result/error? res))
      (is (= 1 (result/value res))))))
