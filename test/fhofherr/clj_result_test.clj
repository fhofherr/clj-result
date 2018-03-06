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

  (testing "auto-wrap values into success"
    (let [f #(+ 1 %)
          res (result/m-bind 1 f)]
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

(deftest swap-pairs
  (is (= [] (#'result/swap-pairs [])))
  (is (thrown? AssertionError (#'result/swap-pairs [:a])))
  (is (= [:b :a] (#'result/swap-pairs [:a :b]))))

(deftest attempt

  (testing "single binding"
    (is (= (result/success :a)
           (result/attempt [x (result/success :a)]
                           x))))

  (testing "multiple bindings"
    (is (= (result/success :b)
           (result/attempt [x (result/success :a)
                            y (result/success :b)]
                           y))))

  (testing "abort on error"
    (is (= (result/error :error)
           (result/attempt [x (result/error :error)
                            y (result/success :a)]
                           y)))))

(deftest attempt-v

  (testing "get success value"
    (is (= :a
           (result/attempt-v [x (result/success :a)]
                           x))))

  (testing "get error value"
    (is (= :error
           (result/attempt-v [x (result/error :error)
                            y (result/success :a)]
                           y)))))

(deftest attempt-as->

  (testing "evaluate to last success"
    (is (= (result/success :a)
           (result/attempt-as-> (result/success :a) $)))
    (is (= (result/success 2)
           (result/attempt-as-> (result/success 1) $
                                (result/success (+ 1 $))))))

  (testing "evaluate to first error"
    (is (= (result/error :a)
           (result/attempt-as-> (result/error :a) $)))
    (is (= (result/error :error)
           (result/attempt-as-> (result/success :a) $
                                (result/error :error)
                                (result/success :b))))))

(deftest attempt-v-as->

  (testing "return value of last success"
    (is (= :a (result/attempt-v-as-> (result/success :a) $)))
    (is (= 2 (result/attempt-v-as-> (result/success 1) $
                                    (result/success (+ 1 $))))))

  (testing "return value of first error"
    (is (= :error (result/attempt-v-as-> (result/error :error) $)))
    (is (= :error (result/attempt-v-as-> (result/success 1) $
                                         (result/error :error)
                                         (result/success (+ 1 $)))))))

(deftest map-e-map-s-and-map-v
  (let [error (result/error 1)
        success (result/success 1)]

    (testing "map-v applies a function to the results value"
      (is (= 2 (->> error (result/map-v inc) result/value)))
      (is (= 2 (->> success (result/map-v inc) result/value))))

    (testing "map-e applies a funtion to errors only"
      (is (= 2 (->> error (result/map-e inc) result/value)))
      (is (= 1 (->> success (result/map-e inc) result/value))))

    (testing "map-s applies a funtion to successes only"
      (is (= 1 (->> error (result/map-s inc) result/value)))
      (is (= 2 (->> success (result/map-s inc) result/value))))))
