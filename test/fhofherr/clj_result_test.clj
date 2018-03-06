(ns fhofherr.clj-result-test
  (:require [clojure.test :refer :all]
            [fhofherr.clj-result :as result]))

(deftest error-implments-result
  (let [value "Ooops!"
        err (result/error value)]
    (is (true? (result/result? err)))
    (is (= value (result/value err)))))

(deftest m-bind

  (testing "apply function to value"
    (let [init-value 1
          f (fn [v] (+ 1 v))
          res (result/m-bind init-value f)]
      (is (= 2 res))))

  (testing "left identity"
    (let [f (fn [v] (+ 1 v))]
      (is (= (result/m-bind 1 f) (f 1)))))

  (testing "right identity"
    (let [s 1
          f identity]
      (is (= s (result/m-bind s f)))))

  (testing "associativity"
    (let [s 1
          f (fn [v] (+ 1 v))
          g (fn [v] (+ 10 v))]
      (is (= (result/m-bind (result/m-bind s f) g)
             (result/m-bind s (fn [v] (result/m-bind (f v) g)))))))

  (testing "do not apply function to error"
    (let [init-error (result/error 1)
          f (fn [v] (+ 1 v))
          res (result/m-bind init-error f)]
      (is (result/result? res))
      (is (= 1 (result/value res))))))

(deftest swap-pairs
  (is (= [] (#'result/swap-pairs [])))
  (is (thrown? AssertionError (#'result/swap-pairs [:a])))
  (is (= [:b :a] (#'result/swap-pairs [:a :b]))))

(deftest attempt

  (testing "single binding"
    (is (= :a (result/attempt [x :a] x))))

  (testing "multiple bindings"
    (is (= :b
           (result/attempt [x :a
                            y :b]
                           y))))

  (testing "abort on error"
    (is (= (result/error :error)
           (result/attempt [x (result/error :error)
                            y :a]
                           y)))))

(deftest attempt-as->

  (testing "evaluate to last value"
    (is (= :a
           (result/attempt-as-> :a $)))
    (is (= 2
           (result/attempt-as-> 1 $
                                (+ 1 $)))))

  (testing "evaluate to first error"
    (is (= (result/error :a)
           (result/attempt-as-> (result/error :a) $)))
    (is (= (result/error :error)
           (result/attempt-as-> :a $
                                (result/error :error)
                                :b)))))

(deftest map-e-map-s-and-map-v
  (let [error (result/error 1)
        v 1]

    (testing "map-v applies a function to the results value"
      (is (= 2 (->> error (result/map-v inc) result/value)))
      (is (= 2 (->> v (result/map-v inc) result/value))))

    (testing "map-e applies a funtion to errors only"
      (is (= 2 (->> error (result/map-e inc) result/value)))
      (is (= 1 (->> v (result/map-e inc) result/value))))

    (testing "map-s applies a funtion to non-errors only"
      (is (= 1 (->> error (result/map-s inc) result/value)))
      (is (= 2 (->> v (result/map-s inc) result/value))))))
