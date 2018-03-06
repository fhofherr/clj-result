(ns fhofherr.clj-result-test
  (:require [clojure.test :refer :all]
            [fhofherr.clj-result :as result]))

(deftest end
  (is (true? (result/end? (result/end nil))))
  (is (false? (result/continue? (result/end nil))))
  (is (false? (result/continue? (result/end nil))))
  (is (true? (result/continue? nil)))

  (let [value "value"]
    (is (true? (result/end? (result/end value))))
    (is (false? (result/continue? (result/end value))))
    (is (false? (result/end? value)))
    (is (true? (result/continue? value)))
    (is (= value (result/value (result/end value))))))

(deftest continue

  (testing "apply function to value"
    (let [value 1
          res (result/continue value #(+ 1 %))]
      (is (= 2 res))))

  (testing "left identity"
    (let [f (fn [v] (+ 1 v))]
      (is (= (result/continue 1 f) (f 1)))))

  (testing "right identity"
    (let [s 1
          f identity]
      (is (= s (result/continue s f)))))

  (testing "associativity"
    (let [s 1
          f (fn [v] (+ 1 v))
          g (fn [v] (+ 10 v))]
      (is (= (result/continue (result/continue s f) g)
             (result/continue s (fn [v] (result/continue (f v) g)))))))

  (testing "do not apply function to an end"
    (let [init-end (result/end 1)
          f (fn [v] (+ 1 v))
          res (result/continue init-end f)]
      (is (result/end? res))
      (is (= 1 (result/value res))))
    (is (= (result/end nil)
           (result/continue (result/end nil) (constantly 1))))))

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

  (testing "abort on end"
    (is (= (result/end :end)
           (result/attempt [x (result/end :end)
                            y :a]
                           y)))))

(deftest attempt-as->

  (testing "evaluate to last value"
    (is (= :a
           (result/attempt-as-> :a $)))
    (is (= 2
           (result/attempt-as-> 1 $
                                (+ 1 $)))))

  (testing "evaluate to first end"
    (is (= (result/end :a)
           (result/attempt-as-> (result/end :a) $)))
    (is (= (result/end :end)
           (result/attempt-as-> :a $
                                (result/end :end)
                                :b)))
    (is (= (result/end nil)
           (result/attempt-as-> :a $
                                (result/end nil)
                                :b))))

  (testing "do not continue after end"
    (let [called (atom false)]
      (is (= (result/end 2)
             (result/attempt-as-> 1 $
                                  (inc $)
                                  (result/end $)
                                  (reset! called true))))
      (is (false? @called)))))

(deftest map-e-map-s-and-map-v
  (let [end (result/end 1)
        v 1]

    (testing "map-v applies a function to the results value"
      (is (= 2 (->> end (result/map-v inc) result/value)))
      (is (= 2 (->> v (result/map-v inc) result/value))))

    (testing "map-e applies a funtion to ends only"
      (is (= 2 (->> end (result/map-e inc) result/value)))
      (is (= 1 (->> v (result/map-e inc) result/value))))

    (testing "map-s applies a funtion to non-ends only"
      (is (= 1 (->> end (result/map-s inc) result/value)))
      (is (= 2 (->> v (result/map-s inc) result/value))))))
