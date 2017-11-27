(ns evening.unify-test
  (:require [clojure.test :refer :all]
            [evening.unify :refer :all]))

(deftest bindings-test
  (testing "basic bindings test"
    (is (= {:bar 3}
           (bindings {:foo {:var :bar}}
                     {:foo 3}))))
  (testing "two bindings"
    (is (= {:bar 3 :quux 5}
           (bindings {:foo {:var :bar} :baz {:var :quux}}
                     {:foo 3 :baz 5}))))
  (testing "nested structure"
    (is (= {:bar 3}
           (bindings {:foo {:baz {:var :bar}}}
                     {:foo {:baz 3}}))))
  (testing "two bindings, nested structure"
    (is (= {:bar 3 :quux 5}
           (bindings {:foo {:baz {:var :bar}} :bing {:var :quux}}
                     {:foo {:baz 3} :bing 5}))))
  (testing "exact match"
    (is (= {}
           (bindings {:foo {:bar 3}}
                     {:foo {:bar 3}}))))
  (testing "same value"
    (is (= {:bar 3}
           (bindings {:foo {:var :bar} :baz {:var :bar}}
                     {:foo 3 :baz 3})))))

(deftest nil-test
  (testing "different value"
    (is nil?
        (bindings {:foo :bar}
                  {:baz :quux})))
  (testing "different value nested"
    (is nil?
        (bindings {:foo {:bar 3}}
                  {:0 "hi" :bing {:baz 327}})))
  (testing "can't unify"
    (is nil?
        (bindings {:foo {:var :bar} :baz {:var :bar}}
                  {:foo 3 :baz 4}))))
