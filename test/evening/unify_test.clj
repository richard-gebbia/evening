(ns evening.unify-test
  (:require [clojure.test :refer :all]
            [evening.unify :refer :all]))

(deftest bindings-test
  (testing "basic bindings test"
    (is (= {:bar 3} (bindings {:foo {:var :bar}} {:foo 3})))))
