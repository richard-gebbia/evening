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
                     {:foo 3 :baz 3}))))
  (testing "same value, nested structure"
    (is (= {:bar 3}
           (bindings {:foo {:baz {:var :bar}} :quux {:var :bar}}
                     {:foo {:baz 3} :quux 3})))))

(deftest nil-test
  (testing "different value"
    (is (nil? (bindings {:foo :bar}
                        {:baz :quux}))))
  (testing "different value nested"
    (is (nil? (bindings {:foo {:bar 3}}
              {:0 "hi" :bing {:baz 327}}))))
  (testing "can't unify"
    (is (nil? (bindings {:foo {:var :bar} :baz {:var :bar}}
              {:foo 3 :baz 4}))))
  (testing "not enough"
    (is (nil? (bindings {:foo {:var :bar} :baz {:var :quux}}
              {:foo 3}))))
  (testing "not enough, same variable"
    (is (nil? (bindings {:foo {:var :bar} :baz {:var :bar}}
              {:foo 3}))))
  (testing "not enough, same variable, nested"
    (is (nil? (bindings {:foo {:0 {:var :bar} :1 {:var :bar}}}
              {:foo {:0 3}})))))

(deftest basic-test
  (testing "basic test"
    (is (= #{{:x "socrates"} {:x "plato"}}
            (all-bindings #{{:man {:var :x}}}
                          #{{:man "socrates"}
                            {:man "plato"}})))))

(deftest simple-unification-test
  (testing "simple unification"
    (is (= #{{:x 5 :y 3} {:x 7 :y 7}}
            (all-bindings #{{:p {:var :x}}
                            {:q {:0 {:var :x} :1 {:var :y}}}
                            {:r {:var :y}}}
                          #{{:p 5}
                            {:p 7}
                            {:q {:0 5 :1 3}}
                            {:q {:0 7 :1 7}}
                            {:r 3}
                            {:r 7}
                            {:z 8}})))))

(deftest same-predicate-unification-test
  (testing "same predicate unification"
    (is (= #{{:x 5} {:x 2}}
            (all-bindings #{{:p {:0 {:var :x} :1 {:var :x}}}}
                          #{{:p {:0 5 :1 5}}
                            {:p {:0 5 :1 6}}
                            {:p 7}
                            {:p {:0 7}}
                            {:p {:0 2 :1 2}}})))))

(deftest with-and-without-variables
  (let [data1 {:man "socrates"}
        data2 {:man "plato"}
        data3 {:sky :blue}
        data4 {:sky :red}
        matcher {:man {:var :x}}]
    (testing "without the variable-less data"
      (is (nil? (all-bindings #{data3 matcher}
                              #{data1 data2}))))
    (testing "with the wrong variable-less data"
      (is (nil? (all-bindings #{data3 matcher}
                              #{data1 data2 data4}))))
    (testing "with the right variable-less data"
      (is (= #{{:x "socrates"} {:x "plato"}} 
              (all-bindings #{data3 matcher}
                            #{data1 data2 data3})))))
  (let [data1 {:p {:0 {:sky :blue} :1 5}}
        data2 {:p {:0 {:sky :red} :1 6}}
        data3 {:q 6}
        data4 {:p {:sky :blue}}
        data5 {:p {:0 {:sky :blue}}}
        matcher {:p {:0 {:sky :blue} :1 {:var :x}}}]
    (testing "nested variable-less data"
      (is (= #{{:x 5}}
              (all-bindings #{matcher}
                            #{data1 data2 data3 data4 data5}))))))

(deftest complex-unification-test
  (testing "complex unification test"
    (let [data1 {:p {:0 {:q {:0 4 :1 2}} :1 2}}
          data2 {:p {:0 {:q {:0 3 :1 8}} :1 7}}
          data3 {:p {:0 {:q {:0 0 :1 5}} :1 5}}
          data4 {:p {:q 9}}
          data5 {:p {:0 {:q 9}}}
          data6 {:p {:0 {:q {:0 9}}}}
          data7 {:r 4}
          data8 {:r {:0 4}}
          matcher1 {:p {:0 {:q {:0 {:var :x} :1 {:var :y}}} :1 {:var :y}}}
          matcher2 {:r {:var :x}}]
      (is (= #{{:x 4 :y 2}}
              (all-bindings #{matcher1 matcher2}
                            #{data1 data2 data3 data4
                              data5 data6 data7 data8}))))))

(deftest basic-inference-test
  (testing "basic inference"
    (is (= #{{:mortal "socrates"} {:mortal "plato"}}
            (infer #{{:man {:var :x}}}
                   #{{:mortal {:var :x}}}
                   #{{:man "socrates"}
                     {:man "plato"}})))))

(deftest mccarthy-logic
  (testing "McCarthy logic"
    (let [fact1 {:walks-like-duck "dolan"}
          fact2 {:walks-like-duck "daffy"}
          fact3 {:looks-like-duck "dolan"}
          fact4 {:looks-like-duck "daffy"}
          fact5 {:quacks-like-duck "dolan"}
          premise1 {:walks-like-duck {:var :x}}
          premise2 {:looks-like-duck {:var :x}}
          premise3 {:quacks-like-duck {:var :x}}
          conclusion1 {:duck {:var :x}}
          inference (infer #{premise1 premise2 premise3}
                           #{conclusion1}
                           #{fact1 fact2 fact3 fact4 fact5})]
      (testing "dolan should be a duck"
        (is (contains? inference {:duck "dolan"})))
      (testing "daffy should not be a duck"
        (is (not (contains? inference {:duck "daffy"})))))))

(deftest closer-to-real-world-test
  (testing "Closer to real world, with submap variable substitution"
    (let [fact1 {:is-positive 5}
          fact2 {:is-positive 3}
          fact3 {:rect {:top 2 :left -4 :width 5 :height 3}}
          fact4 {:rect {:top 2 :left -4 :width 3 :height 3}}
          fact5 {:rect {:top 2 :left -4 :width -3 :height -3}}
          premise1 {:rect {:top {:var :top} :left {:var :left}
                          :width {:var :width} :height {:var :width}}}
          premise2 {:is-positive {:var :width}}
          conclusion {:square premise1}
          inference (infer #{premise1 premise2}
                          #{conclusion}
                          #{fact1 fact2 fact3 fact4 fact5})]
      (testing "squares are square"
        (is (contains? inference {:square fact4})))
      (testing "rectangles aren't necessarily square"
        (is (not (contains? inference {:square fact3}))))
      (testing "negative width doesn't count"
        (is (not (contains? inference {:square fact5})))))))
