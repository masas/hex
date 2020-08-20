(ns hex.priority-set-test
  ""
  (:use [clojure.test])
  (:require [hex.priority-set :as ps]))

(deftest empty-test
  (testing "empty? が正しく動作するか？"
    (testing "空っぽなら真"
      (is (empty? (ps/priority-set)))
      (is (empty? (ps/priority-set-by >))))
    (testing "中身があれば偽"
      (is (not (empty? (ps/push! (ps/priority-set) 1 2))))
      (is (not (empty? (ps/priority-set 1 2)))))))

(deftest equalability-test
  (testing "同一オブジェクトでも個別に生成されたものは等しくない"
    (let [a (ps/priority-set)
          b (ps/priority-set)]
      (is (not (= a b)))))
  (testing "同一オブジェクトの場合だけ等しい"
    (let [a (ps/priority-set)]
      (is (= a a))
      (is (empty? a))
      (ps/push! a 1 [100])
      (is (not (empty? a)))
      (is (= a a)))))

(deftest seqable-test
  (testing "seqできます？"
    (is (= '(2 4 6 8) (seq (ps/priority-set 1 2 3 4 5 6 7 8))))
    (is (= '(8 6 4 2) (seq (ps/priority-set-by > 1 2 3 4 5 6 7 8))))))

(deftest like-a-set-test
  (testing "プライオリティが異なってもsetはsetなので単一化"
    (let [myset (ps/priority-set 1 2 3 4 5 6 7 8)]
      (is (= '(2 4 6 8) (seq myset)))
      (is (= '(8 2 4 6) (seq (ps/push! myset 0 8)))))))
