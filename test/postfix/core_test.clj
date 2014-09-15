(ns postfix.core-test
  (:require [clojure.test :refer :all]
            [postfix.core :refer [postfix]]))

(deftest postfix-test
  (testing "Basic programs"
    (is (= ((postfix 0 1 2 3)) 3) "Only the top stack value is returned.")
    (testing "with pop"
      (is (= ((postfix 0 1 2 3 'pop)) 2))
      (testing "and with swap"
        (is (= ((postfix 0 1 2 'swap 3 'pop)) 1)))))
  (testing "Basic argument handling"
    (is (= ((postfix 2) 3 4) 3))
    (testing "with pop and swap"
      (is (= ((postfix 2 'swap) 3 4) 4))
      (is (= ((postfix 3 'pop 'swap) 3 4 5))))))
