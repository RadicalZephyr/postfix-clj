(ns postfix.core-test
  (:require [clojure.test :refer :all]
            [postfix.core :refer :all]))

(deftest postfix-test
  (testing "Basic programs"
    (is (= ((postfix 0 1 2 3)) 3) "Only the top stack value is returned.")
    (testing "with pop"
      (is (= ((postfix 0 1 2 3 pop)) 2))
      (testing "and with swap"
        (is (= ((postfix 0 1 2 swap 3 pop)) 1)))))
  (testing "Basic argument handling"
    (is (= ((postfix 2) 3 4) 3))
    (testing "with pop and swap"
      (is (= ((postfix 2 swap) 3 4) 4))
      (is (= ((postfix 3 pop swap) 3 4 5)))))
  (testing "Error productions"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"swap: not enough values"
                          ((postfix 0 1 swap))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pop: empty stack"
                          ((postfix 0 1 pop pop))))))

(deftest subcommand-test
  (testing "Add command"
    (is (= (add-cmd [3 4])
           [7]))
    (is (= (add-cmd [0 3 2])
           [0 5]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"add: not enough values on the stack"
                          (add-cmd [1]))))

  (testing "Sub command"
    (is (= (sub-cmd [1 2])
           [-1]))
    (is (= (sub-cmd [0 3 2])
           [0 1]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"sub: not enough values on the stack"
                          (sub-cmd [1])))))
