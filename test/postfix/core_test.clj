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

(defmacro defbinary-op-test [cmd-name op]
  (let [cmd-name (name cmd-name)
        fn-name  (symbol (str cmd-name "-cmd"))
        test-args [3 1]
        op-result (vector (apply (eval op) test-args))]
    `(testing ~(str cmd-name " command")
       (is (~'= (~fn-name ~test-args)
              ~op-result))
       (is (~'= (~fn-name ~(into [0] test-args))
              ~(into [0] op-result)))
       (is (~'thrown-with-msg? clojure.lang.ExceptionInfo
                               (re-pattern ~(str cmd-name
                                                 ": not enough values on the stack"))
                               (~fn-name [1]))))))

(deftest subcommand-test
  (defbinary-op-test add +)
  (defbinary-op-test sub -)
  (defbinary-op-test mul *)
  (defbinary-op-test div /))
