(ns postfix.interpreter-test
  (:require [clojure.test :refer :all]
            [postfix.interpreter :refer :all]))

(defmacro postfix-test [postfix-prog args result]
  `(is (~'= (~postfix-prog ~@args)
            ~result)))

(deftest postfix-test
  (testing "Basic programs"
    (postfix-test (postfix 0 1 2 3)
                  [] 3)
    (postfix-test (postfix 0 1 2 3 pop)
                  [] 2)
    (postfix-test (postfix 0 1 2 swap 3 pop)
                  [] 1))

  (testing "Basic argument handling"
    (postfix-test (postfix 2)
                  [3 4] 3)

    (testing "with pop and swap"
      (postfix-test (postfix 2 swap)
                    [3 4]   4)
      (postfix-test (postfix 3 pop swap)
                    [3 4 5] 5)

      (is (thrown-with-msg? clojure.lang.ArityException
                            #"Wrong number of args \(1\) passed to: "
                            ((postfix 2 swap) 3)))
      (is (thrown-with-msg? clojure.lang.ArityException
                            #"Wrong number of args \(0\) passed to: "
                            ((postfix 1 pop))))))

  (testing "Simple arithmetic programs"
    (postfix-test (postfix 1 4 sub)
                  [3] -1)
    (postfix-test (postfix 1 4 add 5 mul 6 sub 7 div)
                  [3]  4)
    (postfix-test (postfix 5 add mul sub swap div)
                  [7 6 5 4 3] -20)
    (postfix-test (postfix 3 4000 swap pop add)
                  [300 20 1] 4020)
    (postfix-test (postfix 2 add 2 div)
                  [3 7] 5)
    (postfix-test (postfix 1 3 div)
                  [17] 5)
    (postfix-test (postfix 1 3 rem)
                  [17] 2)
    (postfix-test (postfix 1 4 lt)
                  [3] 1)
    (postfix-test (postfix 1 4 lt)
                  [5] 0)
    (postfix-test (postfix 1 4 lt 10 add)
                  [3] 11))

  (testing "Programs with nget"
    (postfix-test (postfix 2 1 nget)
                  [4 5] 4)
    (postfix-test (postfix 2 2 nget)
                  [4 5] 5))

  (testing "Error productions"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"swap: not enough values"
                          ((postfix 0 1 swap))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pop: empty stack"
                          ((postfix 0 1 pop pop))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"nget: index \d+ is out of range"
                          ((postfix 2 3 nget) 4 5)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"nget: index \d+ is out of range"
                          ((postfix 2 0 nget) 4 5)))))

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
  (defbinary-op-test div quot)
  (defbinary-op-test rem rem)

  (testing "Pop command"
    (is (= (pop-cmd [1])
           [])
        (= (pop-cmd [0 1])
           [0]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"pop: empty stack"
                          (pop-cmd []))))

  (testing "Swap command"
    (is (= (swap-cmd [1 3])
           [3 1]))
    (is (= (swap-cmd [0 1 3])
           [0 3 1])))

  (testing "Sel command"
    (is (= (sel-cmd [1 2 3])
           [2])
        (= (sel-cmd [0 2 3])
           [3]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"sel: not enough values on the stack"
                          (sel-cmd [0 1])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"sel: not enough values on the stack"
                          (sel-cmd [0]))))
  (testing "Nget command"
    (is (= (nget-cmd [5 4 3 2 1 3])
           [5 4 3 2 1 3]))
    (is (= (nget-cmd [5 4 3 2 1 1])
           [5 4 3 2 1 1]))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"nget: value at index \d+ is not a number"
                          (nget-cmd [3 2 1 'pop 1])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"nget: index value '\w*?' is not a number"
                          (nget-cmd [3 2 1 'pop])))))
