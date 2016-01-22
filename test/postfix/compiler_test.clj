(ns postfix.compiler-test
  (:require [postfix.compiler :as sut]
            [clojure.test :as t]))

(defmacro postfix-test [postfix-prog args result]
  `(t/is (~'= (~postfix-prog ~@args)
          ~result)))

(t/deftest postfix-test
  (t/testing "Basic programs"
    (postfix-test (sut/postfix 0 1)
                  [] 1)
    (postfix-test (sut/postfix 0 2)
                  [] 2)
    (postfix-test (sut/postfix 0 1 2 3)
                  [] 3)
    (postfix-test (sut/postfix 0 1 2 3 pop)
                  [] 2)
    (postfix-test (sut/postfix 0 1 2 3 swap pop)
                  [] 3))

(t/testing "Basic argument handling"
  (postfix-test (sut/postfix 2)
                [3 4] 3)

  (t/testing "with pop and swap"
    #_(postfix-test (sut/postfix 2 swap)
                    [3 4]   4)
    #_(postfix-test (sut/postfix 3 pop swap)
                    [3 4 5] 5)

    #_(t/is (thrown-with-msg? clojure.lang.ArityException
                              #"Wrong number of args \(1\) passed to: "
                              ((sut/postfix 2 swap) 3)))
    #_(t/is (thrown-with-msg? clojure.lang.ArityException
                              #"Wrong number of args \(0\) passed to: "
                              ((sut/postfix 1 pop)))))))
