(ns postfix.compiler-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [postfix.compiler :as sut]))

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
      (postfix-test (sut/postfix 2 swap)
                    [3 4]   4)
      (postfix-test (sut/postfix 3 pop swap)
                    [3 4 5] 5)

      (t/is (thrown-with-msg? clojure.lang.ArityException
                              #"Wrong number of args \(1\) passed to: "
                              ((sut/postfix 2 swap) 3)))
      (t/is (thrown-with-msg? clojure.lang.ArityException
                              #"Wrong number of args \(0\) passed to: "
                              ((sut/postfix 1 pop))))))

  (t/testing "Simple arithmetic programs"
    (postfix-test (sut/postfix 1 4 sub)
                  [3] -1)
    (postfix-test (sut/postfix 1 4 add 5 mul 6 sub 7 div)
                  [3]  4)
    (postfix-test (sut/postfix 5 add mul sub swap div)
                  [7 6 5 4 3] -20)
    (postfix-test (sut/postfix 3 4000 swap pop add)
                  [300 20 1] 4020)
    (postfix-test (sut/postfix 2 add 2 div)
                  [3 7] 5)
    (postfix-test (sut/postfix 1 3 div)
                  [17] 5)
    (postfix-test (sut/postfix 1 3 rem)
                  [17] 2)
    (postfix-test (sut/postfix 1 4 lt)
                  [3] 1)
    (postfix-test (sut/postfix 1 5 gt)
                  [4] 0)
    (postfix-test (sut/postfix 1 4 lt 10 add)
                  [3] 11))

  (t/testing "Programs with nget"
    (postfix-test (sut/postfix 1 1 nget mul)
                  [5] 25)
    (postfix-test (sut/postfix 2 1 nget)
                  [4 5] 4)
    (postfix-test (sut/postfix 2 2 nget)
                  [4 5] 5)
    (postfix-test (sut/postfix 2 nget)
                  [1 5] 5)
    (postfix-test (sut/postfix 5 sel nget)
                  [1 2 0 9 8] 8)
    (postfix-test (sut/postfix 5 sel nget)
                  [1 2 1 9 8] 9))

  (t/testing "Programs with sel"
    (postfix-test (sut/postfix 1 2 3 sel)
                  [1] 3)
    (postfix-test (sut/postfix 1 2 3 sel)
                  [0] 2)
    (postfix-test (sut/postfix 3 sel)
                  [3 2 0] 2)
    (postfix-test (sut/postfix 3 sel)
                  [3 2 1] 3))

  (t/testing "Programs with executable sequences"
    (postfix-test (sut/postfix 0 (0 swap sub) 7 swap exec)
                  [] -7)
    (postfix-test (sut/postfix 0 (7 swap exec) (0 swap sub) swap exec)
                  [] -7)
    (postfix-test (sut/postfix 4 lt (add) (mul) sel exec)
                  [3 4 5 6] 11)
    (postfix-test (sut/postfix 4 lt (add) (mul) sel exec)
                  [4 3 5 6] 30)
    #_(postfix-test (sut/postfix 2 (mul sub) (1 nget mul) 4 nget swap exec swap exec)
                  [-10 2] 42)))
