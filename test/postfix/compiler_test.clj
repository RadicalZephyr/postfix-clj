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
    #_(postfix-test (sut/postfix 0 1 2 3 pop)
                  [] 2)
    #_(postfix-test (sut/postfix 0 1 2 swap 3 pop)
                    [] 1)))
