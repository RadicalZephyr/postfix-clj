(ns postfix.compiler-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [postfix.compiler :as sut]))

(t/deftest postfix-program-acts-like-a-stack
  (t/is (nil? (seq (sut/empty-program))))

  (t/is (= '(1)
           (-> (sut/empty-program)
               (conj 1)
               seq)))

  (t/is (= 1 (-> (sut/empty-program)
                 (conj 1)
                 peek)))
  (t/is (= 2 (-> (sut/empty-program)
                 (conj 1 2)
                 peek)))

  (t/is (= 1 (-> (sut/empty-program)
                 (conj 1 2)
                 pop
                 peek))))

(t/deftest postfix-program-has-other-attributes
  (t/testing "the empty program"
    (t/testing "has arguments"
      (t/is (= 0 (sut/args-used (sut/empty-program))))
      (t/is (= [] (sut/program-args (sut/empty-program))))
      (t/is (every? symbol? (sut/program-args* (sut/empty-program) 2))))

    (t/testing "has a nil body"
      (t/is (nil? (sut/program-body (sut/empty-program))))))

  (t/testing "a program with a constant body"
    (t/is (= 1 (-> (sut/empty-program)
                   (conj 1)
                   sut/program-body))))

  (t/testing "a program that has used the first arg"
    (let [program (sut/empty-program)
          arg (peek program)
          args (sut/program-args program)]

     (t/testing "has arguments"
       (t/is (= 1 (sut/args-used program)))
       (t/is (vector? args))
       (t/is (= 1 (count args))))

     (t/testing "args-used and program-args should be the same"
       (t/is (= [arg] (sut/program-args program))))

     (t/testing "has argument as body if no program given"
       (t/is (= arg (sut/program-body program))))))

  (t/testing "a program that has used more than one arg"
    (let [program (sut/empty-program)
          arg-sym1 (peek program)
          program-popped (pop program)
          arg-sym2 (peek program-popped)]

      (t/is (symbol? arg-sym1))
      (t/is (str/starts-with? (name arg-sym1) "postfix-arg"))
      (t/is (not= arg-sym1 arg-sym2))

      (t/testing "args-used and programs-args should be the same"
        (t/is (= 2 (sut/args-used program)))
        (t/is (= 2 (count (sut/program-args program))))
        (t/is (= [arg-sym1 arg-sym2] (sut/program-args program))))))

  (t/testing "complex stack usage"
    (t/testing "with nget"
      (let [program (conj (sut/empty-program) 1)
            program (sut/nget program)]
        (t/is (= 1 (sut/args-used program))))
      (let [program (conj (sut/empty-program) 3)
            program (sut/nget program)]
        (t/is (= 3 (sut/args-used program))))
      #_(let [program (sut/nget (sut/empty-program))]
        (t/is (= 3 (sut/args-used program)))))

    (t/testing "with swap"
      (let [program (sut/swap (sut/empty-program))]
        (t/is (= 2 (sut/args-used program)))))))

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
    (postfix-test (sut/postfix 2 1 nget)
                  [4 5] 4)
    (postfix-test (sut/postfix 2 2 nget)
                  [4 5] 5))

  (t/testing "Programs with sel"
    (postfix-test (sut/postfix 1 2 3 sel)
                  [1] 3)
    (postfix-test (sut/postfix 1 2 3 sel)
                  [0] 2))

  (t/testing "Programs with executable sequences"
    (postfix-test (sut/postfix 0 (0 swap sub) 7 swap exec)
                  [] -7)
    (postfix-test (sut/postfix 0 (7 swap exec) (0 swap sub) swap exec)
                  [] -7)
    #_(postfix-test (sut/postfix 4 lt (add) (mul) sel exec)
                  [3 4 5 6] 11)
    #_(postfix-test (sut/postfix 4 lt (add) (mul) sel exec)
                  [4 3 5 6] 30)))
