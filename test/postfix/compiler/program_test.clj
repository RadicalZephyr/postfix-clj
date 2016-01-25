(ns postfix.compiler.program-test
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [postfix.compiler.program :as sut]
            [postfix.compiler.operators :as op]))

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
            program (op/nget program)]
        (t/is (= 1 (sut/args-used program))))
      (let [program (conj (sut/empty-program) 3)
            program (op/nget program)]
        (t/is (= 3 (sut/args-used program))))
      (let [program (op/nget (sut/empty-program))]
        (t/is (= 1 (sut/args-used program)))))

    (t/testing "with swap"
      (let [program (op/swap (sut/empty-program))]
        (t/is (= 2 (sut/args-used program)))))))
