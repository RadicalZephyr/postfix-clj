(ns postfix.compiler.operators
  (:refer-clojure :exclude [rem])
  (:require [clojure.string :as str]
            [clojure.template :as template]
            [postfix.util :as u]))

(defn swap [stack]
  (let [n1 (peek stack)
        n2 (peek (pop stack))]
    (-> stack pop pop
        (conj n1)
        (conj n2))))

(defn nget [stack]
  (let [n (peek stack)]
    (-> stack pop
        (conj
         (if (number? n)
           (u/peekn stack n)
           `(u/peekn ~(vec (seq stack)) ~n))))))

(defn sel [stack]
  (let [pred (peek (u/popn stack 2))
        else (peek (pop stack))
        then (peek stack)
        stack (u/popn stack 3)]
    (if (number? pred)
      (if (= pred 0)
        (conj stack else)
        (conj stack then))
      (conj stack `(if (= ~pred 0) ~else ~then)))))

(defn executable-sequence? [val]
  (and (seq? val)
       (= (first val)
          'clojure.core/fn)))

(defn postfix-arg-sym? [val]
  (and (symbol? val)
       (-> val name (str/starts-with? "postfix-arg"))))

(defn exec [stack]
  (let [top (peek stack)
        stack (pop stack)]
    (if (or (executable-sequence? top)
            (postfix-arg-sym? top))
      (conj stack `(~top ~@(take 2 (reverse stack))))
      (throw (ex-info "'exec' called on a non-executable sequence"
                      {:argument top :stack stack})))))

(defn wrap-bool [f]
  (fn [l r] (if (f l r) 1 0)))

(def lt-fn `(wrap-bool <))
(def gt-fn `(wrap-bool >))

(defn make-operation [op l r]
  (if (and (number? l) (number? r))
    (op l r)
    (list op l r)))

(defmacro defbinary-stack-op [name operator]
  `(defn ~name [stack#]
     (let [~'n1 (peek stack#)
           ~'n2 (peek (pop stack#))]
       (-> stack# pop pop
           (conj (make-operation ~operator ~'n2 ~'n1))))))

(defmacro defbinary-stack-ops [& args]
  `(template/do-template [name op]
                         (defbinary-stack-op name op)
                         ~@args))

(defbinary-stack-ops
  add +
  sub -
  mul *
  div quot
  rem clojure.core/rem
  lt lt-fn
  gt gt-fn)
