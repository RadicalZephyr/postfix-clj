(ns postfix.compiler.operators
  (:refer-clojure :exclude [rem])
  (:require [clojure.string :as str]
            [clojure.template :as template]
            [postfix.util :as u]))

(defn swap [stack]
  (let [n1 (u/peekn stack 0)
        n2 (u/peekn stack 1)]
    (-> stack pop pop
        (conj n1)
        (conj n2))))

(defn nget [stack]
  (let [n (u/peekn stack 0)]
    (-> stack pop
        (conj
         `(u/peekn ~(vec (seq stack)) ~n)))))

(defn sel [stack]
  (let [pred (u/peekn stack 2)
        else (u/peekn stack 1)
        then (u/peekn stack 0)
        stack (u/popn stack 3)]
    (conj stack `(if (= ~pred 0) ~else ~then))))

(defn exec [stack]
  (let [top (peek stack)
        stack (pop stack)]
    (conj stack `(~top ~@(take 2 (reverse stack))))))

(defn wrap-bool [f]
  (fn [l r] (if (f l r) 1 0)))

(def lt-fn `(wrap-bool <))
(def gt-fn `(wrap-bool >))

(defn make-operation [op l r]
  (list op l r))

(defmacro defbinary-stack-op [name operator]
  `(defn ~name [stack#]
     (let [~'n1 (u/peekn stack# 0)
           ~'n2 (u/peekn stack# 1)]
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
