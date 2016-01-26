(ns postfix.compiler.operators
  (:refer-clojure :exclude [rem])
  (:require [clojure.string :as str]
            [clojure.template :as template]
            [postfix.util :as u]))

(defprotocol IPostfixOperator
  (expression-type [op]
    "Get the ultimate type of this expression.")
  (compile-operator [op]
    "Compile this operator into code."))

(extend-type clojure.lang.ISeq
  IPostfixOperator
  (expression-type [op])
  (compile-operator [op] op))

(extend-type clojure.lang.IPersistentVector
  IPostfixOperator
  (expression-type [[operator-key data]]
    (if (satisfies? IPostfixOperator data)
      (expression-type data)
      (throw (ex-info "Tried to get type of non-operator."
                      {:operator data}))))
  (compile-operator [[operator-key data]]
    (if (satisfies? IPostfixOperator data)
      (compile-operator data)
      (throw (ex-info "Tried to compile non-operator."
                      {:operator data})))))

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
         [:operator `(u/peekn ~(vec (seq stack)) ~n)]))))

(defn sel [stack]
  (let [pred (u/peekn stack 2)
        else (u/peekn stack 1)
        then (u/peekn stack 0)
        stack (u/popn stack 3)]
    (conj stack
          [:operator `(if (= ~pred 0) ~else ~then)])))

(defn exec [stack]
  (let [top (peek stack)
        stack (pop stack)]
    (conj stack
          [:operator `(~top ~@(take 2 (reverse stack)))])))

(defn wrap-bool [f]
  (fn [l r] (if (f l r) 1 0)))

(def lt-fn `(wrap-bool <))
(def gt-fn `(wrap-bool >))

(defmacro defbinary-stack-op [name operator]
  `(defn ~name [stack#]
     (let [n1# (u/peekn stack# 0)
           n2# (u/peekn stack# 1)]
       (-> stack# pop pop
           (conj [:operator (list ~operator n2# n1#)])))))

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

(def operator-build-fns
  {:pop pop
   :swap swap
   :nget nget
   :sel sel
   :exec exec

   :add add
   :sub sub
   :mul mul
   :div div
   :rem rem
   :lt lt
   :gt gt})

(defn build-ast-node
  "Create the AST node for this operator from the given stack."
  [op stack]
  ((operator-build-fns (keyword op)) stack))
