(ns postfix.compiler
  (:refer-clojure :exclude [rem])
  (:require [clojure.string :as str]
            [clojure.template :as template]
            [postfix.util :as u])
  (:import (clojure.lang Seqable IPersistentStack)))

(def postfix-arg (partial gensym "postfix-arg"))

(defprotocol IPostfixProgram
  (args-used [prog] "Return the number of args used by this program.")
  (-current-arg [prog] "Internal method for getting the \"current\" argument.")
  (program-args [prog] "Return the argument vector for this program.")
  (program-body [prog] "Return the body of this program."))

(deftype PostfixProgram [stack arg-source args-used]
  Seqable
  (seq [this] (seq stack))

  IPersistentStack
  (cons [this v] (PostfixProgram. (conj stack v) arg-source args-used))
  (peek [this] (if (seq stack)
                 (peek stack)
                 (do
                   (when (= 0 @args-used) (swap! args-used inc))
                   (-current-arg this))))
  (pop [this]
    (if (seq stack)
      (PostfixProgram. (pop stack) arg-source args-used)
      (do
        (swap! args-used inc)
        this)))

  IPostfixProgram
  (args-used [prog] @args-used)
  (-current-arg [prog] (when (> @args-used 0) (nth arg-source (dec @args-used))))
  (program-args [prog] (vec (take @args-used arg-source)))
  (program-body [prog] (if-let [top (peek stack)]
                         top
                         (when (> @args-used 0)
                           (first arg-source)))))

(defn empty-program []
  (->PostfixProgram [] (repeatedly postfix-arg) (atom 0)))

(defn swap [stack]
  (let [n1 (peek stack)
        n2 (peek (pop stack))]
    (-> stack pop pop
        (conj n1)
        (conj n2))))

(defn nget [stack]
  (let [n (peek stack)
        data (peek (u/popn stack n))]
    (-> stack pop
        (conj data))))

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

(defmulti compile-instruction
  (fn [stack instruction]
    (cond
      (number? instruction) :number
      (symbol? instruction) :command
      (list? instruction) :executable-sequence
      :else :default)))

(defmethod compile-instruction :default [stack instruction]
  (throw (ex-info "Encountered unknown instruction."
                  {:stack stack :instruction instruction})))

(defmethod compile-instruction :number [stack instruction]
  (conj stack instruction))

(defn lookup [instruction]
  (if-let [fn (ns-resolve (find-ns 'postfix.compiler)
                          instruction)]
    fn
    (throw (ex-info "Could not resolve symbol"
                    {:instruction instruction}))))

(defmethod compile-instruction :command [stack instruction]
  ((lookup instruction) stack))

(declare postfix*)

(defmethod compile-instruction :executable-sequence [stack instruction]
  (conj stack (postfix* 1 instruction)))

(defn make-arg-vector [num-args]
  (vec (repeatedly num-args postfix-arg)))

(defn compile-instructions [stack instructions]
  (reduce compile-instruction stack instructions))

(defn postfix* [num-args program]
  (let [program-args (make-arg-vector num-args)
        arg-stack (vec (reverse program-args))
        compiled-program (compile-instructions arg-stack program)
        ret (peek compiled-program)]
    `(fn ~program-args ~ret)))

(defmacro postfix [num-args & program]
  (postfix* num-args program))
