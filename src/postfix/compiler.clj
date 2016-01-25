(ns postfix.compiler
  (:refer-clojure :exclude [rem])
  (:require [clojure.string :as str]
            [clojure.template :as template]
            [postfix.util :as u])
  (:import (clojure.lang Seqable IPersistentStack)
           java.io.Writer))

(def postfix-arg (partial gensym "postfix-arg"))

(defprotocol IPostfixProgram
  (args-used [prog] "Return the number of args used by this program.")
  (program-args* [prog n]
    "Return an argument vector of size n for this program.

    `n' cannot be less than the minimum number of args required.")
  (program-args [prog] "Return the minimum required argument vector for this program.")
  (program-body [prog] "Return the body of this program."))

(deftype PostfixProgram [stack depth arg-source args-used]
  Seqable
  (seq [this] (seq stack))

  IPersistentStack
  (cons [this v] (PostfixProgram. (conj stack v) depth arg-source args-used))
  (peek [this] (if (seq stack)
                 (peek stack)
                 (do
                   (swap! args-used max (inc depth))
                   (nth arg-source depth))))
  (pop [this]
    (if (seq stack)
      (PostfixProgram. (pop stack) depth arg-source args-used)
      (PostfixProgram. stack (inc depth) arg-source args-used)))

  IPostfixProgram
  (args-used [prog] @args-used)
  (program-args* [prog n]
    (if (>= n @args-used)
      (vec (take n arg-source))
      (throw (ex-info "Too few arguments specified for program."
                      {:program prog
                       :requested-arg-count n
                       :needed-arg-count @args-used}))))
  (program-args [prog] (program-args* prog @args-used))

  (program-body [prog]
    (if-let [top (peek stack)]
      top
      (when (> @args-used 0)
        (first arg-source)))))

(defmethod print-method PostfixProgram [program ^Writer w]
  (.write w (format "#postfix.compiler.PostfixProgram[%d,"
                    (args-used program)))
  (print-method (seq program) w)
  (.write w "]"))

(defmethod print-dup PostfixProgram [program w]
  (print-method program w))

(defn empty-program []
  (let [args (repeatedly postfix-arg)]
    (->PostfixProgram [] 0 args (atom 0))))

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

(defmethod compile-instruction :number [stack value]
  (conj stack value))

(defn lookup [instruction]
  (if-let [fn (ns-resolve (find-ns 'postfix.compiler)
                          instruction)]
    fn
    (throw (ex-info "Could not resolve symbol"
                    {:instruction instruction}))))

(defmethod compile-instruction :command [stack instruction]
  ((lookup instruction) stack))

(declare postfix*)

(defmethod compile-instruction :executable-sequence [stack executable-sequence]
  (conj stack (postfix* 1 executable-sequence)))

(defn make-arg-vector [num-args]
  (vec (repeatedly num-args postfix-arg)))

(defn compile-instructions [stack instructions]
  (reduce compile-instruction stack instructions))

(defn postfix* [num-args instructions]
  (let [program-args (make-arg-vector num-args)
        arg-stack (vec (reverse program-args))
        compiled-program (compile-instructions arg-stack instructions)
        ret (peek compiled-program)]
    `(fn ~program-args ~ret)))

(defmacro postfix [num-args & program]
  (postfix* num-args program))
