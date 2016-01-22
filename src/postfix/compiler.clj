(ns postfix.compiler
  (:refer-clojure :exclude [rem])
  (:require [clojure.template :as template]
            [postfix.util :as u]))

(declare compile-instructions)

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
        then (peek stack)]
    (if (number? pred)
      (if (= pred 0)
        (-> stack pop pop pop (conj else))
        (-> stack pop pop pop (conj then)))
      (conj stack `(if (= ~pred 0) ~else ~then)))))

(defn exec [stack]
  (let [top (peek stack)
        stack (pop stack)]
    (if (vector? top)
      (compile-instructions stack top)
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

(defmethod compile-instruction :executable-sequence [stack instruction]
  (conj stack (vec instruction)))

(defn make-arg-vector [num-args]
  (vec (repeatedly num-args (partial gensym "postfix-arg"))))

(defn compile-instructions [stack instructions]
  (reduce compile-instruction stack instructions))

(defmacro postfix [num-args & program]
  (let [program-args (make-arg-vector num-args)
        arg-stack (vec (reverse program-args))
        compiled-program (compile-instructions arg-stack program)
        ret (peek compiled-program)]
    `(fn ~program-args ~ret)))
