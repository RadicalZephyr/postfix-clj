(ns postfix.compiler
  (:require [postfix.compiler.program :as prog]
            [postfix.compiler.operators]))

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
  (if-let [fn (ns-resolve (find-ns 'postfix.compiler.operators)
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
  (vec (repeatedly num-args prog/postfix-arg)))

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
