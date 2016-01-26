(ns postfix.compiler
  (:require [clojure.walk :as w]
            [postfix.compiler.operators]
            [postfix.compiler.program :as prog]))

(defmulti instruction->ast
  (fn [stack instruction]
    (cond
      (number? instruction) :number
      (symbol? instruction) :command
      (list? instruction) :executable-sequence
      :else :default)))

(defmethod instruction->ast :default [stack instruction]
  (throw (ex-info "Encountered unknown instruction."
                  {:stack stack :instruction instruction})))

(defmethod instruction->ast :number [stack value]
  (conj stack value))

(defn lookup [instruction]
  (if-let [fn (ns-resolve (find-ns 'postfix.compiler.operators)
                          instruction)]
    fn
    (throw (ex-info "Could not resolve symbol"
                    {:instruction instruction}))))

(defmethod instruction->ast :command [stack instruction]
  ((lookup instruction) stack))

(declare make-executable-sequence)

(defmethod instruction->ast :executable-sequence [stack instructions]
  (conj stack (make-executable-sequence instructions)))

(defn build-ast [program instructions]
  (reduce instruction->ast program instructions))

(defmulti compile-ast-node first)

(defmethod compile-ast-node :default [ast-node]
  ast-node)

(defmethod compile-ast-node :number [[_ value]]
  value)

(defn dispatch-node [ast-node]
  (if (vector? ast-node)
    (compile-ast-node ast-node)
    ast-node))

(defn compile-ast [ast]
  (w/postwalk dispatch-node ast))

(defn make-executable-sequence [instructions]
  (let [ast (build-ast (prog/empty-program) instructions)
        args-used (prog/args-used ast)
        program-args (prog/program-args* ast args-used)
        compiled-program (compile-ast (prog/program-body ast))]
    `(fn ~program-args ~compiled-program)))

(defmacro postfix [num-args & instructions]
  (let [ast (build-ast (prog/empty-program num-args) instructions)
        program-args (prog/program-args* ast num-args)
        compiled-program (compile-ast (prog/program-body ast))]
    `(fn ~program-args ~compiled-program)))
