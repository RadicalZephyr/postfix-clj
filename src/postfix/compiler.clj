(ns postfix.compiler
  (:require [clojure.walk :as w]
            [postfix.compiler.operators :as op]
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

(defmethod instruction->ast :command [stack instruction]
  (if-let [ast-node (op/build-ast-node instruction stack)]
    ast-node
    (throw (ex-info "Could not resolve symbol"
                    {:instruction instruction}))))

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

(defmethod compile-ast-node :operator [[_ data]]
  data)

(declare compile-ast)

(defmethod compile-ast-node :executable-sequence [[_ data]]
  (let [{:keys [program-args program-ast]} data
        compiled-program (compile-ast program-ast)]
    `(fn ~program-args ~compiled-program)))

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
        program-ast (prog/program-body ast)]
    [:executable-sequence {:program-args program-args
                           :program-ast program-ast}]))

(defmacro postfix [num-args & instructions]
  (let [ast (build-ast (prog/empty-program num-args) instructions)
        program-args (prog/program-args* ast num-args)
        compiled-program (compile-ast (prog/program-body ast))]
    `(fn ~program-args ~compiled-program)))
