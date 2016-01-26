(ns postfix.compiler
  (:require [postfix.compiler.operators]
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

(defmethod compile-ast-node :default [ast]
  ast)

(defn compile-ast [ast]
  ast)

(defn generate-program-fn
  ([compiled-program]
   (generate-program-fn compiled-program (prog/args-used compiled-program)))

  ([compiled-program num-args]
   (let [program-args (prog/program-args* compiled-program num-args)
         body (prog/program-body compiled-program)]
     `(fn ~program-args ~body))))

(defn make-executable-sequence [instructions]
  (let [ast (build-ast (prog/empty-program) instructions)
        compiled-program (compile-ast ast)]
    (generate-program-fn compiled-program)))

(defmacro postfix [num-args & instructions]
  (let [ast (build-ast (prog/empty-program num-args) instructions)
        compiled-program (compile-ast ast)]
    (generate-program-fn compiled-program num-args)))
