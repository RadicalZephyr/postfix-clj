(ns postfix.compiler
  (:require [postfix.compiler.operators]
            [postfix.compiler.program :as prog]))

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
  (conj stack (postfix* executable-sequence)))

(defn compile-instructions [program instructions]
  (reduce compile-instruction program instructions))

(defn generate-program-fn
  ([compiled-program]
   (generate-program-fn compiled-program (prog/args-used compiled-program)))

  ([compiled-program num-args]
   (let [program-args (prog/program-args* compiled-program num-args)
         body (prog/program-body compiled-program)]
     `(fn ~program-args ~body))))

(defn postfix*
  ([instructions]
   (let [compiled-program (compile-instructions (prog/empty-program) instructions)]
     (generate-program-fn compiled-program)))

  ([num-args instructions]
   (let [compiled-program (compile-instructions (prog/empty-program num-args) instructions)]
     (generate-program-fn compiled-program num-args))))

(defmacro postfix [num-args & instructions]
  (postfix* num-args instructions))
