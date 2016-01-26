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

(defn make-executable-sequence [instructions]
  (let [ast (build-ast (prog/empty-program) instructions)
        args-used (prog/args-used ast)
        program-args (prog/program-args* ast args-used)
        program-ast (prog/program-body ast)]
    [:executable-sequence {:program-args program-args
                           :program-ast program-ast}]))

(declare compile-ast)

(defn compile-operator-node [data]
  (op/compile-operator data))

(defn compile-executable-sequence-node [data]
  (let [{:keys [program-args program-ast]} data
        compiled-program (compile-ast program-ast)]
    `(fn ~program-args ~compiled-program)))

(defn merge-meta [obj metamap]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj (merge metamap (meta obj)))
    obj))

(def compile-map {:number identity
                  :operator compile-operator-node
                  :executable-sequence compile-executable-sequence-node})

(defn compile-ast [parse-tree]
  (if (and (sequential? parse-tree) (seq parse-tree))
    (cond
      (vector? parse-tree)
      (if-let [transform (compile-map (first parse-tree))]
        (apply transform (map compile-ast
                              (next parse-tree)))
        (mapv compile-ast parse-tree))

      :else (map compile-ast parse-tree))
    parse-tree))

(defmacro postfix [num-args & instructions]
  (let [ast (build-ast (prog/empty-program num-args) instructions)
        program-args (prog/program-args* ast num-args)
        compiled-program (compile-ast (prog/program-body ast))]
    `(fn ~program-args ~compiled-program)))
