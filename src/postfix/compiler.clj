(ns postfix.compiler)

(defmulti compile-instruction
  (fn [stack instruction]
    (cond
      (number? instruction) :number
      (symbol? instruction) :symbol
      :else :default)))

(defmethod compile-instruction :default [stack instruction]
  (throw (ex-info "Encountered unknown instruction."
                  {:stack stack :instruction instruction})))

(defmethod compile-instruction :number [stack instruction]
  (conj stack instruction))

(defn swap [stack]
  (let [n1 (peek stack)
        n2 (peek (pop stack))]
    (-> stack
        pop
        pop
        (conj n1)
        (conj n2))))

(defmethod compile-instruction :symbol [stack instruction]
  ((case instruction
     pop pop
     swap swap) stack))

(defmacro postfix [num-args & program]
  (let [compiled-program (reduce compile-instruction [] program)
        ret (last compiled-program)]
    `(constantly ~ret)))
