(ns postfix.compiler)

(defmacro postfix [num-args & program]
  (let [ret (last program)]
    `(constantly ~ret)))
