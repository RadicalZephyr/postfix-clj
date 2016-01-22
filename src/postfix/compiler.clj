(ns postfix.compiler)

(defmacro postfix [& args]
  `#(identity 3))
