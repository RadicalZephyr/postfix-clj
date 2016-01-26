(ns postfix.compiler.program
  (:import (clojure.lang Seqable IPersistentStack)
           java.io.Writer))

(def postfix-arg (partial gensym "postfix-arg"))

(defprotocol IPostfixProgram
  (args-used [prog] "Return the number of args used by this program.")
  (program-args* [prog n]
    "Return an argument vector of size n for this program.

    `n' cannot be less than the minimum number of args required.")
  (program-args [prog] "Return the minimum required argument vector for this program.")
  (program-body [prog] "Return the body of this program."))

(deftype PostfixProgram [stack depth arg-source args-used]
  Seqable
  (seq [this] (seq (concat (->> arg-source
                                (drop depth)
                                (take (- @args-used depth))
                                reverse)
                           stack)))

  IPersistentStack
  (cons [this v] (PostfixProgram. (conj stack v) depth arg-source args-used))
  (peek [this]
    (if (seq stack)
      (peek stack)
      (do
        (swap! args-used max (inc depth))
        (nth arg-source depth))))
  (pop [this]
    (if (seq stack)
      (PostfixProgram. (pop stack) depth arg-source args-used)
      (PostfixProgram. stack (inc depth) arg-source args-used)))

  IPostfixProgram
  (args-used [prog] @args-used)
  (program-args* [prog n]
    (if (>= n @args-used)
      (vec (take n arg-source))
      (throw (ex-info "Too few arguments specified for program."
                      {:program prog
                       :requested-arg-count n
                       :needed-arg-count @args-used}))))
  (program-args [prog] (program-args* prog @args-used))

  (program-body [prog]
    (if-let [top (peek stack)]
      top
      (when (> @args-used 0)
        (first arg-source)))))

(defmethod print-method PostfixProgram [program ^Writer w]
  (.write w (format "#postfix.compiler.PostfixProgram[%d,"
                    (args-used program)))
  (print-method (seq program) w)
  (.write w "]"))

(defmethod print-dup PostfixProgram [program w]
  (print-method program w))

(defn empty-program
  ([] (empty-program 0))

  ([expected-args]
   (let [args (repeatedly postfix-arg)]
     (->PostfixProgram [] 0 args (atom expected-args)))))

(defn generate-program-fn
  ([compiled-program]
   (generate-program-fn compiled-program (args-used compiled-program)))

  ([compiled-program num-args]
   (let [program-args (program-args* compiled-program num-args)
         body (program-body compiled-program)]
     `(fn ~program-args ~body))))
