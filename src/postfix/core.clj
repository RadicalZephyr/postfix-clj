(ns postfix.core)

(def special-commands '[add div eq exec gt lt mul nget pop rem sel swap])

(defn correct-arg-count? [args expected]
  (= (count args)
     expected))


(defn gen-let-bindings [stack-name args]
  (if (empty? args)
    []
    (into []
          (apply concat
           (map-indexed (fn [idx item]
                          `[~item (nth ~stack-name ~idx)])
                        args)))))

(defmacro defpostfix-command [cmd-name [stack-name & args] & forms]
  (let [num-args (count args)
        count-test (if (= num-args 1)
                     `(empty? ~stack-name)
                     `(< (count ~stack-name) ~num-args))
        cmd-name (name cmd-name)
        exn-msg  (str cmd-name ": " (if (= num-args 1)
                                      "empty stack"
                                      "not enough values on the stack"))
        fn-name  (symbol (str cmd-name "-cmd"))]
    `(defn ~fn-name [~stack-name]
       (if ~count-test
         (throw (ex-info ~exn-msg {})))
       (let ~(gen-let-bindings stack-name args)
        ~@forms))))

(defpostfix-command add [stack top next]
  (conj stack (+ next top)))

(defpostfix-command sub [stack top next]
  (conj stack (- next top)))


(defn postfix-do [command]
  (cond
   (or (integer? command)
       (list?    command)) (fn [stack] (conj stack command))

   (symbol? command) (case command
                       pop  (fn [stack]
                              (if (empty? stack)
                                (throw (ex-info "pop: empty stack" {})))
                              (pop stack))

                       swap (fn [stack]
                              (if (< (count stack) 2)
                                (throw (ex-info "swap: not enough values" {})))
                              (let [top (peek stack)
                                    next (peek (pop stack))]
                                (conj (conj stack top) next)))

                       sub  (fn [stack]
                              (if (< (count stack) 2)
                                (throw (ex-info "sub: not enough values")))
                              (let [top (peek stack)
                                    next (peek (pop stack))]
                                (conj stack (- next top)))))
   :else 'error))

(defmacro postfix [num-params & prog]
  `(fn [& fn-args#]
     (when (not (correct-arg-count? fn-args# ~num-params))
       (println "Incorrect number of arguments: expected" ~num-params
                ". Got " (count fn-args#)))
     (let [stack# (atom (vec (reverse fn-args#)))]
       (doseq [head# '~prog]
         (swap! stack# (postfix-do head#)))
       (peek @stack#))))
