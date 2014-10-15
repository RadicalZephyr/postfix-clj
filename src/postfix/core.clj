(ns postfix.core)

(def special-commands '[add div eq exec gt lt mul nget pop rem sel swap])

(defn correct-arg-count? [args expected]
  (= (count args)
     expected))

(defn popn [coll n]
  (if (= n 0)
    coll
    (recur (pop coll) (dec n))))

(defn gen-let-bindings [stack-name args]
  (if (empty? args)
    []
    (into []
          (apply concat
           (map-indexed (fn [idx item]
                          `[~item (peek (popn ~stack-name ~idx))])
                        args)))))

(defmacro defpostfix-command
  "Define a function for use as a postfix command.

  The function defined has the suffix \"-cmd\" appended to cmd-name.
  Stack name is the variable that the stack is accesible under inside
  the body of the command.

  Automatic error checking is created based on the number of
  arguments.  The remainder of the symbols found inside the parameter
  vector are bound as the names for the current members of the stack,
  in top down order.  The stack variable is the resulting stack after
  all bound values have been popped off of the current stack.

  Example: If the arg vector is [stack top next third] then the
  function will require that the stack have at least three items, and
  the top three items will be bound to top next and third respectively
  while 'stack' will only contain the contents of the stack starting
  _after_ the item third."
  [cmd-name [stack-name & args] & forms]
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
         (let [~stack-name (popn ~stack-name ~num-args)]
          ~@forms)))))

(defmacro defpostfix-int-op [cmd-name fn]
  `(defpostfix-command ~cmd-name [stack# top# next#]
     (conj stack# (~fn next# top#))))


;; Define all postfix binary operations

(defpostfix-int-op add +)
(defpostfix-int-op sub -)
(defpostfix-int-op mul *)
(defpostfix-int-op div /)


;; All other postfix keywords

(defpostfix-command pop [stack top]
  stack)

(defpostfix-command swap [stack top next]
  (conj (conj stack top) next))


;; Driver code for postfix interpreter

(defn postfix-do [command]
  (cond
   (or (integer? command)
       (list?    command)) (fn [stack] (conj stack command))

   (symbol? command) (case command
                       pop pop-cmd

                       swap swap-cmd

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
