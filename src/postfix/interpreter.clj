(ns postfix.interpreter
  (:require [postfix.util :as u]))

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
                          `[~item (peek (u/popn ~stack-name ~idx))])
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
         (let [~stack-name (u/popn ~stack-name ~num-args)]
          ~@forms)))))

(defmacro defpostfix-int-op [cmd-name fn]
  `(defpostfix-command ~cmd-name [stack# top# next#]
     (conj stack# (~fn next# top#))))


;; Define all postfix binary operations

(defpostfix-int-op add +)
(defpostfix-int-op sub -)
(defpostfix-int-op mul *)
(defpostfix-int-op div quot)
(defpostfix-int-op rem rem)

(defmacro defwrapped-cmp-operator [op-name fn]
  `(do
     (defn ~op-name [lhs# rhs#]
       (if (~(eval fn) lhs# rhs#)
         1 0))
     (defpostfix-int-op ~op-name ~op-name)))

(defwrapped-cmp-operator lt <)
(defwrapped-cmp-operator lte <=)

(defwrapped-cmp-operator gt >)
(defwrapped-cmp-operator gte >=)

(defwrapped-cmp-operator eq =)


;; All other postfix keywords

(defpostfix-command pop [stack top]
  stack)

(defpostfix-command swap [stack top next]
  (conj (conj stack top) next))

(defpostfix-command sel [stack v1 v2 v3]
  (conj stack
        (if (= v3 0)
          v1 v2)))

(defpostfix-command nget [stack index]
  (let [stack-len (count stack)]
    (cond
     (and (number? index)
          (<= 1 index stack-len))
     (let [item (nth stack (- stack-len index))]
       (if (number? item)
         (conj stack item)
         (throw (ex-info
                 (format "nget: value at index %d is not a number" index)
                 {}))))
     (not (number? index))
     (throw (ex-info (format "nget: index value '%s' is not a number"
                             index) {}))
     :else (throw (ex-info (format "nget: index %d is out of range"
                                   index) {})))))

;; Driver code for postfix interpreter

(defn get-postfix-cmd [cmd-sym]
  (let [cmd-sym (symbol (str (name cmd-sym) "-cmd"))]
    (cmd-sym (ns-publics (find-ns 'postfix.interpreter)))))

(defn postfix-do [command]
  (cond
   (or (integer? command)
       (list?    command)) (fn [stack] (conj stack command))

   (symbol? command) (get-postfix-cmd command)
   :else 'error))

(defn make-param-list [num-params]
  (vec (map #(symbol (str "a" %))
            (range num-params))))

(defmacro postfix [num-params & prog]
  (let [arg-list (make-param-list num-params)]
   `(fn ~arg-list
      (let [stack# (atom ~(vec (reverse arg-list)))]
        (doseq [head# '~prog]
          (swap! stack# (postfix-do head#)))
        (peek @stack#)))))
