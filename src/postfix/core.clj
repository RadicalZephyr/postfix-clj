(ns postfix.core)

(def special-commands '[add div eq exec gt lt mul nget pop rem sel swap])

(defn correct-arg-count? [args expected]
  (= (count args)
     expected))

(defn postfix-do [command]
  (cond
   (integer? command) (fn [stack] (conj stack command))
   (list? command) (fn [stack] (conj stack command))
   (symbol? command) (case command
                       pop (fn [stack]
                             (if (empty? stack)
                               (throw (ex-info "pop: empty stack" {})))
                             (pop stack))
                       swap (fn [stack]
                              (if (< (count stack) 2)
                                (throw (ex-info "swap: not enough values" {})))
                              (let [top (peek stack)
                                    next (peek (pop stack))]
                                (conj (conj stack top) next))))
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
