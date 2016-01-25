(ns postfix.util)

(defn popn [coll n]
  (if (= n 0)
    coll
    (recur (pop coll) (dec n))))

(defn peekn [coll n]
  (peek (popn coll n)))
