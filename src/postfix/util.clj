(ns postfix.util)

(defn popn [coll n]
  (if (= n 0)
    coll
    (recur (pop coll) (dec n))))
