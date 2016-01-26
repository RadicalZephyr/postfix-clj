(ns postfix.compiler.compile)

(defn merge-meta [obj metamap]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj (merge metamap (meta obj)))
    obj))

(def compile-map {:number identity})

(defn compile-ast [parse-tree]
  (if (and (sequential? parse-tree) (seq parse-tree))
    (if-let [transform (compile-map (first parse-tree))]
      (merge-meta
       (apply transform (map (partial compile-ast compile-map)
                             (next parse-tree)))
       (meta parse-tree))
      (with-meta
        (into [(first parse-tree)]
              (map (partial compile-ast compile-map)
                   (next parse-tree)))
        (meta parse-tree)))
    parse-tree))
