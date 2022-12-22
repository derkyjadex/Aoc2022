(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-monkey [line]
  (let [[name expr] (str/split line #":")
        [a b c] (read-string (str "[" expr "]"))]
    (if (number? a)
      [(symbol name) a]
      [(symbol name) [b a c]])))

(defn run [env expr]
  (cond
    (number? expr)
    expr

    (symbol? expr)
    (if-let [sub (get env expr)]
      (run env sub)
      expr)

    :else
    (let [[op a b] expr
          a (run env a)
          b (run env b)]
      (if (and (number? a) (number? b))
        ((eval op) a b)
        [op a b]))))

(defn solve [[_ expr value]]
  (cond
    (number? expr)
    (solve ['= value expr])

    (symbol? expr)
    value

    :else
    (let [[op a b] expr
          num (if (number? a) a b)
          expr' (if (number? a) b a)
          value' (case (str op)
                   "+" (- value (if (number? a) a b))
                   "*" (/ value (if (number? a) a b))

                   "-" (if (number? a)
                         (- a value)
                         (+ b value))

                   "/" (if (number? a)
                         (/ a value)
                         (* b value)))]
      (solve ['= expr' value']))))

(let [env (->> (slurp "day21-input.txt")
               (str/split-lines)
               (map read-monkey)
               (into {}))]
  (->> (run (dissoc env 'humn) 'root)
       (solve)
       (println)))
