(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-monkey [line]
  (let [[name expr] (str/split line #":")
        [a b c] (read-string (str "[" expr "]"))]
    (if (number? a)
      [(keyword name) a]
      [(keyword name) (eval b) (keyword a) (keyword c)])))

(defn run [input]
  (let [{known true exprs false}
        (->> input
             (str/split-lines)
             (map read-monkey)
             (group-by #(number? (second %))))]
    (loop [known (into {} known)
           queue (into clojure.lang.PersistentQueue/EMPTY exprs)]
      (if (empty? queue)
        known
        (let [[name op a b :as monkey] (peek queue)
              a (a known)
              b (b known)]
          (if (and (number? a) (number? b))
            (recur (assoc known name (op a b)) (pop queue))   
            (recur known (conj (pop queue) monkey))))))))

(->> (slurp "day21-input.txt")
     (run)
     (:root)
     (println))
