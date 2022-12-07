(require '[clojure.string :as str])
(require '[clojure.set])

(defn bad-item [line]
  (let [i (/ (count line) 2)
        a (set (subs line 0 i)) 
        b (set (subs line i))]
    (first (clojure.set/intersection a b))))

(defn priority [item]
  (let [c (int item)]
    (if (>= c (int \a))
      (- c 96)
      (- c 38))))


(->> (str/split (slurp "day3-input.txt") #"\n")
     (map bad-item)
     (map priority)
     (reduce +)
     (println))
