(require '[clojure.string :as str])
(require '[clojure.set])

(defn group-badge [group]
  (let [[a b c] (map set group)]
    (first (clojure.set/intersection a b c))))

(defn priority [item]
  (let [c (int item)]
    (if (>= c (int \a))
      (- c 96)
      (- c 38))))

(->> (str/split (slurp "day3-input.txt") #"\n")
     (partition 3)
     (map group-badge)
     (map priority)
     (reduce +)
     (println))
