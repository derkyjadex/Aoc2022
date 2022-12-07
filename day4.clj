(require '[clojure.string :as str])
(require '[clojure.set])

(defn read-pair [line]
  (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
       (drop 1)
       (map read-string)))

(defn fully-contains? [pair]
  (let [[a b c d] pair]
    (or (and (<= a c) (>= b d))
        (and (<= c a) (>= d b)))))

(->> (str/split (slurp "day4-input.txt") #"\n")
     (map read-pair)
     (filter fully-contains?)
     (count)
     (println))
