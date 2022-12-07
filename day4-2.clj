(require '[clojure.string :as str])
(require '[clojure.set])

(defn read-pair [line]
  (->> (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line)
       (drop 1)
       (map read-string)))

(defn overlaps? [pair]
  (let [[a b c d] pair]
    (and (<= a d) (>= b c))))

(->> (str/split (slurp "day4-input.txt") #"\n")
     (map read-pair)
     (filter overlaps?)
     (count)
     (println))
